#include "serialisation.hpp"
#include "object.hpp"
#include "builtins.hpp"
#include "utility.hpp"
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>

std::map<Func, char const*> func_names = {
#define ENTRY(name) {name, #name }
#include "funcs.inc"
#undef ENTRY
};

std::map<std::string, Func> funcs_by_name = {
#define ENTRY(name) { #name , name }
#include "funcs.inc"
#undef ENTRY
};

namespace {
    static_assert(sizeof(Ref) == sizeof(std::uint64_t), "pointer storage mismatch");
    static_assert(sizeof(CRef) == sizeof(Ref), "help please no");
    static_assert(sizeof(std::uint64_t) == 8, "unexpected size of uint64_t");
    static_assert(sizeof(std::uint32_t) == 4, "unexpected size of uint32_t");

    auto const hisp_tag = "HISP";

    struct SerializedObject {
        ObjectType type;
        std::uint32_t size;
        char forward_dummy[sizeof(Ref)];

        SerializedObject(CRef obj)
            : type{obj->type}, size{obj->size}, forward_dummy{}
        {
            static_assert(sizeof(SerializedObject) == sizeof(Object), "bad serialization implementation");
        }
    };

    struct SerializedApplication : SerializedObject {
        static constexpr ObjectType TYPE = ObjectType::application_object;
        std::uint64_t left_addr, right_addr;

        SerializedApplication(Application const* app, Space const& space)
            : SerializedObject(app)
            , left_addr(space.to_offset(app->left))
            , right_addr(space.to_offset(app->right))
        {
            static_assert(sizeof(SerializedApplication) == sizeof(Application), "bad serialization implementation");
        }
    };

    struct SerializedNumber : SerializedObject {
        static constexpr ObjectType TYPE = ObjectType::number_object;
        std::uint64_t value;

        SerializedNumber(Number const* num)
            : SerializedObject(num), value(num->value)
        {
            static_assert(sizeof(SerializedNumber) == sizeof(Number), "bad serialization implementation");
        }
    };

    struct SerializedFunction : SerializedObject {
        static constexpr ObjectType TYPE = ObjectType::function_object;
        char name[sizeof(Ref)];

        SerializedFunction(Function const* func)
            : SerializedObject(func)
        {
            static_assert(sizeof(SerializedFunction) == sizeof(Function), "bad serialization implementation");
            auto const func_name = func_names[func->func];
            std::strncpy(name, func_name, sizeof(Ref));
        }
    };

    struct SerializedForwarder : SerializedObject {
        static constexpr ObjectType TYPE = ObjectType::forwarder_object;
        std::uint64_t target_addr;

        SerializedForwarder(Forwarder const* fwd, Space const& space)
            : SerializedObject(fwd)
            , target_addr(space.to_offset(fwd->target))
        {
            static_assert(sizeof(SerializedForwarder) == sizeof(Forwarder), "bad serialization implementation");
        }
    };

    void write_object(Space const& space, Object const& obj, std::ostream& os) {
        if (auto app = try_cast<Application>(obj)) {
            SerializedApplication s(app, space);
            os.write(reinterpret_cast<char const*>(&s), s.size);
        } else if (auto num = try_cast<Number>(obj)) {
            SerializedNumber s(num);
            os.write(reinterpret_cast<char const*>(&s), s.size);
        } else if (auto func = try_cast<Function>(obj)) {
            SerializedFunction s(func);
            os.write(reinterpret_cast<char const*>(&s), s.size);
        } else if (auto fwd = try_cast<Forwarder>(obj)) {
            SerializedForwarder s(fwd, space);
            os.write(reinterpret_cast<char const*>(&s), s.size);
        } else {
            assert(!"invalid object");
        }
    }

    void read_object(Space& space, std::istream& is) {
        auto obj = space.allocate(sizeof(Object));
        auto ptr = reinterpret_cast<char*>(obj);
        is.read(reinterpret_cast<char*>(obj), sizeof(Object));
        space.extend(obj, obj->size);
        obj->forward = obj;
        is.read(ptr + sizeof(Object), obj->size - sizeof(Object));

        if (auto s_app = try_cast<SerializedApplication>(obj)) {
            auto app = cast<Application>(obj);
            app->left = space.from_offset(s_app->left_addr);
            app->right = space.from_offset(s_app->right_addr);
        } else if (try_cast<SerializedNumber>(obj)) {
            // no special action necessary
        } else if (auto s_func = try_cast<SerializedFunction>(obj)) {
            char name[sizeof(Ref) + 1];
            name[sizeof(Ref)] = '\0';
            std::strncpy(name, s_func->name, sizeof(Ref));
            auto func = cast<Function>(obj);
            func->func = funcs_by_name[name];
        } else if (auto s_fwd = try_cast<SerializedForwarder>(obj)) {
            auto fwd = cast<Forwarder>(obj);
            fwd->target = space.from_offset(s_fwd->target_addr);
        } else {
            assert(!"invalid object");
        }
    }
    void write_uint64(std::ostream& os, std::uint64_t i) {
        os.write(reinterpret_cast<char const*>(&i), sizeof(std::uint64_t));
    }

    void write_uint32(std::ostream& os, std::uint32_t i) {
        os.write(reinterpret_cast<char const*>(&i), sizeof(std::uint32_t));
    }

    std::uint64_t read_uint64(std::istream& is) {
        std::uint64_t i;
        is.read(reinterpret_cast<char*>(&i), sizeof(std::uint64_t));
        return i;
    }
}

void write_init_file(CRef root, Space const& space) {
    std::ofstream hic("out.hic", std::ios::binary);

    hic.write(hisp_tag, 4);
    write_uint32(hic, 0);
    write_uint64(hic, 0);

    auto root_addr = space.to_offset(root);
    write_uint64(hic, root_addr);

    auto heap_size = space.bytes_allocated();
    write_uint64(hic, heap_size);

    for (auto& obj : space)
        write_object(space, obj, hic);
}

MemoryInfo read_init_file() {
    std::ifstream hic("out.hic", std::ios::binary);

    char tag[4] = {};
    hic.read(tag, sizeof(tag));
    assert(std::memcmp(tag, hisp_tag, sizeof(tag)) == 0 && "invalid file");
    hic.ignore(12);

    auto root_offset = read_uint64(hic);
    auto heap_size = read_uint64(hic);

    // This should be refactored to call into the garbage collection module.
    Space space;
    space.init_space(2*heap_size);
    auto root = space.from_offset(root_offset);

    while (space.bytes_allocated() != heap_size)
        read_object(space, hic);

    return {root, std::move(space)};
}
