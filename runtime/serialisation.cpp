#include "serialisation.hpp"
#include "object.hpp"
#include "main.hpp"
#include <cstdint>
#include <cstring>
#include <fstream>
#include <istream>
#include <ostream>

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

        SerializedApplication(Application const* app, char const* space)
            : SerializedObject(app)
        {
            static_assert(sizeof(SerializedApplication) == sizeof(Application), "bad serialization implementation");
            left_addr = reinterpret_cast<char const*>(app->left) - space;
            right_addr = reinterpret_cast<char const*>(app->right) - space;
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

    void write_object(char* base, CRef obj, std::ostream& os) {
        if (auto app = try_cast<Application>(obj)) {
            SerializedApplication s(app, base);
            os.write(reinterpret_cast<char const*>(&s), sizeof(SerializedApplication));
        } else if (auto num = try_cast<Number>(obj)) {
            SerializedNumber s(num);
            os.write(reinterpret_cast<char const*>(&s), sizeof(SerializedNumber));
        } else if (auto func = try_cast<Function>(obj)) {
            SerializedFunction s(func);
            os.write(reinterpret_cast<char const*>(&s), sizeof(SerializedFunction));
        } else {
            assert(!"invalid object");
        }
    }

    void read_object(char* base, char* ptr, std::istream& is) {
        is.read(ptr, sizeof(Object));
        auto obj = reinterpret_cast<Ref>(ptr);
        is.read(ptr + sizeof(Object), obj->size - sizeof(Object));

        obj->forward = obj;

        if (auto s_app = try_cast<SerializedApplication>(obj)) {
            auto app = cast<Application>(obj);
            app->left = reinterpret_cast<Ref>(base + s_app->left_addr);
            app->right = reinterpret_cast<Ref>(base + s_app->right_addr);
        } else if (try_cast<SerializedNumber>(obj)) {
            // no special action necessary
        } else if (auto s_func = try_cast<SerializedFunction>(obj)) {
            char name[sizeof(Ref) + 1];
            name[sizeof(Ref)] = '\0';
            std::strncpy(name, s_func->name, sizeof(Ref));
            auto func = cast<Function>(obj);
            func->func = funcs_by_name[name];
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

void write_init_file(MemoryInfo memory) {
    std::ofstream hic("out.hic", std::ios::binary);

    hic.write(hisp_tag, 4);
    write_uint32(hic, 0);

    std::uint64_t root_addr = reinterpret_cast<char const*>(memory.root) - memory.space;
    write_uint64(hic, root_addr);

    std::uint64_t heap_size = memory.size;
    write_uint64(hic, heap_size);

    auto const memory_end = memory.space + memory.size;
    char* ptr = memory.space;
    while (ptr != memory_end) {
        auto obj = reinterpret_cast<CRef>(ptr);
        write_object(memory.space, obj, hic);
        ptr += obj->size;
    }
}

MemoryInfo read_init_file() {
    std::ifstream hic("out.hic", std::ios::binary);

    char tag[4] = {};
    hic.read(tag, sizeof(tag));
    assert(std::memcmp(tag, hisp_tag, sizeof(tag)) == 0 && "invalid file");
    hic.ignore(4);

    auto root_offset = read_uint64(hic);
    auto heap_size = read_uint64(hic);

    // This should be refactored to call into the garbage collection module.
    char* space = static_cast<char*>(calloc(2*heap_size, 1));
    auto root = reinterpret_cast<Ref>(space + root_offset);

    auto const memory_end = space + heap_size;
    char* ptr = space;
    while (ptr != memory_end) {
        read_object(space, ptr, hic);
        auto obj = reinterpret_cast<Ref>(ptr);
        ptr += obj->size;
    }

    // Gah.  We actually also need to pass the real heap size; we'll assume
    // these two parts will work together for now, but this is really ugly.
    return {root, space, heap_size};
}
