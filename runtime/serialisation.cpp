#include "serialisation.hpp"
#include "object.hpp"
#include "main.hpp"
#include <cstdint>
#include <fstream>

std::map<Func, char const*> func_names = {
#define ENTRY(name) {name, #name }
    ENTRY(comb_i),
    ENTRY(comb_k),
    ENTRY(comb_s),
    ENTRY(comb_l),
    ENTRY(comb_r),
    ENTRY(comb_y),
    ENTRY(print),
    ENTRY(add),
    ENTRY(sub),
    ENTRY(le)
#undef ENTRY
};

namespace {
    static_assert(sizeof(Ref) == sizeof(std::uint64_t), "pointer storage mismatch");
    static_assert(sizeof(CRef) == sizeof(Ref), "help please no");
    static_assert(sizeof(std::uint64_t) == 8, "unexpected size of uint64_t");
    static_assert(sizeof(std::uint32_t) == 4, "unexpected size of uint32_t");

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
        std::uint64_t value;

        SerializedNumber(Number const* num)
            : SerializedObject(num), value(num->value)
        {
            static_assert(sizeof(SerializedNumber) == sizeof(Number), "bad serialization implementation");
        }
    };

    struct SerializedFunction : SerializedObject {
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

    void write_uint64(std::ostream& os, std::uint64_t i) {
        os.write(reinterpret_cast<char const*>(&i), sizeof(std::uint64_t));
    }

    void write_uint32(std::ostream& os, std::uint32_t i) {
        os.write(reinterpret_cast<char const*>(&i), sizeof(std::uint32_t));
    }
}

void write_init_file(MemoryInfo memory) {
    std::ofstream hic("out.hic", std::ios::binary);

    auto const hisp_tag = "HISP";
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
