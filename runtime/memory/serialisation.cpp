#include "memory/serialisation.hpp"
#include "hisp/builtins.hpp"
#include "hisp/object.hpp"
#include "hisp/utility.hpp"
#include "meta/members.hpp"
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/sizeof.hpp>

const std::size_t func_name_length = 8;
std::map<Func, char const*> func_names = {
#define ENTRY(name)                                                                                                    \
    { name, #name }
#include "memory/funcs.inc"
#undef ENTRY
};

std::map<std::string, Func> funcs_by_name = {
#define ENTRY(name)                                                                                                    \
    { #name, name }
#include "memory/funcs.inc"
#undef ENTRY
};

namespace {
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

template <typename Type, typename Tag>
struct SerializedMemberSize {
    using type = mpl::sizeof_<Type>;
};

template <>
struct SerializedMemberSize<Ref, MEMBER_TAG(Object, forward)> {
    using type = mpl::int_<0>;
};

template <typename Tag>
struct SerializedMemberSize<Ref, Tag> {
    using type = mpl::sizeof_<std::uint64_t>;
};

template <typename Tag>
struct SerializedMemberSize<Func, Tag> {
    using type = mpl::int_<func_name_length>;
};

template <typename T>
struct GetSerializedMemberSize : SerializedMemberSize<typename T::member_type, typename T::tag>::type {};

template <typename T>
struct SerializedSize {
    using sizes = typename mpl::transform_view<RecObjectMembers<T>, GetSerializedMemberSize<_1>>::type;
    using type = typename mpl::fold<sizes, mpl::int_<0>, mpl::plus<_1, _2>>::type;
    static constexpr std::uint32_t value = type::value;
};

auto const hisp_tag = "HISP";

struct MemberSerializer {
    template <typename MemberType>
    static void print(MemberType const& member, Space const&, std::ostream& out) {
        out.write(reinterpret_cast<char const*>(&member), sizeof(member));
    }

    static void print(CRef ptr, Space const& space, std::ostream& out) {
        std::uint64_t offset = space.to_offset(ptr);
        out.write(reinterpret_cast<char const*>(&offset), sizeof(offset));
    }

    static void print(Func fptr, Space const&, std::ostream& out) {
        char name[func_name_length];
        std::strncpy(name, func_names[fptr], sizeof(name));
        out.write(name, sizeof(name));
    }

    template <typename Member, typename Tag, typename Obj>
    static void execute(Tag, Obj const* obj, Space const& space, std::ostream& out) {
        auto member = Member::template get(obj);
        print(member, space, out);
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, size), Obj const*, Space const&, std::ostream& out) {
        std::uint32_t size = SerializedSize<Obj>::value;
        std::cerr << size << '\n';
        out.write(reinterpret_cast<char const*>(&size), sizeof(size));
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, forward), Obj const*, Space const&, std::ostream&) {
        // No need to save forwarding pointers.
    }
};

template <typename T>
struct SerializationGenerator {
    template <typename Obj>
    static void serialize(Obj const* obj, Space const& space, std::ostream& out) {
        using BaseSG = SerializationGenerator<typename ObjectTraits<T>::base>;
        BaseSG::serialize(obj, space, out);

        RuntimeMemberwiseApply<MemberSerializer, T>(obj, space, out);
    }

    static void deserialize(Space& space, std::istream& in) {
        (void)space;
        (void)in;
        // semantically: for each member, read the member
        // special cases: size, forward
    }
};

template <>
struct SerializationGenerator<void> {
    template <typename... Args>
    static void serialize(Args&&...) {}
    template <typename... Args>
    static void deserialize(Args&&...) {}
};

struct SerializedObject {
    ObjectType type;
    std::uint32_t size;
    char forward_dummy[sizeof(Ref)];

    SerializedObject(CRef obj)
        : type{obj->type}
        , size{obj->size}
        , forward_dummy{} {
        static_assert(sizeof(SerializedObject) == sizeof(Object), "bad serialization implementation");
    }
};

struct SerializedApplication : SerializedObject {
    static constexpr ObjectType TYPE = ObjectType::application_object;
    std::uint64_t left_addr, right_addr;

    SerializedApplication(Application const* app, Space const& space)
        : SerializedObject(app)
        , left_addr(space.to_offset(app->left))
        , right_addr(space.to_offset(app->right)) {
        static_assert(sizeof(SerializedApplication) == sizeof(Application), "bad serialization implementation");
    }
};

struct SerializedNumber : SerializedObject {
    static constexpr ObjectType TYPE = ObjectType::number_object;
    std::uint64_t value;

    SerializedNumber(Number const* num)
        : SerializedObject(num)
        , value(num->value) {
        static_assert(sizeof(SerializedNumber) == sizeof(Number), "bad serialization implementation");
    }
};

struct SerializedFunction : SerializedObject {
    static constexpr ObjectType TYPE = ObjectType::function_object;
    char name[sizeof(Ref)];

    SerializedFunction(Function const* func)
        : SerializedObject(func) {
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
        , target_addr(space.to_offset(fwd->target)) {
        static_assert(sizeof(SerializedForwarder) == sizeof(Forwarder), "bad serialization implementation");
    }
};

void write_object(Space const& space, Object const& obj, std::ostream& os) {
    if (auto app = try_cast<Application>(obj)) {
        SerializationGenerator<Application>::serialize(app, space, os);
    } else if (auto num = try_cast<Number>(obj)) {
        SerializationGenerator<Number>::serialize(num, space, os);
    } else if (auto func = try_cast<Function>(obj)) {
        SerializationGenerator<Function>::serialize(func, space, os);
    } else if (auto fwd = try_cast<Forwarder>(obj)) {
        SerializationGenerator<Forwarder>::serialize(fwd, space, os);
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

void write_init_file(CRef, Space const& space) {
    std::ofstream hic("out.hic", std::ios::binary);

    hic.write(hisp_tag, 4);
    write_uint32(hic, 0);
    write_uint64(hic, 0);

    std::uint64_t root_addr = 0xCDDCCDDCCDDCCDDC, heap_size = 0xABBAABBAABBAABBA;
    write_uint64(hic, root_addr);
    write_uint64(hic, heap_size);

    for (auto& obj : space)
        write_object(space, obj, hic);
}

ProgramInitInfo read_init_file(std::string name) {
    std::ifstream hic(std::move(name), std::ios::binary);
    if (!hic)
        throw std::runtime_error{"init file not found"};

    char tag[4] = {};
    hic.read(tag, sizeof(tag));
    assert(std::memcmp(tag, hisp_tag, sizeof(tag)) == 0 && "invalid file");
    hic.ignore(12);

    auto root_offset = read_uint64(hic);
    auto heap_size = read_uint64(hic);

    // This should be refactored to call into the garbage collection module.
    Space space;
    space.init_space(2 * heap_size);
    auto root = space.from_offset(root_offset);

    while (space.bytes_allocated() != heap_size)
        read_object(space, hic);

    return {root, std::move(space)};
}
