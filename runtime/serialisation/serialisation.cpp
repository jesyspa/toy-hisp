#include "serialisation/serialisation.hpp"
#include "serialisation/binary_io.hpp"
#include "serialisation/serialised_hic.hpp"
#include "hisp/builtins.hpp"
#include "hisp/object.hpp"
#include "hisp/utility.hpp"
#include "meta/members.hpp"
#include "meta/apply.hpp"
#include "meta/array_gen.hpp"
#include <cstdint>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/sizeof.hpp>
#include <boost/mpl/placeholders.hpp>

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
namespace ph = mpl::placeholders;

template <typename Type, typename Tag>
struct SerialisedMemberSize {
    using type = mpl::sizeof_<Type>;
};

template <>
struct SerialisedMemberSize<Ref, MEMBER_TAG(Object, forward)> {
    using type = mpl::int_<0>;
};

template <typename Tag>
struct SerialisedMemberSize<Ref, Tag> {
    using type = mpl::sizeof_<std::uint64_t>;
};

template <typename Tag>
struct SerialisedMemberSize<Func, Tag> {
    using type = mpl::int_<func_name_length>;
};

template <typename T>
struct GetSerialisedMemberSize : SerialisedMemberSize<typename T::member_type, typename T::tag>::type {};

template <typename T>
struct SerialisedSize {
    using sizes = typename mpl::transform_view<RecObjectMembers<T>, GetSerialisedMemberSize<ph::_1>>::type;
    using type = typename mpl::fold<sizes, mpl::int_<0>, mpl::plus<ph::_1, ph::_2>>::type;
    using value_type = std::uint32_t;
    static constexpr std::uint32_t value = type::value;
};

struct MemberSerialiser {
    template <typename MemberType>
    static void print(MemberType const& member, SerialisedHic& hic) {
        hic.write(member);
    }

    static void print(Ref ptr, SerialisedHic& hic) {
        std::uint64_t offset = hic.to_offset(ptr);
        hic.write(offset);
    }

    static void print(Func fptr, SerialisedHic& hic) {
        char name[func_name_length];
        std::strncpy(name, func_names[fptr], sizeof name);
        hic.write(name);
    }

    template <typename Member, typename Tag, typename Obj>
    static void execute(Tag, Obj* obj, SerialisedHic& hic) {
        auto member = Member::template get(obj);
        print(member, hic);
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, size), Obj*, SerialisedHic& hic) {
        std::uint32_t size = SerialisedSize<Obj>::value;
        hic.write(size);
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, forward), Obj*, SerialisedHic&) {
        // No need to save forwarding pointers.
    }
};

struct MemberDeserialiser {
    template <typename Member>
    static void read(Member& out, SerialisedHic& hic, int& size) {
        size -= sizeof(Member);
        out = hic.read_direct<Member>();
    }

    static void read(Ref& ref, SerialisedHic& hic, int& size) {
        size -= sizeof(std::uint64_t);
        auto offset = hic.read_direct<std::uint64_t>();
        ref = hic.from_offset(offset);
    }

    static void read(Func& fptr, SerialisedHic& hic, int& size) {
        size -= func_name_length;
        char name[func_name_length + 1];
        hic.read_bytes(name, func_name_length);
        name[func_name_length] = '\0';
        fptr = funcs_by_name.at(name);
    }

    template <typename Member, typename Tag, typename Obj>
    static void execute(Tag, Obj* obj, SerialisedHic& hic, int& size) {
        auto& member = Member::template get(obj);
        read(member, hic, size);
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, type), Obj*, SerialisedHic& hic, int& size) {
        // We already read this when identifying the object
        size -= sizeof(ObjectType);
        hic.ignore(sizeof(ObjectType));
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, size), Obj*, SerialisedHic& hic, int& size) {
        // We already read this when identifying the object
        size += hic.read_direct<std::uint32_t>() - sizeof(std::uint32_t);
    }

    template <typename Member, typename Obj>
    static void execute(MEMBER_TAG(Object, forward), Obj*, SerialisedHic&, int&) {
        // No need to save forwarding pointers.
    }
};

template <typename T>
struct SerialisationGenerator {
    static void execute(T* obj, SerialisedHic& hic) { RuntimeRecMemberwiseApply<MemberSerialiser, T>(obj, hic); }
};

template <typename T>
struct DeserialisationGenerator {
    static void execute(T* obj, SerialisedHic& hic) {
        int size = 0;
        RuntimeRecMemberwiseApply<MemberDeserialiser, T>(obj, hic, size);
        hic.ignore(size);
    }
};

void identify_object(Space& space, SerialisedHic& hic) {
    hic.start_object();

    auto obj = space.allocate(sizeof(Object));
    obj->type = hic.read_direct<ObjectType>();
    auto size = hic.read_direct<std::uint32_t>();

    auto& sizes = make_array<mpl::sizeof_, std::uint32_t>();
    space.extend(obj, sizes[(std::size_t)obj->type]);
    hic.ignore(size - sizeof(ObjectType) - sizeof(std::uint32_t));

    hic.end_object(obj);
}

void mark_object(Object& obj, SerialisedHic& hic) {
    hic.start_object();

    auto& sizes = make_array<SerialisedSize>();

    auto size = sizes[(std::size_t)obj.type];
    hic.write(obj.type);
    hic.write(size);
    hic.advance(size - sizeof(ObjectType) - sizeof(std::uint32_t));

    hic.end_object(&obj);
}

void write_object(Ref obj, SerialisedHic& hic) { RuntimeApply<SerialisationGenerator>(obj, hic); }

void read_object(Ref obj, SerialisedHic& hic) { RuntimeApply<DeserialisationGenerator>(obj, hic); }
}

void write_init_file(Ref root, Space& space) {
    SerialisedHic hic;

    for (auto& obj : space)
        mark_object(obj, hic);

    hic.seek_begin();

    for (auto& obj : space)
        write_object(&obj, hic);

    hic.set_root(root);

    std::ofstream os("out.hic", std::ios::binary);
    hic.write_to_stream(os);
}

ProgramInitInfo read_init_file(std::string name) {
    std::ifstream is(std::move(name), std::ios::binary);
    SerialisedHic hic;
    hic.read_from_stream(is);

    if (!is)
        throw std::runtime_error{"failed to read init file"};

    is.close();

    auto heap_size = hic.size();

    // This should be refactored to call into the garbage collection module.
    Space space;
    space.init_space(2 * heap_size);

    while (!hic.at_top())
        identify_object(space, hic);

    hic.seek_begin();

    for (auto& obj : space)
        read_object(&obj, hic);
    auto root = hic.get_root();

    return {root, std::move(space)};
}
