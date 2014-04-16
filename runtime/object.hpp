#pragma once

#include <cstddef>
#include <cstdint>

enum class ObjectType {
    application_object,
    number_object,
    function_object
};

struct Object {
    ObjectType type;
    std::uint32_t size;
    Object* forward;

    Object(Object const&) = delete;
    Object& operator=(Object const&) = delete;
};

using Ref = Object*;
using CRef = Object const*;

class SubStack;
using Func = void (*)(SubStack);

struct Application : Object {
    Ref left, right;
    static constexpr ObjectType TYPE = ObjectType::application_object;
};

struct Number : Object {
    int value;
    static constexpr ObjectType TYPE = ObjectType::number_object;
};

struct Function : Object {
    Func func;
    static constexpr ObjectType TYPE = ObjectType::function_object;
};
