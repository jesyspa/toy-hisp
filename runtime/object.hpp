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
};

using Ref = Object*;
using CRef = Object const*;
