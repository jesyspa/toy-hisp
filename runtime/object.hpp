#pragma once

#include <cstddef>

enum class ObjectType {
    application_object,
    number_object,
    function_object
};

struct Object {
    ObjectType type;
    std::size_t size;
    Object* forward;
};

using Ref = Object*;
using CRef = Object const*;
