#pragma once

#include <cstddef>

enum class object_type {
    application_object,
    number_object,
    function_object
};

struct object {
    object_type type;
    std::size_t size;
    object* forward;
};

using ref = object*;
