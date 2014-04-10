#pragma once

enum class object_type {
    application_object,
    number_object,
    function_object
};

struct object {
    bool used;
    bool allocated;
    object_type type;
    object* next;
};

using ref = object*;
