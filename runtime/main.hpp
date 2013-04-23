#pragma once
#include <memory>
#include <iterator>
#include <vector>

#ifdef __GNUC__
#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define WARN_UNUSED_RESULT
#endif

enum class object_type {
    application_object,
    number_object,
    function_object
};

struct object {
    bool used;
    object_type type;
    object* next;
};

struct application;

using ref = object*;
using stack = std::vector<ref>;
using func_t = ref (*)(stack&);

struct application : object {
    ref left, right;
    static constexpr object_type TYPE = object_type::application_object;
};

struct number : object {
    int value;
    static constexpr object_type TYPE = object_type::number_object;
};

struct function : object {
    func_t func;
    static constexpr object_type TYPE = object_type::function_object;
};

WARN_UNUSED_RESULT
application* make_application(ref left, ref right);

WARN_UNUSED_RESULT
number* make_number(int value);

WARN_UNUSED_RESULT
function* make_function(func_t func);

WARN_UNUSED_RESULT
ref make_bool(bool b);

void collect_garbage();

ref eval(ref);

ref comb_i(stack& sl);
ref comb_k(stack& sl);
ref comb_s(stack& sl);
ref comb_l(stack& sl);
ref comb_r(stack& sl);
ref comb_y(stack& sl);
ref print(stack& sl);
ref add(stack& sl);
ref sub(stack& sl);
ref le(stack& sl);

