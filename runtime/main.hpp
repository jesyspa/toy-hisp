#pragma once
#include "object.hpp"
#include "stack.hpp"

#include <memory>
#include <iterator>
#include <vector>

#ifndef NDEBUG
#define ASSERT_SANITY(r) do { \
    assert(r && r->allocated); \
    if (auto app = try_cast<application>(r)) { \
        assert(app->left && app->left->allocated); \
        assert(app->right && app->right->allocated); \
    } \
} while(false)
#else
#define ASSERT_SANITY(r) do { (void)r; } while(false)
#endif

struct application;

using func_t = ref (*)(stack_ref&);

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

ref comb_i(stack_ref& sl);
ref comb_k(stack_ref& sl);
ref comb_s(stack_ref& sl);
ref comb_l(stack_ref& sl);
ref comb_r(stack_ref& sl);
ref comb_y(stack_ref& sl);
ref print(stack_ref& sl);
ref add(stack_ref& sl);
ref sub(stack_ref& sl);
ref once(stack_ref& sl);
ref le(stack_ref& sl);

