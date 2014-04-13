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

using func_t = void (*)(stack_ref);

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

void make_application(stack_ref s);

void make_number(stack_ref s, int value);

void make_function(stack_ref s, func_t func);

void make_bool(stack_ref s, bool b);

void collect_garbage();

void eval(stack_ref s);

void comb_i(stack_ref s);
void comb_k(stack_ref s);
void comb_s(stack_ref s);
void comb_l(stack_ref s);
void comb_r(stack_ref s);
void comb_y(stack_ref s);
void print(stack_ref s);
void add(stack_ref s);
void sub(stack_ref s);
void once(stack_ref s);
void le(stack_ref s);

