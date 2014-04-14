#pragma once
#include "object.hpp"
#include "stack.hpp"

#include <memory>
#include <iterator>
#include <vector>

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

void make_application(SubStack stack);

void make_number(SubStack stack, int value);

void make_function(SubStack stack, Func func);

void make_bool(SubStack stack, bool value);

void eval(SubStack stack);

void comb_i(SubStack stack);
void comb_k(SubStack stack);
void comb_s(SubStack stack);
void comb_l(SubStack stack);
void comb_r(SubStack stack);
void comb_y(SubStack stack);
void print(SubStack stack);
void add(SubStack stack);
void sub(SubStack stack);
void once(SubStack stack);
void le(SubStack stack);

