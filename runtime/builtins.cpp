#include "builtins.hpp"
#include "eval.hpp"
#include "stack.hpp"
#include "utility.hpp"
#include <cstdio>

// Implementations of Hisp built-in functions.  All expect their arguments to be passed on the
// stack, and will place their return value on it, too.
//
// We should probably either use the program stack for this or some stack-based language that we can
// then interpret (or compile?).  The latter would make it easier to eventually compile to
// supercombinators, too.

void comb_i(SubStack stack) {
    auto arg = stack.extract_as<Application>();
    stack.push(arg->right);
}

void comb_k(SubStack stack) {
    auto arg = stack.extract_as<Application>();
    stack.pop();
    stack.push(arg->right);
}

void comb_s(SubStack stack) {
    auto f = stack.extract_as<Application>();
    auto g = stack.extract_as<Application>();
    auto x = stack.extract_as<Application>();
    stack.push(g->right);
    stack.push(x->right);
    stack.push(f->right);
    stack.push(x->right);
    make_application(stack);
    stack.roll(2);
    make_application(stack);
    make_application(stack);
}

void comb_l(SubStack stack) {
    auto f = stack.extract_as<Application>();
    auto g = stack.extract_as<Application>();
    auto x = stack.extract_as<Application>();
    stack.push(g->right);
    stack.push(f->right);
    stack.push(x->right);
    make_application(stack);
    stack.flip();
    make_application(stack);
}

void comb_r(SubStack stack) {
    auto f = stack.extract_as<Application>();
    auto g = stack.extract_as<Application>();
    auto x = stack.extract_as<Application>();
    stack.push(f->right);
    stack.push(g->right);
    stack.push(x->right);
    make_application(stack);
    make_application(stack);
}

void comb_y(SubStack stack) {
    auto f = stack.extract_as<Application>();
    stack.push(f->right);
    stack.push(f);
    make_application(stack);
}

void print(SubStack stack) {
    auto arg = stack.extract_as<Application>();
    auto child_s = request_stack();
    child_s.push(arg->right);
    eval(child_s);
    auto num = cast<Number>(child_s.top());
    std::printf("%d\n", num->value);
}

void add(SubStack stack) {
    auto lhs = stack.extract_as<Application>();
    auto rhs = stack.extract_as<Application>();
    stack.push(lhs->right);
    auto child_s = request_stack();
    stack.push(rhs->right);
    eval(child_s);
    stack.flip();
    eval(child_s);
    auto rhs_num = stack.extract_as<Number>()->value;
    auto lhs_num = stack.extract_as<Number>()->value;
    make_number(stack, lhs_num + rhs_num);
}

void sub(SubStack stack) {
    auto lhs = stack.extract_as<Application>();
    auto rhs = stack.extract_as<Application>();
    stack.push(lhs->right);
    auto child_s = request_stack();
    stack.push(rhs->right);
    eval(child_s);
    stack.flip();
    eval(child_s);
    auto rhs_num = stack.extract_as<Number>()->value;
    auto lhs_num = stack.extract_as<Number>()->value;
    make_number(stack, lhs_num - rhs_num);
}

void le(SubStack stack) {
    auto lhs = stack.extract_as<Application>();
    auto rhs = stack.extract_as<Application>();
    stack.push(lhs->right);
    auto child_s = request_stack();
    stack.push(rhs->right);
    eval(child_s);
    stack.flip();
    eval(child_s);
    auto rhs_num = stack.extract_as<Number>()->value;
    auto lhs_num = stack.extract_as<Number>()->value;
    make_bool(stack, lhs_num <= rhs_num);
}

