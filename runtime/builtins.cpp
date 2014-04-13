#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "construct.hpp"

void comb_i(stack_ref s) {
    auto arg = s.extract_as<application>();
    s.push(arg->right);
}

void comb_k(stack_ref s) {
    auto arg = s.extract_as<application>();
    s.pop();
    s.push(arg->right);
}

void comb_s(stack_ref s) {
    auto f = s.extract_as<application>();
    auto g = s.extract_as<application>();
    auto x = s.extract_as<application>();
    s.push(g->right);
    s.push(x->right);
    s.push(f->right);
    s.push(x->right);
    make_application(s);
    s.roll(2);
    make_application(s);
    make_application(s);
}

void comb_l(stack_ref s) {
    auto f = s.extract_as<application>();
    auto g = s.extract_as<application>();
    auto x = s.extract_as<application>();
    s.push(g->right);
    s.push(f->right);
    s.push(x->right);
    make_application(s);
    s.flip();
    make_application(s);
}

void comb_r(stack_ref s) {
    auto f = s.extract_as<application>();
    auto g = s.extract_as<application>();
    auto x = s.extract_as<application>();
    s.push(f->right);
    s.push(g->right);
    s.push(x->right);
    make_application(s);
    make_application(s);
}

void comb_y(stack_ref s) {
    auto f = s.extract_as<application>();
    s.push(f->right);
    s.push(f);
    make_application(s);
}

void print(stack_ref s) {
    auto arg = s.extract_as<application>();
    auto child_s = request_stack();
    child_s.push(arg->right);
    eval(child_s);
    auto num = child_s.extract_as<number>();
    std::printf("%d\n", num->value);
    s.push(num);
}

void add(stack_ref s) {
    auto lhs = s.extract_as<application>();
    auto rhs = s.extract_as<application>();
    s.push(lhs->right);
    auto child_s = request_stack();
    s.push(rhs->right);
    eval(child_s);
    s.flip();
    eval(child_s);
    auto rhs_num = s.extract_as<number>()->value;
    auto lhs_num = s.extract_as<number>()->value;
    make_number(s, lhs_num + rhs_num);
}

void sub(stack_ref s) {
    auto lhs = s.extract_as<application>();
    auto rhs = s.extract_as<application>();
    s.push(lhs->right);
    auto child_s = request_stack();
    s.push(rhs->right);
    eval(child_s);
    s.flip();
    eval(child_s);
    auto rhs_num = s.extract_as<number>()->value;
    auto lhs_num = s.extract_as<number>()->value;
    make_number(s, lhs_num - rhs_num);
}

void le(stack_ref s) {
    auto lhs = s.extract_as<application>();
    auto rhs = s.extract_as<application>();
    s.push(lhs->right);
    auto child_s = request_stack();
    s.push(rhs->right);
    eval(child_s);
    s.flip();
    eval(child_s);
    auto rhs_num = s.extract_as<number>()->value;
    auto lhs_num = s.extract_as<number>()->value;
    make_bool(s, lhs_num <= rhs_num);
}

