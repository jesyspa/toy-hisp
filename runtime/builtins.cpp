#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "construct.hpp"

ref comb_i(stack& sl) {
    return cast<application>(extract(sl))->right;
}

ref comb_k(stack& sl) {
    auto arg = cast<application>(extract(sl));
    pop(sl);
    return arg->right;
}

ref comb_s(stack& sl) {
    auto f = cast<application>(extract(sl));
    auto g = cast<application>(extract(sl));
    auto x = cast<application>(extract(sl));
    return mk_app(
            mk_app(f->right, x->right),
            mk_app(g->right, x->right));
}

ref comb_l(stack& sl) {
    auto f = cast<application>(extract(sl));
    auto g = cast<application>(extract(sl));
    auto x = cast<application>(extract(sl));
    return mk_app(
            mk_app(f->right, x->right),
            g->right);
}

ref comb_r(stack& sl) {
    auto f = cast<application>(extract(sl));
    auto g = cast<application>(extract(sl));
    auto x = cast<application>(extract(sl));
    return mk_app(
        f->right,
        mk_app(g->right, x->right));
}

ref comb_y(stack& sl) {
    auto f = cast<application>(extract(sl));
    return mk_app(f->right, f);
}

ref print(stack& sl) {
    auto arg = cast<application>(extract(sl));
    auto val = eval(arg->right);
    std::printf("%d\n", cast<number>(val)->value);
    return val;
}

ref add(stack& sl) {
    auto lhs = cast<application>(extract(sl));
    auto rhs = cast<application>(extract(sl));
    return make_number(
        eval_as<number>(lhs->right)->value
        + eval_as<number>(rhs->right)->value);
}

ref sub(stack& sl) {
    auto lhs = cast<application>(extract(sl));
    auto rhs = cast<application>(extract(sl));
    return make_number(
        eval_as<number>(lhs->right)->value
        - eval_as<number>(rhs->right)->value);
}

ref le(stack& sl) {
    auto lhs = cast<application>(extract(sl));
    auto rhs = cast<application>(extract(sl));
    auto lhs_val = eval_as<number>(lhs->right);
    auto rhs_val = eval_as<number>(rhs->right);
    return make_bool(lhs_val->value <= rhs_val->value);
}

