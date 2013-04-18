#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"

ref comb_i(stack& sl) {
    return extract(sl)->right;
}

ref comb_k(stack& sl) {
    auto arg = extract(sl)->right;
    pop(sl);
    return arg;
}

ref comb_s(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    auto result = make_application(
        make_application(f->right, x->right),
        make_application(g->right, x->right));
    return result;
}

ref comb_l(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    auto result = make_application(
        make_application(f->right, x->right),
        g->right);
    return result;
}

ref comb_r(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    auto result = make_application(
        f->right,
        make_application(g->right, x->right));
    return result;
}

ref comb_y(stack& sl) {
    auto f = extract(sl);
    return make_application(f->right, make_application(make_function(comb_y), f->right));
}

ref print(stack& sl) {
    auto arg = extract(sl);
    auto val = eval(arg->right);
    std::printf("%d\n", cast<number>(val)->value);
    return val;
}

ref add(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    auto result = make_number(
        eval_as<number>(lhs->right)->value
        + eval_as<number>(rhs->right)->value);
    return result;
}

ref sub(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    auto result = make_number(
        eval_as<number>(lhs->right)->value
        - eval_as<number>(rhs->right)->value);
    return result;
}

ref le(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    auto lhs_val = eval_as<number>(lhs->right);
    auto rhs_val = eval_as<number>(rhs->right);
    return make_bool(lhs_val->value <= rhs_val->value);
}

