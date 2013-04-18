#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"

safe_ref<object> comb_i(stack& sl) {
    return extract(sl)->right;
}

safe_ref<object> comb_k(stack& sl) {
    auto arg = extract(sl);
    pop(sl);
    return arg->right;
}

safe_ref<object> comb_s(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    return make_application(
        make_application(f->right, x->right),
        make_application(g->right, x->right));
}

safe_ref<object> comb_l(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    return make_application(
        make_application(f->right, x->right),
        g->right);
}

safe_ref<object> comb_r(stack& sl) {
    auto f = extract(sl);
    auto g = extract(sl);
    auto x = extract(sl);
    return make_application(
        f->right,
        make_application(g->right, x->right));
}

safe_ref<object> comb_y(stack& sl) {
    auto f = extract(sl);
    return make_application(f->right, make_application(make_function(comb_y), f->right));
}

safe_ref<object> print(stack& sl) {
    auto arg = extract(sl);
    auto val = eval(arg->right);
    std::printf("%d\n", cast<number>(val)->value);
    return val;
}

safe_ref<object> add(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    return make_number(
        eval_as<number>(lhs->right)->value
        + eval_as<number>(rhs->right)->value);
}

safe_ref<object> sub(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    return make_number(
        eval_as<number>(lhs->right)->value
        - eval_as<number>(rhs->right)->value);
}

safe_ref<object> le(stack& sl) {
    auto lhs = extract(sl);
    auto rhs = extract(sl);
    auto lhs_val = eval_as<number>(lhs->right);
    auto rhs_val = eval_as<number>(rhs->right);
    return make_bool(lhs_val->value <= rhs_val->value);
}

