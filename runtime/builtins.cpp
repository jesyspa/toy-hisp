#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "construct.hpp"

application* extract_app(stack& sl) {
    return cast<application>(save(extract(sl)));
}

application* get_n_app(stack& sl, int n) {
    return cast<application>(get_n(sl, n));
}

ref comb_i(stack& sl) {
    return extract_app(sl)->right;
}

ref comb_k(stack& sl) {
    auto arg = extract_app(sl);
    pop(sl);
    return arg->right;
}

ref comb_s(stack& sl) {
    auto f = get_n_app(sl, 0);
    auto g = get_n_app(sl, 1);
    auto x = get_n_app(sl, 2);
    ref r = mk_app(
            mk_app(f->right, x->right),
            mk_app(g->right, x->right));
    pop_n(sl, 3);
    return r;
}

ref comb_l(stack& sl) {
    auto f = get_n_app(sl, 0);
    auto g = get_n_app(sl, 1);
    auto x = get_n_app(sl, 2);
    ref r = mk_app(
            mk_app(f->right, x->right),
            g->right);
    pop_n(sl, 3);
    return r;
}

ref comb_r(stack& sl) {
    auto f = get_n_app(sl, 0);
    auto g = get_n_app(sl, 1);
    auto x = get_n_app(sl, 2);
    ref r = mk_app(
        f->right,
        mk_app(g->right, x->right));
    pop_n(sl, 3);
    return r;
}

ref comb_y(stack& sl) {
    auto f = get_n_app(sl, 0);
    ref r = mk_app(f->right, mk_app(f->left, f->right));
    pop(sl);
    return r;
}

ref print(stack& sl) {
    auto arg = get_n_app(sl, 0);
    ref val = eval(arg->right);
    std::printf("%d\n", cast<number>(val)->value);
    pop(sl);
    return val;
}

ref add(stack& sl) {
    auto lhs = get_n_app(sl, 0);
    auto rhs = get_n_app(sl, 1);
    ref r = make_number(
        eval_as<number>(lhs->right)->value
        + eval_as<number>(rhs->right)->value);
    pop_n(sl, 2);
    return r;
}

ref sub(stack& sl) {
    auto lhs = get_n_app(sl, 0);
    auto rhs = get_n_app(sl, 1);
    ref r = make_number(
        eval_as<number>(lhs->right)->value
        - eval_as<number>(rhs->right)->value);
    pop_n(sl, 2);
    return r;
}

ref le(stack& sl) {
    auto lhs = get_n_app(sl, 0);
    auto rhs = get_n_app(sl, 1);
    ref r = make_bool(
        eval_as<number>(lhs->right)->value
        <= eval_as<number>(rhs->right)->value);
    pop_n(sl, 2);
    return r;
}

