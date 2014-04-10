#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "construct.hpp"

application* extract_app(stack& s) {
    return cast<application>(s.extract());
}

application* get_n_app(stack& s, int n) {
    return cast<application>(s.get_nth(n));
}

ref comb_i(stack& s) {
    return extract_app(s)->right;
}

ref comb_k(stack& s) {
    auto arg = extract_app(s);
    s.pop();
    return arg->right;
}

ref comb_s(stack& s) {
    auto f = get_n_app(s, 0);
    auto g = get_n_app(s, 1);
    auto x = get_n_app(s, 2);
    ref r = mk_app(
            mk_app(f->right, x->right),
            mk_app(g->right, x->right));
    s.pop_n(3);
    return r;
}

ref comb_l(stack& s) {
    auto f = get_n_app(s, 0);
    auto g = get_n_app(s, 1);
    auto x = get_n_app(s, 2);
    ref r = mk_app(
            mk_app(f->right, x->right),
            g->right);
    s.pop_n(3);
    return r;
}

ref comb_r(stack& s) {
    auto f = get_n_app(s, 0);
    auto g = get_n_app(s, 1);
    auto x = get_n_app(s, 2);
    ref r = mk_app(
        f->right,
        mk_app(g->right, x->right));
    s.pop_n(3);
    return r;
}

ref comb_y(stack& s) {
    auto f = get_n_app(s, 0);
    ref r = mk_app(f->right, mk_app(f->left, f->right));
    s.pop();
    return r;
}

ref print(stack& s) {
    auto arg = get_n_app(s, 0);
    ref val = eval(arg->right);
    std::printf("%d\n", cast<number>(val)->value);
    s.pop();
    return val;
}

ref add(stack& s) {
    auto lhs = get_n_app(s, 0);
    auto rhs = get_n_app(s, 1);
    ref r = make_number(
        eval_as<number>(lhs->right)->value
        + eval_as<number>(rhs->right)->value);
    s.pop_n(2);
    return r;
}

ref sub(stack& s) {
    auto lhs = get_n_app(s, 0);
    auto rhs = get_n_app(s, 1);
    ref r = make_number(
        eval_as<number>(lhs->right)->value
        - eval_as<number>(rhs->right)->value);
    s.pop_n(2);
    return r;
}

ref le(stack& s) {
    auto lhs = get_n_app(s, 0);
    auto rhs = get_n_app(s, 1);
    ref r = make_bool(
        eval_as<number>(lhs->right)->value
        <= eval_as<number>(rhs->right)->value);
    s.pop_n(2);
    return r;
}

