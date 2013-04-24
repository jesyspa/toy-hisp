#pragma once

#include "main.hpp"
#include "stack.hpp"
#include "garbage_collection.hpp"

template<typename T>
ref construct(T const&);

template<typename LHS, typename RHS>
struct mk_app_impl {
    LHS lhs;
    RHS rhs;

    operator ref() const {
        return construct(*this);
    }
};

template<typename LHS, typename RHS>
mk_app_impl<LHS, RHS> mk_app(LHS lhs, RHS rhs) {
    return {lhs, rhs};
}

template<typename>
struct do_construct;

template<>
struct do_construct<int> {
    static void run(stack& s, int n) {
        push(s,make_number(n));
    }
};

template<>
struct do_construct<func_t> {
    static void run(stack& s, func_t f) {
        push(s,make_function(f));
    }
};

template<>
struct do_construct<ref> {
    static void run(stack& s, ref r) {
        push(s, r);
    }
};

template<>
struct do_construct<application*> {
    static void run(stack& s, application* app) {
        push(s, app);
    }
};

template<typename LHS, typename RHS>
struct do_construct<mk_app_impl<LHS, RHS>> {
    static void run(stack& s, mk_app_impl<LHS, RHS> const& app) {
        do_construct<RHS>::run(s, app.rhs);
        do_construct<LHS>::run(s, app.lhs);
        auto f = get_n(s, 0);
        auto x = get_n(s, 1);
        auto res = make_application(f, x);
        pop_n(s, 2);
        push(s, res);
    }
};

template<typename T>
ref construct(T const& t) {
    stack s;
    register_stack(s);
    do_construct<T>::run(s, t);
    unregister_stack();
    return extract(s);
}
