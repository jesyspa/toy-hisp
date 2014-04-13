#pragma once

#include "main.hpp"
#include "stack.hpp"
#include "garbage_collection.hpp"

template<typename T>
stack_ref construct(T const&);

template<typename LHS, typename RHS>
struct mk_app_impl {
    LHS lhs;
    RHS rhs;
};

template<typename LHS, typename RHS>
mk_app_impl<LHS, RHS> mk_app(LHS lhs, RHS rhs) {
    return {lhs, rhs};
}

template<typename>
struct do_construct;

template<>
struct do_construct<int> {
    static void run(stack_ref s, int n) {
        make_number(s, n);
    }
};

template<>
struct do_construct<func_t> {
    static void run(stack_ref s, func_t f) {
        make_function(s, f);
    }
};

template<>
struct do_construct<ref> {
    static void run(stack_ref s, ref r) {
        s.push(r);
    }
};

template<>
struct do_construct<application*> {
    static void run(stack_ref s, application* app) {
        s.push(app);
    }
};

template<typename LHS, typename RHS>
struct do_construct<mk_app_impl<LHS, RHS>> {
    static void run(stack_ref& s, mk_app_impl<LHS, RHS> const& app) {
        do_construct<LHS>::run(s, app.lhs);
        do_construct<RHS>::run(s, app.rhs);
        make_application(s);
    }
};

template<typename T>
stack_ref construct(T const& t) {
    auto s = request_stack();
    do_construct<T>::run(s, t);
    return s;
}
