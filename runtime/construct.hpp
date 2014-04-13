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
    static void run(stack_ref& s, int n) {
        s.push(make_number(n));
    }
};

template<>
struct do_construct<func_t> {
    static void run(stack_ref& s, func_t f) {
        s.push(make_function(f));
    }
};

template<>
struct do_construct<ref> {
    static void run(stack_ref& s, ref r) {
        s.push(r);
    }
};

template<>
struct do_construct<application*> {
    static void run(stack_ref& s, application* app) {
        s.push(app);
    }
};

template<typename LHS, typename RHS>
struct do_construct<mk_app_impl<LHS, RHS>> {
    static void run(stack_ref& s, mk_app_impl<LHS, RHS> const& app) {
        do_construct<RHS>::run(s, app.rhs);
        do_construct<LHS>::run(s, app.lhs);
        auto f = s.get_nth(0);
        auto x = s.get_nth(1);
        auto res = make_application(f, x);
        s.pop_n(2);
        s.push(res);
    }
};

template<typename T>
ref construct(T const& t) {
    auto s = request_stack();
    do_construct<T>::run(s, t);
    return s.extract();
}
