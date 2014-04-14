#pragma once

#include "main.hpp"
#include "stack.hpp"
#include "garbage_collection.hpp"

template<typename T>
SubStack construct(T const&);

template<typename LHS, typename RHS>
struct AppMaker {
    LHS lhs;
    RHS rhs;
};

template<typename LHS, typename RHS>
AppMaker<LHS, RHS> mk_app(LHS lhs, RHS rhs) {
    return {lhs, rhs};
}

template<typename>
struct ConstructImpl;

template<>
struct ConstructImpl<int> {
    static void run(SubStack s, int n) {
        make_number(s, n);
    }
};

template<>
struct ConstructImpl<Func> {
    static void run(SubStack s, Func f) {
        make_function(s, f);
    }
};

template<>
struct ConstructImpl<Ref> {
    static void run(SubStack s, Ref r) {
        s.push(r);
    }
};

template<>
struct ConstructImpl<Application*> {
    static void run(SubStack s, Application* app) {
        s.push(app);
    }
};

template<typename LHS, typename RHS>
struct ConstructImpl<AppMaker<LHS, RHS>> {
    static void run(SubStack& s, AppMaker<LHS, RHS> const& app) {
        ConstructImpl<LHS>::run(s, app.lhs);
        ConstructImpl<RHS>::run(s, app.rhs);
        make_application(s);
    }
};

template<typename T>
SubStack construct(T const& t) {
    auto s = request_stack();
    ConstructImpl<T>::run(s, t);
    return s;
}
