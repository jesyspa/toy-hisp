#pragma once

#include "main.hpp"
#include "stack.hpp"
#include "garbage_collection.hpp"

// Note: all this is preemptively deprecated in favour of loading a memory dump.  The machinery for
// that still has to be written, so we're stuck with this for now, but we'd rather get rid of it if
// possible.

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
    static void run(SubStack stack, int n) {
        make_number(stack, n);
    }
};

template<>
struct ConstructImpl<Func> {
    static void run(SubStack stack, Func f) {
        make_function(stack, f);
    }
};

template<>
struct ConstructImpl<Ref> {
    static void run(SubStack stack, Ref r) {
        stack.push(r);
    }
};

template<>
struct ConstructImpl<Application*> {
    static void run(SubStack stack, Application* app) {
        stack.push(app);
    }
};

template<typename LHS, typename RHS>
struct ConstructImpl<AppMaker<LHS, RHS>> {
    static void run(SubStack& stack, AppMaker<LHS, RHS> const& app) {
        ConstructImpl<LHS>::run(stack, app.lhs);
        ConstructImpl<RHS>::run(stack, app.rhs);
        make_application(stack);
    }
};

template<typename T>
SubStack construct(T const& t) {
    auto stack = request_stack();
    ConstructImpl<T>::run(stack, t);
    return stack;
}
