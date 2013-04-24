#include "stack.hpp"
#include "garbage_collection.hpp"
#include "debug.hpp"
#include <cstdio>
#include <cassert>

bool empty(stack const& sl) {
    return sl.empty();
}

ref top(stack const& sl) {
    return sl.back();
}

ref get_n(stack const& sl, std::size_t n) {
    assert(n <= sl.size() && "out of bounds");
    return sl.rbegin()[n];
}

void push(stack& sl, ref app) {
    sl.push_back(app);
}

WARN_UNUSED_RESULT
ref extract(stack& sl) {
    assert(!empty(sl) && "extracting from empty stack");
    auto app = top(sl);
    pop(sl);
    return app;
}

void pop(stack& sl) {
    assert(!empty(sl) && "popping empty stack");
    sl.pop_back();
}

void pop_n(stack& sl, std::size_t n) {
    assert(n <= sl.size() && "poping too far");
    sl.resize(sl.size()-n);
}

