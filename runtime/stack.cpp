#include "stack.hpp"
#include "garbage_collection.hpp"
#include <cstdio>
#include <cassert>

bool empty(stack const& sl) {
    return sl.empty();
}

ref top(stack const& sl) {
    return sl.back();
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

