#include "stack.hpp"
#include <cstdio>
#include <cassert>

void push(stack& sl, application* app) {
    sl = make_stack_link(sl, app);
}

WARN_UNUSED_RESULT
safe_ref<application> extract(stack& sl) {
    assert(sl && "extracting from empty stack");
    auto app = sl->arg;
    sl = sl->prev;
    return app;
}

void pop(stack& sl) {
    assert(sl && "popping empty stack");
    sl = sl->prev;
}

