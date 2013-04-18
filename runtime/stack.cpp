#include "stack.hpp"
#include <cassert>

void push(stack& sl, application* app) {
    stack p(new stack_link{nullptr, app});
    p->prev.swap(sl);
    p.swap(sl);
}

WARN_UNUSED_RESULT
application* extract(stack& sl) {
    assert(sl && "extracting from empty stack");
    auto app = sl->arg;
    stack local;
    local.swap(sl);
    sl.swap(local->prev);
    return app;
}

void pop(stack& sl) {
    assert(sl && "popping empty stack");
    stack local;
    local.swap(sl);
    sl.swap(local->prev);
}

