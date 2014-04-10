#include "stack.hpp"
#include <cassert>

std::size_t stack::size() const {
    return top_ - data_.begin();
}

bool stack::empty() const {
    return data_.begin() == top_;
}

ref stack::top() const {
    return *top_;
}

ref stack::get_nth(std::size_t n) const {
    assert(n <= size() && "out of bounds");
    return top_[-n-1];
}

void stack::push(ref app) {
    assert(top_ != data_.end() && "out of bounds");
    *top_++ = app;
}

WARN_UNUSED_RESULT
ref stack::extract() {
    assert(!empty() && "extracting from empty stack");
    return *--top_;
}

void stack::pop() {
    assert(!empty() && "popping empty stack");
    --top_;
}

void stack::pop_n(std::size_t n) {
    assert(n <= size() && "poping too far");
    top_ -= n;
}


