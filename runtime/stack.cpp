#include "stack.hpp"
#include <cassert>

stack_ref stack::get_ref() {
    return {this, top_};
}

auto stack::base() const -> const_iterator {
    return data_.begin();
}

auto stack::top() const -> const_iterator {
    return top_;
}

std::size_t stack_ref::size() const {
    return ref_->top_ - base_;
}

bool stack_ref::empty() const {
    return ref_->top_ == base_;
}

bool stack_ref::singleton() const {
    return size() == 1;
}

ref stack_ref::top() const {
    assert(!empty() && "inspecting top of empty stack");
    return ref_->top_[-1];
}

ref stack_ref::get_nth(std::size_t n) const {
    assert(n <= size() && "out of bounds");
    return ref_->top_[-n-1];
}

void stack_ref::push(ref app) {
    assert(ref_->top_ != ref_->data_.end() && "out of bounds");
    *ref_->top_++ = app;
}

WARN_UNUSED_RESULT
ref stack_ref::extract() {
    assert(!empty() && "extracting from empty stack");
    return *--ref_->top_;
}

void stack_ref::pop() {
    assert(!empty() && "popping empty stack");
    --ref_->top_;
}

void stack_ref::pop_n(std::size_t n) {
    assert(n <= size() && "poping too far");
    ref_->top_ -= n;
}

void stack_ref::roll(std::size_t n) {
    assert(n <= size() && "rolling too far");
    auto p = top();
    std::copy_backward(ref_->top_ - n - 1, ref_->top_ - 1, ref_->top_);
    ref_->top_[-n-1] = p;
}

void stack_ref::flip() {
    assert(size() >= 2 && "flipping almost-empty stack");
    std::swap(ref_->top_[-1], ref_->top_[-2]);
}

