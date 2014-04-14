#include "stack.hpp"
#include <cassert>

Stack::Stack()
    : data_{{}}
    , top_{data_.begin()}
{}

SubStack Stack::get_ref() {
    return {*this, top_};
}

auto Stack::begin() const -> const_iterator {
    return data_.begin();
}

auto Stack::end() const -> const_iterator {
    return top_;
}

auto Stack::begin() -> iterator {
    return data_.begin();
}

auto Stack::end() -> iterator {
    return top_;
}

SubStack::SubStack(Stack& stack, iterator base)
    : ref_{&stack}
    , base_{base}
{}

std::size_t SubStack::size() const {
    return ref_->top_ - base_;
}

bool SubStack::empty() const {
    return ref_->top_ == base_;
}

bool SubStack::singleton() const {
    return size() == 1;
}

Ref SubStack::top() const {
    assert(!empty() && "inspecting top of empty stack");
    return ref_->top_[-1];
}

void SubStack::push(Ref app) {
    assert(ref_->top_ != ref_->data_.end() && "out of bounds");
    *ref_->top_++ = app;
}

WARN_UNUSED_RESULT
Ref SubStack::extract() {
    assert(!empty() && "extracting from empty stack");
    return *--ref_->top_;
}

void SubStack::pop() {
    assert(!empty() && "popping empty stack");
    --ref_->top_;
}

void SubStack::pop_n(std::size_t n) {
    assert(n <= size() && "poping too far");
    ref_->top_ -= n;
}

void SubStack::roll(std::size_t n) {
    assert(n <= size() && "rolling too far");
    auto p = top();
    std::copy_backward(ref_->top_ - n - 1, ref_->top_ - 1, ref_->top_);
    ref_->top_[-n-1] = p;
}

void SubStack::flip() {
    assert(size() >= 2 && "flipping almost-empty stack");
    std::swap(ref_->top_[-1], ref_->top_[-2]);
}

