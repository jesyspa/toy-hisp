#include "memory/stack.hpp"
#include <cassert>

StackStorage::StackStorage() noexcept
    : data_{{}}
    , top_{data_.begin()} {}

Stack StackStorage::get_ref() noexcept {
    return {*this, top_};
}

auto StackStorage::begin() const -> const_iterator { return data_.begin(); }

auto StackStorage::end() const -> const_iterator { return top_; }

auto StackStorage::begin() -> iterator { return data_.begin(); }

auto StackStorage::end() -> iterator { return top_; }

Stack::Stack(StackStorage& stack, iterator base) noexcept
    : ref_{&stack}
    , base_{base} {}

std::size_t Stack::size() const noexcept { return ref_->top_ - base_; }

bool Stack::empty() const noexcept { return ref_->top_ == base_; }

bool Stack::singleton() const noexcept { return size() == 1; }

Ref Stack::top() const {
    assert(!empty() && "inspecting top of empty stack");
    return ref_->top_[-1];
}

void Stack::push(Ref app) {
    assert(ref_->top_ != ref_->data_.end() && "out of bounds");
    *ref_->top_++ = app;
}

Ref Stack::extract() {
    assert(!empty() && "extracting from empty stack");
    return *--ref_->top_;
}

void Stack::pop() {
    assert(!empty() && "popping empty stack");
    --ref_->top_;
}

void Stack::pop_n(std::size_t n) {
    assert(n <= size() && "poping too far");
    ref_->top_ -= n;
}

void Stack::roll(std::size_t n) {
    assert(n <= size() && "rolling too far");
    auto p = top();
    std::copy_backward(ref_->top_ - n - 1, ref_->top_ - 1, ref_->top_);
    ref_->top_[-n - 1] = p;
}

void Stack::flip() {
    assert(size() >= 2 && "flipping almost-empty stack");
    std::swap(ref_->top_[-1], ref_->top_[-2]);
}
