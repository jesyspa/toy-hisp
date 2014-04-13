#pragma once

#include "object.hpp"
#include "macros.hpp"

#include <array>

class stack_ref;

class stack {
public:
    static std::size_t const STACK_SIZE = 1024;

private:
    using storage = std::array<ref, STACK_SIZE>;

public:
    using iterator = storage::iterator;
    using const_iterator = storage::const_iterator;

private:
    storage data_;
    iterator top_;

public:
    friend class stack_ref;

    stack() : data_{{}}, top_{data_.begin()} {}

    stack(stack const&) = delete;
    stack& operator=(stack const&) = delete;

    stack_ref get_ref();
    const_iterator base() const;
    const_iterator top() const;
};

class stack_ref {
    using iterator = stack::iterator;
    stack* ref_;
    iterator base_;

    stack_ref(stack* ref, iterator base) : ref_(ref), base_(base) {}

public:
    friend class stack;

    std::size_t size() const;
    bool empty() const;
    ref top() const;
    ref get_nth(std::size_t n) const;

    void push(ref r);
    WARN_UNUSED_RESULT
    ref extract();
    void pop();
    void pop_n(std::size_t n);
};
