#pragma once

#include "object.hpp"
#include "macros.hpp"

#include <array>

class stack {
public:
    static std::size_t const STACK_SIZE = 1024;

private:
    using storage = std::array<ref, STACK_SIZE>;
    using iterator = storage::iterator;
    using const_iterator = storage::const_iterator;

    storage data_;
    iterator top_;

public:
    stack() : data_{{}}, top_{data_.begin()} {}

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

