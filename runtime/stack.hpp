#pragma once

#include "object.hpp"
#include "macros.hpp"
#include "utility.hpp"

#include <array>

class stack_ref;

// Main abstraction class for the stack mechanism.  While evaluating, we often
// need to stop using the stack we are currently working with and evaluate an
// expression by itself.  This class allows us to split off such a "sub-stack"
// at will.
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
    iterator base();
    iterator top();
};

// A sub-stack of the stack described above.  This tracks its own base but
// performs all other operations on the parent stack.
class stack_ref {
    using iterator = stack::iterator;
    stack* ref_;
    iterator base_;

    stack_ref(stack* ref, iterator base) : ref_(ref), base_(base) {}

public:
    friend class stack;

    std::size_t size() const;
    bool empty() const;
    bool singleton() const;
    ref top() const;
    ref get_nth(std::size_t n) const;

    void push(ref r);
    WARN_UNUSED_RESULT
    ref extract();
    template<typename T>
    WARN_UNUSED_RESULT
    T* extract_as();
    template<typename T>
    WARN_UNUSED_RESULT
    T* try_extract_as();
    void pop();
    void pop_n(std::size_t n);
    void roll(std::size_t n);
    void flip();
};

template<typename T>
WARN_UNUSED_RESULT
T* stack_ref::extract_as() {
    return cast<T>(extract());
}

template<typename T>
WARN_UNUSED_RESULT
T* stack_ref::try_extract_as() {
    return try_cast<T>(extract());
}

