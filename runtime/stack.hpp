#pragma once

#include "object.hpp"
#include "macros.hpp"
#include "utility.hpp"

#include <array>

class SubStack;

// Main abstraction class for the Stack mechanism.  While evaluating, we often
// need to stop using the Stack we are currently working with and evaluate an
// expression by itself.  This class allows us to split off such a "sub-Stack"
// at will.
class Stack {
public:
    static std::size_t const STACK_SIZE = 1024;

private:
    using storage = std::array<Ref, STACK_SIZE>;

public:
    using iterator = storage::iterator;
    using const_iterator = storage::const_iterator;

private:
    storage data_;
    iterator top_;

public:
    friend class SubStack;

    Stack() : data_{{}}, top_{data_.begin()} {}

    Stack(Stack const&) = delete;
    Stack& operator=(Stack const&) = delete;

    SubStack get_ref();
    const_iterator begin() const;
    const_iterator end() const;
    iterator begin();
    iterator end();
};

// A sub-stack of the stack described above.  This tracks its own base but
// performs all other operations on the parent stack.
class SubStack {
    using iterator = Stack::iterator;
    Stack* ref_;
    iterator base_;

    SubStack(Stack& stack, iterator base) : ref_(&stack), base_(base) {}

public:
    friend class Stack;

    std::size_t size() const;
    bool empty() const;
    bool singleton() const;
    Ref top() const;

    void push(Ref r);
    WARN_UNUSED_RESULT
    Ref extract();
    template<typename T>
    WARN_UNUSED_RESULT
    T* extract_as();
    void pop();
    void pop_n(std::size_t n);
    void roll(std::size_t n);
    void flip();
};

template<typename T>
WARN_UNUSED_RESULT
T* SubStack::extract_as() {
    return cast<T>(extract());
}

