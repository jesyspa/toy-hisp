#pragma once

#include "object.hpp"
#include "macros.hpp"
#include "utility.hpp"

#include <array>

class SubStack;

// Stores pointers into the heap that are still useful.  Built-in functions operate on this stack
// instead of using the program stack as it is easier for the garbage collector to scan this.
//
// As we often need to suspend a computation and perform a different one, both using a stack, we
// avoid extra allocations by having them share a stack.  The stack is not modified directly;
// instead, a sub-stack is requested which remembers its base and provides an interface for updating
// the whole stack.
class Stack {
    static std::size_t const STACK_SIZE = 1024;
    using storage = std::array<Ref, STACK_SIZE>;

public:
    using iterator = storage::iterator;
    using const_iterator = storage::const_iterator;

private:
    storage data_;
    iterator top_;

public:
    friend class SubStack;

    Stack();

    Stack(Stack const&) = delete;
    Stack& operator=(Stack const&) = delete;

    SubStack get_ref();
    const_iterator begin() const;
    const_iterator end() const;
    iterator begin();
    iterator end();
};

// Provides an interface to modify a Stack.  So called because we only expect a SubStack to be
// responsible for some (top) part of the stack; anything below the point where it was created is
// not of interest to it.
class SubStack {
    using iterator = Stack::iterator;
    Stack* ref_;
    iterator base_;

    SubStack(Stack& stack, iterator base);

public:
    friend class Stack;

    std::size_t size() const;
    bool empty() const;
    bool singleton() const;
    Ref top() const;

    void push(Ref obj);
    WARN_UNUSED_RESULT
    Ref extract();
    template<typename T>
    WARN_UNUSED_RESULT
    T* extract_as();
    template<typename T>
    WARN_UNUSED_RESULT
    T* try_extract_as(); // leave unchanged on fail
    void pop();
    void pop_n(std::size_t n);
    // move the top element to position n, moving all intermediate elements up.
    void roll(std::size_t n);
    void flip();
};

template<typename T>
WARN_UNUSED_RESULT
T* SubStack::extract_as() {
    return cast<T>(extract());
}

template<typename T>
WARN_UNUSED_RESULT
T* SubStack::try_extract_as() {
    auto p = try_cast<T>(top());
    if (p)
        pop();
    return p;
}

