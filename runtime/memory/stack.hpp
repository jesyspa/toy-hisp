#pragma once

#include "hisp/object.hpp"
#include "macros.hpp"
#include "hisp/utility.hpp"

#include <array>

class Stack;

/*! \brief Provides storage for stacks of pointers into the heap.
 *
 *  All Hisp built-in operations take and return their arguments on the stack.  Many of these functions need to create
 *  further stacks in order to evaluate their arguments, creating a "stack of stacks" situation.  This class provides an
 *  abstraction for the allocation of these stacks, allowing them to be partially overlapping.
 */
class StackStorage {
    // TODO: Split the storage interface from the stack-of-stacks interface.

    //! \brief Maximum total number of elements stored.
    static std::size_t const STACK_SIZE = 1024;
    //! \brief Underlying storage type.
    using storage = std::array<Ref, STACK_SIZE>;

public:
    //! \brief Iterator over individual (mutable) elements.
    using iterator = storage::iterator;
    //! \brief Iterator over individual (const) elements.
    using const_iterator = storage::const_iterator;

private:
    storage data_;
    iterator top_;

public:
    friend class Stack;

    //! \brief Construct an empty stack.
    StackStorage() noexcept;

    StackStorage(StackStorage const&) = delete;
    StackStorage& operator=(StackStorage const&) = delete;

    //! \brief Create a stack on top of the current ones.
    Stack get_ref() noexcept;

    //! \brief Get an iterator to the bottom individual element.
    iterator begin();
    //! \brief Get an iterator one past the top individual element.
    iterator end();
    //! \copydoc StackStorage::begin()
    const_iterator begin() const;
    //! \copydoc StackStorage::end()
    const_iterator end() const;
};

//! \brief Represents a single stack in the stack-of-stacks structure.
class Stack {
    using iterator = StackStorage::iterator;
    /*! \brief Pointer to the underlying storage.
     *
     *  We need this to find the current top.
     */
    StackStorage* ref_;
    //! \brief Bottom of the current stack.
    iterator base_;

    /*! \brief Construct a stack with the given underlying storage and base.
     *
     *  The base should be in the storage.
     */
    Stack(StackStorage& storage, iterator base) noexcept;

public:
    friend class StackStorage;

    //! \brief Return number of elements currently stored.
    std::size_t size() const noexcept;
    //! \brief Check whether the stack is empty.
    bool empty() const noexcept;
    //! \brief Check whether the stack contains exactly one element.
    bool singleton() const noexcept;
    //! \brief Return a reference to the top element of the stack.
    Ref top() const;

    //! \brief Add an element to the stack.
    void push(Ref obj);
    //! \brief Pop the stack and return the old top.
    Ref extract();
    //! \brief Pop the stack and return the old top, cast to a T*.
    template <typename T>
    T* extract_as();
    /*! \brief If the top is convertible to a T*, pop and return it.
     *
     *  Returns nullptr if the top is not convertible, and leaves the stack unchaged.
     */
    template <typename T>
    T* try_extract_as();
    //! \brief Remove the top element from the stack.
    void pop();
    //! \brief Remove the top n elements from the stack.
    void pop_n(std::size_t n);
    //! \brief Move the top element down n places, move intermediate elements up.
    void roll(std::size_t n);
    //! \brief Flip the top and second-to-top elements around.
    void flip();
};

template <typename T>
T* Stack::extract_as() {
    return cast<T>(extract());
}

template <typename T>
T* Stack::try_extract_as() {
    auto p = try_cast<T>(top());
    if (p)
        pop();
    return p;
}
