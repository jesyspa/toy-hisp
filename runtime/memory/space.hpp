#pragma once

#include "hisp/object.hpp"
#include <cstdint>
#include <iosfwd>
#include <iterator>

//! \brief Memory area that can be used to allocate Hisp objects.
class Space {
    //! \brief Pointer to the beginning of the space.
    char* bottom_;
    //! \brief Pointer to the beginning of the unallocated part.
    char* free_bottom_;
    //! \brief Pointer one past the end of the allocated part.
    char* top_;

    /*! \brief Helper to implement the common functions in iterator and const_iterator.
     *
     *  \remark We use Ref as the underlying type, as we know all pointers are really to non-const objects, and this
     *  removes the need to template over the representations.  const_iterator should ensure that the pointee is never
     *  modified.
     */
    class BaseIterator {
    protected:
        //! \brief Pointer to current object.
        Ref obj_;

        //! \brief Construct an iterator pointer to the given object.
        BaseIterator(Ref obj);

        //! \brief Construct an invalid iterator.
        BaseIterator();

        //! \brief Increment the iterator and return the new value.
        BaseIterator& operator++();

        /*! \brief Increment the iterator and return the old value.
         *
         *  \remark Returns a Ref instead of a BaseIterator; derived classes should wrap the result into a suitable
         *  iterator type.
         */
        Ref operator++(int);

        //! \brief Advance the iterator.
        void increment();

    public:
        //! \brief Check whether the given iterators refer to the same object.
        friend bool operator==(BaseIterator lhs, BaseIterator rhs) { return lhs.obj_ == rhs.obj_; }

        //! \brief Check whether the given iterators refer to different objects.
        friend bool operator!=(BaseIterator lhs, BaseIterator rhs) { return !(lhs == rhs); }

        //! \brief Check whether the left iterator's object was allocated before the right iterator's.
        friend bool operator<(BaseIterator lhs, BaseIterator rhs) { return lhs.obj_ < rhs.obj_; }

        //! \brief Check whether the left iterator's object was allocated after the right iterator's.
        friend bool operator>(BaseIterator lhs, BaseIterator rhs) { return rhs < lhs; }

        //! \brief Check whether the left iterator's object was allocated no later than the right iterator's.
        friend bool operator<=(BaseIterator lhs, BaseIterator rhs) { return !(rhs < lhs); }

        //! \brief Check whether the left iterator's object was allocated no earlier than the right iterator's.
        friend bool operator>=(BaseIterator lhs, BaseIterator rhs) { return !(lhs < rhs); }
    };

public:
    //! \brief Iterator over mutable objects.
    class iterator : public BaseIterator, public std::iterator<std::forward_iterator_tag, Object> {
        friend class Space;
        using BaseIterator::BaseIterator;

    public:
        //! \copydoc BaseIterator::operator++()
        iterator& operator++();

        //! \copybrief BaseIterator::operator++()
        iterator operator++(int);

        //! \brief Get a reference to the current object.
        Object& operator*() const;

        //! \brief Get a pointer to the current object.
        Ref operator->() const;

        //! \copydoc BaseIterator::BaseIterator()
        iterator() = default;
    };

    //! \brief Iterator over const objects.
    class const_iterator : public BaseIterator, public std::iterator<std::forward_iterator_tag, Object const> {
        friend class Space;
        const_iterator(CRef obj);

    public:
        //! \copydoc BaseIterator::operator++()
        const_iterator& operator++();

        //! \copybrief BaseIterator::operator++(int)
        const_iterator operator++(int);

        //! \copydoc iterator::operator*()
        Object const& operator*() const;

        //! \copydoc iterator::operator->()
        CRef operator->() const;

        //! \copydoc BaseIterator::BaseIterator()
        const_iterator() = default;
    };

    //! \brief Construct an empty space.
    Space();
    Space(Space const&) = delete;
    //! \brief Construct a space from another space, leaving the other space empty.
    Space(Space&& other);
    Space& operator=(Space const&) = delete;
    //! \brief Overwrite the current space with the other, leaving the other empty.
    Space& operator=(Space&& other);

    //! \brief Swap this space with other.
    void swap(Space& other);

    //! \brief Initialize the space to the given size.
    //
    //  Should only be called when the space is not already initialized; you cannot use this to resize a space.
    void init_space(std::size_t size);

    //! \brief Reset the space to be empty again, clearing any resources.
    void deinit_space();

    //! \brief Allocate an object of the given size.
    Ref allocate(std::size_t size);

    // It can make sense to give the last object extra space.  We don't know
    // what object was the last so we need to provide that, as well as the new
    // size.
    //
    // Using this on any object except the one allocated last is undefined
    // behaviour.  Don't do it.
    //
    // Returns whether the extension succeeded.

    /*! \brief Extend the last allocation to be of the given size.
     *
     *  This can only ever enlarge the object, and may only be called on the last object allocated.  We need to be given
     *  a pointer to this object, as we don't usually know what was allocated last.
     */
    bool extend(Ref obj, std::size_t size);

    //! \brief Copy the given object into this space, updating the pointer to it.
    void migrate(Ref& obj);

    //! \brief Check whether this space is initialized.
    bool initialized() const;
    //! \brief Return the number of bytes this space can contain in total.
    std::size_t size() const;
    //! \brief Return the number of bytes already allocated.
    std::size_t bytes_allocated() const;
    //! \brief Return the number of bytes still available.
    std::size_t bytes_available() const;
    //! \brief Check whether the given pointer is pointing into this space.
    bool contains(CRef ptr) const;

    //! \brief Return an iterator to the first allocated object.
    iterator begin();
    //! \brief Return an iterator to one past the last allocated object.
    iterator end();
    //! \copydoc Space::begin()
    const_iterator begin() const;
    //! \copydoc Space::end()
    const_iterator end() const;
    //! \copydoc Space::begin()
    const_iterator cbegin() const;
    //! \copydoc Space::end()
    const_iterator cend() const;

    //! \brief Return the byte offset of the object from the beginning of the heap.
    std::size_t to_offset(CRef obj) const;
    //! \brief Return the object at the given offset.
    Ref from_offset(std::size_t offset);
    //! \copydoc Space::from_offset(std::size_t)
    CRef from_offset(std::size_t offset) const;

    //! \brief Print a hexadecimal representation of the space.
    void print_hexdump(std::ostream& os) const;
};
