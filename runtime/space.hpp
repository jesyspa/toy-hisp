#pragma once

#include "object.hpp"
#include <cstdint>
#include <iosfwd>

// Allocation space
class Space {
    // pointer to the beginning of the space
    char* bottom_;
    // pointer to the beginning of the unallocated part
    char* free_bottom_;
    // pointer one past the end of the allocated part
    char* top_;

    // Base iterator for iterating over objects.  Note that we always use Ref as
    // the underlying pointer type; we ensure in const_iterator that the referent
    // is not accidentally modified.
    class BaseIterator {
    protected:
        Ref obj_;

        BaseIterator(Ref obj);
        BaseIterator();

        BaseIterator& operator++();
        Ref operator++(int);

        void increment();

    public:
        friend bool operator==(BaseIterator lhs, BaseIterator rhs) {
            return lhs.obj_ == rhs.obj_;
        }

        friend bool operator!=(BaseIterator lhs, BaseIterator rhs) {
            return !(lhs == rhs);
        }

        friend bool operator<(BaseIterator lhs, BaseIterator rhs) {
            return lhs.obj_ < rhs.obj_;
        }

        friend bool operator>(BaseIterator lhs, BaseIterator rhs) {
            return rhs < lhs;
        }

        friend bool operator<=(BaseIterator lhs, BaseIterator rhs) {
            return !(rhs < lhs);
        }

        friend bool operator>=(BaseIterator lhs, BaseIterator rhs) {
            return !(lhs < rhs);
        }
    };

public:

    class iterator : public BaseIterator {
        friend class Space;
        iterator(Ref obj);

    public:
        iterator& operator++();
        iterator operator++(int);

        Ref operator*() const;

        iterator() = default;
    };

    class const_iterator : public BaseIterator {
        friend class Space;
        const_iterator(CRef obj);

    public:
        const_iterator& operator++();
        const_iterator operator++(int);

        CRef operator*() const;

        const_iterator() = default;
    };

    Space();

    void init_space(std::size_t);
    void deinit_space();

    Ref allocate(std::size_t);
    // It can make sense to give the last object extra space.  We don't know
    // what object was the last so we need to provide that, as well as the new
    // size.
    //
    // Using this on any object except the one allocated last is undefined
    // behaviour.  Don't do it.
    //
    // Returns whether the extension succeeded.
    bool extend(Ref obj, std::size_t);


    bool initialized() const;
    std::size_t size() const;
    std::size_t bytes_allocated() const;
    std::size_t bytes_available() const;
    bool contains(CRef ptr) const;

    iterator begin();
    iterator end();
    const_iterator begin() const;
    const_iterator end() const;
    const_iterator cbegin() const;
    const_iterator cend() const;

    std::size_t to_offset(CRef obj) const;
    CRef from_offset(std::size_t offset) const;
    Ref from_offset(std::size_t offset);

    void print_readable(std::ostream& os) const;
};
