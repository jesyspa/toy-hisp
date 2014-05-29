#include "memory/space.hpp"

// base iterators
Space::BaseIterator::BaseIterator(Ref obj) noexcept : obj_(obj) {}

auto Space::BaseIterator::operator++() noexcept -> BaseIterator& {
    increment();
    return *this;
}

auto Space::BaseIterator::operator++(int) noexcept -> Ref {
    Ref obj = obj_;
    increment();
    return obj;
}

void Space::BaseIterator::increment() noexcept {
    auto ptr = reinterpret_cast<char*>(obj_);
    ptr += obj_->size;
    obj_ = reinterpret_cast<Ref>(ptr);
}

// mutable iterator
auto Space::iterator::operator++() noexcept -> iterator& { return static_cast<iterator&>(BaseIterator::operator++()); }

auto Space::iterator::operator++(int) noexcept -> iterator {
    return {BaseIterator::operator++(0)};
}

Object& Space::iterator::operator*() const noexcept { return *obj_; }

Ref Space::iterator::operator->() const noexcept { return obj_; }

// const iterators
Space::const_iterator::const_iterator(CRef obj) noexcept : BaseIterator{const_cast<Ref>(obj)} {}

auto Space::const_iterator::operator++() noexcept -> const_iterator& {
    return static_cast<const_iterator&>(BaseIterator::operator++());
}

auto Space::const_iterator::operator++(int) noexcept -> const_iterator {
    return {BaseIterator::operator++(0)};
}

Object const& Space::const_iterator::operator*() const noexcept { return *obj_; }

CRef Space::const_iterator::operator->() const noexcept { return obj_; }

// space support
auto Space::begin() -> iterator {
    return iterator{reinterpret_cast<Ref>(bottom_)};
}

auto Space::end() -> iterator {
    return iterator{reinterpret_cast<Ref>(free_bottom_)};
}

auto Space::begin() const -> const_iterator {
    return const_iterator{reinterpret_cast<CRef>(bottom_)};
}

auto Space::end() const -> const_iterator {
    return const_iterator{reinterpret_cast<CRef>(free_bottom_)};
}

auto Space::cbegin() const -> const_iterator { return begin(); }

auto Space::cend() const -> const_iterator { return end(); }
