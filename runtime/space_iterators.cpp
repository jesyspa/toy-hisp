#include "space.hpp"

Space::BaseIterator::BaseIterator(Ref obj) : obj_(obj) {}

auto Space::BaseIterator::operator++() -> BaseIterator& {
    increment();
    return *this;
}

auto Space::BaseIterator::operator++(int) -> Ref {
    Ref obj = obj_;
    increment();
    return obj;
}

void Space::BaseIterator::increment() {
    auto ptr = reinterpret_cast<char*>(obj_);
    ptr += obj_->size;
    obj_ = reinterpret_cast<Ref>(ptr);
}

Space::iterator::iterator(Ref obj) : BaseIterator{obj} {}

auto Space::iterator::operator++() -> iterator& {
    return static_cast<iterator&>(BaseIterator::operator++());
}

auto Space::iterator::operator++(int) -> iterator {
    return {BaseIterator::operator++(0)};
}

Ref Space::iterator::operator*() const {
    return obj_;
}

Space::const_iterator::const_iterator(CRef obj) : BaseIterator{const_cast<Ref>(obj)} {}

auto Space::const_iterator::operator++() -> const_iterator& {
    return static_cast<const_iterator&>(BaseIterator::operator++());
}

auto Space::const_iterator::operator++(int) -> const_iterator {
    return {BaseIterator::operator++(0)};
}

CRef Space::const_iterator::operator*() const {
    return obj_;
}
