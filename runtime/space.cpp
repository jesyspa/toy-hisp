#include "space.hpp"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>

Space::Space()
    : bottom_{}
    , free_bottom_{}
    , top_{} {}

Space::Space(Space&& other)
    : Space() {
    swap(other);
}

Space& Space::operator=(Space&& other) {
    swap(other);
    return *this;
}

void Space::swap(Space& other) {
    std::swap(bottom_, other.bottom_);
    std::swap(free_bottom_, other.free_bottom_);
    std::swap(top_, other.top_);
}

void Space::init_space(std::size_t size) {
    assert(!initialized() && "cannot double-initialize space");
    bottom_ = static_cast<char*>(calloc(size, 1));
    free_bottom_ = bottom_;
    top_ = bottom_ + size;
}

void Space::deinit_space() {
    assert(initialized() && "cannot deinitialize uninitialized space");
    free(bottom_);
    bottom_ = free_bottom_ = top_ = nullptr;
}

Ref Space::allocate(std::size_t size) {
    if (size > bytes_available())
        return nullptr;
    auto obj = reinterpret_cast<Ref>(free_bottom_);
    free_bottom_ += size;
    obj->size = size;
    obj->forward = obj;
    return obj;
}

bool Space::extend(Ref obj, std::size_t size) {
    assert(contains(obj) && "trying to extend foreign object");
    auto ptr = reinterpret_cast<char*>(obj);
    std::size_t old_size = free_bottom_ - ptr;
    std::size_t increase = size - old_size;
    if (increase > bytes_available())
        return false;
    free_bottom_ += increase;
    obj->size = size;
    return true;
}

void Space::migrate(Ref& obj) {
    assert(obj && "migrating a nullptr");
    if (obj->forward != obj) {
        obj = obj->forward;
        return;
    }

    auto new_obj = allocate(obj->size);
    std::memcpy(new_obj, obj, obj->size);
    obj = obj->forward = new_obj->forward = new_obj;
}

bool Space::initialized() const { return bottom_ != nullptr; }

std::size_t Space::size() const { return static_cast<std::size_t>(top_ - bottom_); }

std::size_t Space::bytes_allocated() const { return static_cast<std::size_t>(free_bottom_ - bottom_); }

std::size_t Space::bytes_available() const { return static_cast<std::size_t>(top_ - free_bottom_); }

bool Space::contains(CRef obj) const {
    assert(initialized() && "using uninitialized space");
    auto ptr = reinterpret_cast<char const*>(obj);
    return bottom_ <= ptr && ptr < top_;
}

std::size_t Space::to_offset(CRef obj) const {
    assert(initialized() && "using uninitialized space");
    auto ptr = reinterpret_cast<char const*>(obj);
    return ptr - bottom_;
}

CRef Space::from_offset(std::size_t offset) const {
    assert(initialized() && "using uninitialized space");
    return reinterpret_cast<CRef>(bottom_ + offset);
}

Ref Space::from_offset(std::size_t offset) {
    assert(initialized() && "using uninitialized space");
    return reinterpret_cast<Ref>(bottom_ + offset);
}

void Space::print_readable(std::ostream& os) const {
    for (std::size_t i = 0; i < size(); ++i) {
        if (i != 0 && i % 8 == 0)
            os << '\n' << (void*)(bottom_ + i) << ": ";

        unsigned val = static_cast<unsigned char>(bottom_[i]);
        os << std::setfill('0') << std::setw(2) << std::hex << val;
        if (i % 8 != 7)
            os << ' ';
    }
}
