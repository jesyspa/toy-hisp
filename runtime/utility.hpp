#pragma once

#include "object.hpp"
#include <cassert>

// Check whether the object is of the given type
template<typename T>
bool is(CRef obj) {
    assert(obj && "invalid pointer");
    return obj->type == T::TYPE;
}

// Unconditionally treat the pointer as referring to the given type.
template<typename T>
T* cast(Ref obj) {
    assert(is<T>(obj) && "type mismatch");
    return reinterpret_cast<T*>(obj);
}

template<typename T>
T const* cast(CRef obj) {
    assert(is<T>(obj) && "type mismatch");
    return reinterpret_cast<T const*>(obj);
}

// Attempt to cast to a pointer of the given type.  Returns nullptr if the referent is of a
// different type.
template<typename T>
T* try_cast(Ref obj) {
    if (is<T>(obj))
        return reinterpret_cast<T*>(obj);
    return nullptr;
}

template<typename T>
T const* try_cast(CRef obj) {
    if (is<T>(obj))
        return reinterpret_cast<T const*>(obj);
    return nullptr;
}

