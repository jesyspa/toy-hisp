#pragma once

#include "object.hpp"
#include <cassert>
#include <type_traits>

// Check whether the object is of the given type
template<typename T>
bool is(CRef obj) {
    assert(obj && "invalid pointer");
    return obj->type == T::TYPE;
}

template<typename T>
bool is_convertible(CRef obj) {
    assert(obj && "invalid pointer");
    if (!std::is_same<T, Forwarder>::value && is<Forwarder>(obj))
        obj = reinterpret_cast<Forwarder const*>(obj)->target;
    return obj->type == T::TYPE;
}

// Unconditionally treat the pointer as referring to the given type.
template<typename T>
T* cast(Ref obj) {
    assert(is_convertible<T>(obj) && "type mismatch");
    if (!std::is_same<T, Forwarder>::value && is<Forwarder>(obj))
        obj = reinterpret_cast<Forwarder*>(obj)->target;
    return reinterpret_cast<T*>(obj);
}

template<typename T>
T const* cast(CRef obj) {
    assert(is_convertible<T>(obj) && "type mismatch");
    if (!std::is_same<T, Forwarder>::value && is<Forwarder>(obj))
        obj = reinterpret_cast<Forwarder const*>(obj)->target;
    return reinterpret_cast<T const*>(obj);
}

template<typename T>
T& cast(Object& obj) {
    return *cast<T>(&obj);
}

template<typename T>
T const& cast(Object const& obj) {
    return *cast<T>(&obj);
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

template<typename T>
T* try_cast(Object& obj) {
    return try_cast<T>(&obj);
}

template<typename T>
T const* try_cast(Object const& obj) {
    return try_cast<T>(&obj);
}

