#pragma once

#include "object.hpp"
#include <cassert>

template<typename T>
bool is(CRef obj) {
    assert(obj && "null type");
    return obj->type == T::TYPE;
}

template<typename T>
T* cast(Ref obj) {
    assert(is<T>(obj) && "type mismatch");
    return static_cast<T*>(obj);
}

template<typename T>
T const* cast(CRef obj) {
    assert(is<T>(obj) && "type mismatch");
    return static_cast<T const*>(obj);
}

template<typename T>
T* try_cast(Ref obj) {
    if (is<T>(obj))
        return static_cast<T*>(obj);
    return nullptr;
}

template<typename T>
T const* try_cast(CRef obj) {
    if (is<T>(obj))
        return static_cast<T const*>(obj);
    return nullptr;
}

