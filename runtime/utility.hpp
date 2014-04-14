#pragma once

#include "object.hpp"
#include <cassert>

template<typename T>
bool is(object const* r) {
    assert(r && "null type");
    return r->type == T::TYPE;
}

template<typename T>
T* cast(ref r) {
    assert(is<T>(r) && "type mismatch");
    return static_cast<T*>(r);
}

template<typename T>
T const* cast(object const* r) {
    assert(is<T>(r) && "type mismatch");
    return static_cast<T const*>(r);
}

template<typename T>
T* try_cast(object* r) {
    if (is<T>(r))
        return static_cast<T*>(r);
    return nullptr;
}

template<typename T>
T const* try_cast(object const* r) {
    if (is<T>(r))
        return static_cast<T const*>(r);
    return nullptr;
}

