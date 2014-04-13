#pragma once

#include "object.hpp"
#include <cassert>

template<typename T>
bool is(ref r) {
    assert(r && "null type");
    return r->type == T::TYPE;
}

template<typename T>
T* cast(ref r) {
    assert(is<T>(r) && "type mismatch");
    return reinterpret_cast<T*>(r);
}

template<typename T>
T* try_cast(ref r) {
    if (is<T>(r))
        return reinterpret_cast<T*>(r);
    return nullptr;
}

