#pragma once

#include "main.hpp"
#include <cassert>

template<typename T>
bool is(ref r) {
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

template<typename T>
T* eval_as(ref r) {
    return cast<T>(eval(r));
}

