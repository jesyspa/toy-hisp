#include "main.hpp"
#include "garbage_collection.hpp"
#include "utility.hpp"
#include <cassert>
#include <list>
#include <iterator>

namespace {
    std::list<ref> root_set;
    ref first;
    std::size_t object_count;
}

root::root(ref p) {
    root_set.push_front(p);
    it = root_set.begin();
}

root::root(root&& o) : it(o.it) {
    o.it = iter_t{};
}

root& root::operator=(root&& o) {
    swap(o);
    return *this;
}

root::~root() {
    root_set.erase(it);
}

void root::swap(root& o) {
    std::swap(it, o.it);
}

template<typename T>
WARN_UNUSED_RESULT
T* new_object() {
    if (++object_count > 1024*1024) {
        printf("too many objects!");
        exit(EXIT_FAILURE);
    }
    auto obj = static_cast<T*>(malloc(sizeof(T)));
    if (!obj) {
        // If we ran out of memory, run a garbage collection pass and
        // then try again.
        collect_garbage();
        obj = static_cast<T*>(malloc(sizeof(T)));
        if (!obj) {
            printf("out of memory");
            exit(EXIT_FAILURE);
        }
    }
    obj->next = nullptr;
    obj->type = T::TYPE;
    // For debugging purposes, we always collect garbage.
    collect_garbage();
    if (!first) {
        first = obj;
    } else {
        obj->next = first;
        first = obj;
    }
    return obj;
}


WARN_UNUSED_RESULT
safe_ref<application> make_application(ref left, ref right) {
    auto app = new_object<application>();
    app->left = left;
    app->right = right;
    return app;
}

WARN_UNUSED_RESULT
safe_ref<number> make_number(int value) {
    auto num = new_object<number>();
    num->value = value;
    return num;
}

WARN_UNUSED_RESULT
safe_ref<function> make_function(func_t func) {
    auto fun = new_object<function>();
    fun->func = func;
    return fun;
}

void walk(ref r) {
    if (r->used)
        return;
    r->used = true;
    if (auto* app = try_cast<application>(r)) {
        walk(app->left);
        walk(app->right);
    }
}

void collect_garbage() {
    for (auto p = first; p; p = p->next)
        p->used = false;

    for (auto p : root_set)
        walk(p);

    while (first && !first->used) {
        auto* p = first->next;
        free(first);
        --object_count;
        first = p;
    }
    if (!first)
        return;
    for (auto p = first; p->next; p = p->next) {
        if (p->next->used)
            continue;
        auto* next = p->next;
        p->next = next->next;
        free(next);
        --object_count;
    }
}

WARN_UNUSED_RESULT
ref make_bool(bool b) {
    if (b)
        return make_function(comb_k);
    else
        return make_application(make_function(comb_k), make_function(comb_i));
}

