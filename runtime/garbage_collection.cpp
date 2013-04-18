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

root::root() : it(root_set.end()) {}

root::root(ref p) {
    if (p) {
        root_set.push_front(p);
        it = root_set.begin();
    } else {
        it = root_set.end();
    }
}

root::root(root&& o) : it(o.it) {
    o.it = iter_t{};
}

root& root::operator=(root&& o) {
    swap(o);
    return *this;
}

root::~root() {
    if (it != root_set.end())
        root_set.erase(it);
}

void root::swap(root& o) {
    std::swap(it, o.it);
}

void root::dismiss() {
    it = root_set.end();
}

template<typename T>
WARN_UNUSED_RESULT
T* new_object() {
    assert(++object_count <= 1024*1024 && "too many objects!");
    auto obj = static_cast<T*>(malloc(sizeof(T)));
    if (!obj) {
        // If we ran out of memory, run a garbage collection pass and
        // then try again.
        collect_garbage();
        obj = static_cast<T*>(malloc(sizeof(T)));
        assert(obj && "out of memory");
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
    PRESERVE(left);
    PRESERVE(right);
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

WARN_UNUSED_RESULT
safe_ref<stack_link> make_stack_link(stack_link* prev, application* arg) {
    PRESERVE(prev);
    PRESERVE(arg);
    auto link = new_object<stack_link>();
    link->prev = prev;
    link->arg = arg;
    return link;
}

void walk(ref r) {
    assert(r && "walking over nothing");
    if (r->used)
        return;
    r->used = true;
    if (auto* app = try_cast<application>(r)) {
        walk(app->left);
        walk(app->right);
    } else if (auto* st = try_cast<stack_link>(r)) {
        if (st->prev)
            walk(st->prev);
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

