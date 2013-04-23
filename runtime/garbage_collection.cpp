#include "main.hpp"
#include "garbage_collection.hpp"
#include "utility.hpp"
#include "debug.hpp"
#include <cassert>
#include <cstring>
#include <list>
#include <iterator>
#include <vector>

namespace {
    bool garbage_state = false;
    ref first;
    std::size_t object_count;
    std::size_t default_check_at = 64;
    std::size_t check_at = default_check_at;

    std::size_t const tmp_root_size = 4;
    ref tmp_roots[tmp_root_size];
    std::size_t next_tmp_root;

    std::vector<stack*> stacks;
}

ref save(ref r) {
    tmp_roots[next_tmp_root++] = r;
    if (next_tmp_root > tmp_root_size)
        next_tmp_root = 0;
    return r;
}

template<typename T>
WARN_UNUSED_RESULT
T* new_object() {
    ++object_count;
    assert(object_count <= 1024*1024 && "too many objects!");
    auto obj = static_cast<T*>(malloc(sizeof(T)));
    if (!obj) {
        // If we ran out of memory, run a garbage collection pass and
        // then try again.
        collect_garbage();
        obj = static_cast<T*>(malloc(sizeof(T)));
        if (!obj) {
            printf("out of memory");
            std::exit(-1);
        }
    }
    obj->next = nullptr;
    obj->type = T::TYPE;
    obj->used = !garbage_state;
#ifndef NDEBUG
    if (object_count >= check_at)
        collect_garbage();
#else
    (void)object_count;
    (void)check_at;
    collect_garbage();
#endif
    obj->next = first;
    first = obj;
    save(obj);
    return obj;
}

void free_object(object* p) {
    free(p);
    --object_count;
}

WARN_UNUSED_RESULT
application* make_application(ref left, ref right) {
    auto app = new_object<application>();
    app->left = left;
    app->right = right;
    return app;
}

WARN_UNUSED_RESULT
number* make_number(int value) {
    auto num = new_object<number>();
    num->value = value;
    return num;
}

WARN_UNUSED_RESULT
function* make_function(func_t func) {
    auto fun = new_object<function>();
    fun->func = func;
    return fun;
}

void walk(ref r) {
    assert(r && "walking over nothing");
    if (r->used != garbage_state)
        return;
    r->used = !garbage_state;
    if (auto* app = try_cast<application>(r)) {
        walk(app->left);
        walk(app->right);
    }
}

void collect_garbage() {
    garbage_state = !garbage_state;

    for (auto p : tmp_roots)
        if (p)
            walk(p);

    for (auto st : stacks)
        for (auto p : *st)
            walk(p);

    if (!first)
        return;
    for (auto p = first; p && p->next; p = p->next) {
        if (p->next->used != garbage_state)
            continue;
        auto* next = p->next;
        p->next = next->next;
        free_object(next);
    }
    check_at = std::min(2*object_count, default_check_at);
}

WARN_UNUSED_RESULT
ref make_bool(bool b) {
    if (b)
        return make_function(comb_k);
    else
        return make_application(
                make_function(comb_k),
                make_function(comb_i));
}

void register_stack(stack& s) {
    stacks.push_back(&s);
}

void unregister_stack() {
    stacks.pop_back();
}
