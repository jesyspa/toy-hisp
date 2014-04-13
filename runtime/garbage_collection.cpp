#include "main.hpp"
#include "garbage_collection.hpp"
#include "utility.hpp"
#include "debug.hpp"
#include "construct.hpp"
#include <array>
#include <cassert>
#include <cstring>
#include <iterator>
#include <list>

bool garbage_state = false;

namespace {
    ref first;
    std::size_t object_count;
    std::size_t default_check_at = 64;
    std::size_t check_at = default_check_at;

    stack global_stack;
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
    obj->allocated = true;
    obj->next = nullptr;
    obj->type = T::TYPE;
#ifndef NDEBUG
    (void)object_count;
    (void)check_at;
    collect_garbage();
#else
    if (object_count >= check_at)
        collect_garbage();
#endif
    obj->used = !garbage_state;
    obj->next = first;
    first = obj;
    return obj;
}

void free_object(object* p) {
    p->allocated = false;
#ifdef NDEBUG
    free(p);
#endif
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

void assert_global_sanity() {
    for (auto p = first; p ; p = p->next)
        ASSERT_SANITY(p);
}

void dump_memory() {
#ifndef NDEBUG
    print_one(multi_graph{global_stack.base(), global_stack.top(), first});
#endif
}

void walk(ref r) {
    assert(r && "walking over nothing");
    assert(r->allocated);
    if (r->used != garbage_state)
        return;
    r->used = !garbage_state;
    if (auto* app = try_cast<application>(r)) {
        walk(app->left);
        walk(app->right);
    }
}

template<typename F>
void on_all_roots(F f) {
    for (auto it = global_stack.base(); it != global_stack.top(); ++it)
        f(*it);
}

void collect_garbage() {
    garbage_state = !garbage_state;

    on_all_roots(walk);

    dump_memory();

    bool cleaned_any = false;

    while (first && first->used == garbage_state) {
        cleaned_any = true;
        auto p = first->next;
        free_object(first);
        first = p;
    }
    for (auto p = first; p && p->next; ) {
        if (p->next->used != garbage_state) {
            p = p->next;
            continue;
        }
        cleaned_any = true;
        auto* next = p->next;
        p->next = next->next;
        free_object(next);
    }
    check_at = std::min(2*object_count, default_check_at);
    assert_global_sanity();
    if (cleaned_any)
        dump_memory();
}

WARN_UNUSED_RESULT
ref make_bool(bool b) {
    if (b)
        return make_function(comb_k);
    else
        return mk_app(comb_k, comb_i);
}

stack_ref request_stack() {
    return global_stack.get_ref();
}

void print_allocated() {
#ifndef NDEBUG
    printf("total: %lu", object_count);
    for (auto p = first; p ; p = p->next) {
        ASSERT_SANITY(p);
        print_one(p);
        printf("\n");
    }
#endif
}

