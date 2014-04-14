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
    std::size_t const HEAP_SIZE = 1024;
    char* active_space;
    char* space_bottom;
    char* space_top;

    stack global_stack;
}

char* allocate_space() {
    return static_cast<char*>(calloc(HEAP_SIZE, 1));
}

void deallocate_space(void* ptr) {
#ifdef NDEBUG
    free(ptr);
#else
    (void)ptr;
#endif
}

void init_gc() {
    assert(!active_space && "gc already initialized");

    active_space = allocate_space();
    space_bottom = active_space;
    space_top = active_space + HEAP_SIZE;
}

ref allocate_from(char*& space, std::size_t bytes) {
    auto obj = reinterpret_cast<ref>(space);
    space += bytes;
    return obj;
}

template<typename T>
WARN_UNUSED_RESULT
T* new_object() {
    // Ensure we have enough memory.
    if (space_top - space_bottom < sizeof(T)) {
        // If not, try again after garbage collection.
        collect_garbage();
        if (space_top - space_bottom < sizeof(T)) {
            printf("out of memory\n");
            std::exit(-1);
        }
    }
    auto obj = allocate_from(space_bottom, sizeof(T));
    obj->type = T::TYPE;
    obj->forward = obj;
    obj->size = sizeof(T);
    return static_cast<T*>(obj);
}

void make_application(stack_ref s) {
    auto app = new_object<application>();
    app->right = s.extract();
    app->left = s.extract();
    s.push(app);
}

void make_number(stack_ref s, int value) {
    auto num = new_object<number>();
    num->value = value;
    s.push(num);
}

void make_function(stack_ref s, func_t func) {
    auto fun = new_object<function>();
    fun->func = func;
    s.push(fun);
}

void dump_memory() {
#ifndef NDEBUG
#ifndef DUMP_RAW
    print_one(multi_graph{global_stack.base(), global_stack.top(), active_space, (std::size_t(space_bottom - active_space))});
#else
    print_one(memory{global_stack.base(), global_stack.top(), active_space, (std::size_t)(space_bottom - active_space)});
#endif
#endif
}

void move_ptr(char*& bottom, ref& r) {
    assert(r && "moving a nullptr");
    if (r->forward != r) {
        r = r->forward;
        return;
    }
    assert(is_heap_ptr(r));

    auto new_r = allocate_from(bottom, r->size);
    std::memcpy(new_r, r, r->size);
    r = r->forward = new_r->forward = new_r;
}

void scan(ref obj, char*& bottom) {
    if (auto app = try_cast<application>(obj)) {
        move_ptr(bottom, app->left);
        move_ptr(bottom, app->right);
    }
}

void update_roots(char*& bottom) {
    for (auto it = global_stack.base(); it != global_stack.top(); ++it)
        move_ptr(bottom, *it);
}

void update_semispace(char* space, char*& bottom) {
    char* scanned = space;
    while (scanned != bottom) {
        auto obj = reinterpret_cast<ref>(scanned);
        scan(obj, bottom);
        scanned += obj->size;
    }
}

void collect_garbage() {
    dump_memory();

    char* tospace = allocate_space();
    char* tospace_bottom = tospace;
    char* tospace_top = tospace + HEAP_SIZE;
    update_roots(tospace_bottom);
    update_semispace(tospace, tospace_bottom);

    deallocate_space(active_space);

    active_space = tospace;
    space_bottom = tospace_bottom;
    space_top = tospace_top;

    dump_memory();
}

void make_bool(stack_ref s, bool b) {
    if (b) {
        make_function(s, comb_k);
    } else {
        make_function(s, comb_k);
        make_function(s, comb_i);
        make_application(s);
    }
}

stack_ref request_stack() {
    return global_stack.get_ref();
}

bool is_heap_ptr(void const* ptr) {
    auto c = static_cast<char const*>(ptr);
    return active_space <= c && c < space_top;
}
