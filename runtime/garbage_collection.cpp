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

namespace {
    std::size_t const HEAP_SIZE = 1024;
    char* active_space;
    char* space_bottom;
    char* space_top;

    Stack global_stack;
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

Ref allocate_from(char*& space, std::size_t bytes) {
    auto obj = reinterpret_cast<Ref>(space);
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

void make_application(SubStack stack) {
    auto app = new_object<Application>();
    app->right = stack.extract();
    app->left = stack.extract();
    stack.push(app);
}

void make_number(SubStack stack, int value) {
    auto num = new_object<Number>();
    num->value = value;
    stack.push(num);
}

void make_function(SubStack stack, Func func) {
    auto fun = new_object<Function>();
    fun->func = func;
    stack.push(fun);
}

void dump_memory() {
#ifndef NDEBUG
#ifndef DUMP_RAW
    print_one(MultiGraphBag{global_stack, active_space, (std::size_t(space_bottom - active_space))});
#else
    print_one(MemoryBag{global_stack, active_space, (std::size_t)(space_bottom - active_space)});
#endif
#endif
}

void move_ptr(char*& bottom, Ref& obj) {
    assert(obj && "moving a nullptr");
    if (obj->forward != obj) {
        obj = obj->forward;
        return;
    }
    assert(is_heap_ptr(obj));

    auto new_obj = allocate_from(bottom, obj->size);
    std::memcpy(new_obj, obj, obj->size);
    obj = obj->forward = new_obj->forward = new_obj;
}

void scan(Ref obj, char*& bottom) {
    if (auto app = try_cast<Application>(obj)) {
        move_ptr(bottom, app->left);
        move_ptr(bottom, app->right);
    }
}

void update_roots(char*& bottom) {
    for (auto& e : global_stack)
        move_ptr(bottom, e);
}

void update_semispace(char* space, char*& bottom) {
    char* scanned = space;
    while (scanned != bottom) {
        auto obj = reinterpret_cast<Ref>(scanned);
        scan(obj, bottom);
        scanned += obj->size;
    }
}

void collect_garbage() {
    char* tospace = allocate_space();
    char* tospace_bottom = tospace;
    char* tospace_top = tospace + HEAP_SIZE;
    update_roots(tospace_bottom);
    update_semispace(tospace, tospace_bottom);

    deallocate_space(active_space);

    active_space = tospace;
    space_bottom = tospace_bottom;
    space_top = tospace_top;
}

void make_bool(SubStack stack, bool value) {
    if (value) {
        make_function(stack, comb_k);
    } else {
        make_function(stack, comb_k);
        make_function(stack, comb_i);
        make_application(stack);
    }
}

SubStack request_stack() {
    return global_stack.get_ref();
}

bool is_heap_ptr(void const* ptr) {
    auto c = static_cast<char const*>(ptr);
    return active_space <= c && c < space_top;
}
