#include "garbage_collection.hpp"
#include "debug.hpp"
#include "main.hpp"
#include "serialisation.hpp"
#include "utility.hpp"
#include <array>
#include <cassert>
#include <cstring>
#include <iterator>
#include <list>

namespace {
    Space active_space;
    Stack global_stack;
}

void deallocate_space(void* ptr) {
#ifdef NDEBUG
    free(ptr);
#else
    (void)ptr;
#endif
}

template<typename T>
WARN_UNUSED_RESULT
T* new_object() {
    auto obj = active_space.allocate(sizeof(T));
    if (!obj) {
        // If not, try again after garbage collection.
        collect_garbage();
        obj = active_space.allocate(sizeof(T));
        if (!obj) {
            printf("out of memory\n");
            std::exit(-1);
        }
    }
    obj->type = T::TYPE;
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
    print_one(MultiGraphBag{global_stack, active_space});
#else
    print_one(MemoryBag{global_stack, active_space});
#endif
#endif
}

void create_init_file() {
    assert(global_stack.begin() + 1 == global_stack.end() && "dangerous with so many stacks");
    write_init_file(MemoryInfo{*global_stack.begin(), active_space});
}

SubStack use_init_file() {
    assert(!active_space.initialized() && "memory already initialized");
    auto memory = read_init_file();
    active_space = memory.space;
    auto stack = request_stack();
    stack.push(memory.root);
    return stack;
}

void move_ptr(Space& bottom, Ref& obj) {
    assert(obj && "moving a nullptr");
    if (obj->forward != obj) {
        obj = obj->forward;
        return;
    }
    assert(is_heap_ptr(obj));

    auto new_obj = bottom.allocate(obj->size);
    std::memcpy(new_obj, obj, obj->size);
    obj = new_obj;
}

void scan(Ref obj, Space& tospace) {
    if (auto app = try_cast<Application>(obj)) {
        move_ptr(tospace, app->left);
        move_ptr(tospace, app->right);
    }
}

void update_roots(Space& tospace) {
    for (auto& e : global_stack)
        move_ptr(tospace, e);
}

void update_semispace(Space& tospace) {
    // NOTE: Cannot be replaced with a foreach loop, we are modifying tospace as we go.
    for (auto it = tospace.begin(); it != tospace.end(); ++it)
        scan(*it, tospace);
}

void collect_garbage() {
    Space tospace;
    tospace.init_space(active_space.size());
    update_roots(tospace);
    update_semispace(tospace);

    active_space.deinit_space();

    active_space = tospace;
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

bool is_heap_ptr(CRef obj) {
    return active_space.contains(obj);
}

void deinit_gc() {
    active_space.deinit_space();
}
