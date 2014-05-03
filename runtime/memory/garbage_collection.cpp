#include "memory/garbage_collection.hpp"
#include "debug/debugger.hpp"
#include "memory/serialisation.hpp"
#include "memory/stack.hpp"
#include "memory/space.hpp"
#include "hisp/utility.hpp"
#include <cassert>
#include <cstring>
#include <iterator>
#include <list>

namespace {
std::size_t bytes_alive_at_last_collection;
Space active_space;
StackStorage global_stack;
}

template <typename T>
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

void make_application(Stack stack) {
    auto app = new_object<Application>();
    app->right = stack.extract();
    app->left = stack.extract();
    stack.push(app);
}

void make_number(Stack stack, int value) {
    auto num = new_object<Number>();
    num->value = value;
    stack.push(num);
}

void make_function(Stack stack, Func func) {
    auto fun = new_object<Function>();
    fun->func = func;
    stack.push(fun);
}

void create_init_file() {
    assert(global_stack.begin() + 1 == global_stack.end() && "dangerous with so many stacks");
    write_init_file(*global_stack.begin(), active_space);
}

Stack use_init_file(std::string name) {
    assert(!active_space.initialized() && "memory already initialized");
    auto memory = read_init_file(std::move(name));
    active_space = std::move(memory.space);
    auto stack = request_stack();
    stack.push(memory.root);
    return stack;
}

void scan(Object& obj, Space& tospace) {
    if (auto app = try_cast<Application>(obj)) {
        tospace.migrate(app->left);
        tospace.migrate(app->right);
    } else if (auto fwd = try_cast<Forwarder>(obj)) {
        tospace.migrate(fwd->target);
    }
}

void update_roots(Space& tospace) {
    for (auto& e : global_stack)
        tospace.migrate(e);
}

void update_semispace(Space& tospace) {
    // NOTE: Cannot be replaced with a foreach loop, we are modifying tospace as we go.
    for (auto it = tospace.begin(); it != tospace.end(); ++it)
        scan(*it, tospace);
}

void collect_garbage() {
    Space tospace;
    auto const new_size = std::max(active_space.size(), 2 * bytes_alive_at_last_collection);
    tospace.init_space(new_size);
    update_roots(tospace);
    update_semispace(tospace);

    active_space.deinit_space();

    active_space = std::move(tospace);
    bytes_alive_at_last_collection = active_space.bytes_allocated();
}

Stack request_stack() { return global_stack.get_ref(); }

bool is_heap_ptr(CRef obj) { return active_space.contains(obj); }

void deinit_gc() { active_space.deinit_space(); }

DebugMemoryInfo get_debug_memory_info() {
    return {&global_stack, &active_space};
}
