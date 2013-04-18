#pragma once
#include <memory>
#include <list>
#include <iterator>

#ifdef __GNUC__
#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define WARN_UNUSED_RESULT
#endif

enum class object_type {
    application_object,
    number_object,
    function_object,
    stack_link_object
};

struct object {
    bool used;
    object_type type;
    object* next;
};

using ref = object*;

struct stack_link;

class root {
    friend void collect_garbage();
    struct link;
    link* it;
    static link* used;

public:
    root();
    root(ref p);
    root(root const&) = delete;
    root(root&& o);
    root& operator=(root const&) = delete;
    root& operator=(root&& o);
    ~root();
    void swap(root& o);
};

template<typename T>
class safe_ref {
    template<typename S>
    friend class safe_ref;
    root protector;
    T* ptr;

public:
    safe_ref() : ptr() {}
    template<typename S>
    safe_ref(safe_ref<S>&& o) : protector(std::move(o.protector)), ptr(o.get()) {
    }
    safe_ref(T* p) : protector(p), ptr(p) {}
    safe_ref<T>& operator=(T* p) {
        root tmp(p);
        protector.swap(tmp);
        ptr = p;
        return *this;
    }

    operator T*() const {
        return ptr;
    }

    T* operator->() const {
        return ptr;
    }

    T& operator*() const {
        return *ptr;
    }

    T* get() const {
        return ptr;
    }
};

using stack = safe_ref<stack_link>;

struct application : object {
    ref left, right;
    static constexpr object_type TYPE = object_type::application_object;
};

struct number : object {
    int value;
    static constexpr object_type TYPE = object_type::number_object;
};

using func_t = safe_ref<object> (*)(stack&);

struct function : object {
    func_t func;
    static constexpr object_type TYPE = object_type::function_object;
};

struct stack_link : object {
    stack_link* prev;
    application* arg;
    static constexpr object_type TYPE = object_type::stack_link_object;
};

WARN_UNUSED_RESULT
safe_ref<application> make_application(ref left, ref right);

WARN_UNUSED_RESULT
safe_ref<number> make_number(int value);

WARN_UNUSED_RESULT
safe_ref<function> make_function(func_t func);

WARN_UNUSED_RESULT
safe_ref<stack_link> make_stack_link(stack_link* prev, application* arg);

WARN_UNUSED_RESULT
ref make_bool(bool b);

void collect_garbage();

safe_ref<object> eval(ref);

safe_ref<object> comb_i(stack& sl);
safe_ref<object> comb_k(stack& sl);
safe_ref<object> comb_s(stack& sl);
safe_ref<object> comb_l(stack& sl);
safe_ref<object> comb_r(stack& sl);
safe_ref<object> comb_y(stack& sl);
safe_ref<object> print(stack& sl);
safe_ref<object> add(stack& sl);
safe_ref<object> sub(stack& sl);
safe_ref<object> le(stack& sl);

