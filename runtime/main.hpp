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
    function_object
};

struct object {
    bool used;
    object_type type;
    object* next;
};

using ref = object*;

struct stack_link;
using stack = std::unique_ptr<stack_link>;

struct application : object {
    ref left, right;
    static constexpr object_type TYPE = object_type::application_object;
};

struct number : object {
    int value;
    static constexpr object_type TYPE = object_type::number_object;
};

using func_t = ref (*)(stack&);

struct function : object {
    func_t func;
    static constexpr object_type TYPE = object_type::function_object;
};

class root {
    using iter_t = std::list<ref>::iterator;
    iter_t it;

public:
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
    root protector;
    T* ptr;

public:
    safe_ref(T* p) : protector(p), ptr(p) {}

    operator T*() const {
        return ptr;
    }
};

WARN_UNUSED_RESULT
safe_ref<application> make_application(ref left, ref right);

WARN_UNUSED_RESULT
safe_ref<number> make_number(int value);

WARN_UNUSED_RESULT
safe_ref<function> make_function(func_t func);

WARN_UNUSED_RESULT
ref make_bool(bool b);

void collect_garbage();

ref eval(ref);

ref comb_i(stack& sl);
ref comb_k(stack& sl);
ref comb_s(stack& sl);
ref comb_l(stack& sl);
ref comb_r(stack& sl);
ref comb_y(stack& sl);
ref print(stack& sl);
ref add(stack& sl);
ref sub(stack& sl);
ref le(stack& sl);

