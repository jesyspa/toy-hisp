#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "garbage_collection.hpp"
#include <cassert>

namespace {
    function make_static_comb_i() {
        function f{};
        f.func = comb_i;
        f.allocated = true;
        f.type = function::TYPE;
        return f;
    }
    function static_comb_i = make_static_comb_i();
}

ref update(application* app, ref newval) {
    if (auto napp = try_cast<application>(newval)) {
        app->left = napp->left;
        app->right = napp->right;
    } else {
        app->right = newval;
        app->left = &static_comb_i;
    }
    return app;
}

// Evaluate the expression at the top of stack_ref, returning the result on
// the same stack.  We assume the expression is the only thing currently on
// the stack.
void eval(stack_ref s) {
    assert(s.singleton() && "incorrect number of args");

    while (is<application>(s.top()) || !s.singleton()) {
        ASSERT_SANITY(s.top());
        while (auto app = try_cast<application>(s.top()))
            s.push(app->left);

        dump_memory();
        assert(is<function>(s.top()) && "type error: trying to apply non-func");
        auto f = s.extract_as<function>();

        f->func(s);
        dump_memory();
    }
}
