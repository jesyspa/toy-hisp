#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "garbage_collection.hpp"
#include <cassert>

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
