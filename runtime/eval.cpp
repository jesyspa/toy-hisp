#include "debug.hpp"
#include "garbage_collection.hpp"
#include "macros.hpp"
#include "main.hpp"
#include "stack.hpp"
#include "utility.hpp"
#include <cassert>

// Evaluate the expression at the top of stack_ref, returning the result on the same stack.  We
// assume the expression is the only thing currently on the stack.
void eval(SubStack stack) {
    assert(stack.singleton() && "incorrect number of args");

    while (is<Application>(stack.top()) || !stack.singleton()) {
        ASSERT_SANITY(stack.top());
        while (auto app = try_cast<Application>(stack.top()))
            stack.push(app->left);

        assert(is<Function>(stack.top()) && "type error: trying to apply non-func");
        auto f = stack.extract_as<Function>();

        f->func(stack);
    }
}
