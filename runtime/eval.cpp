#include "eval.hpp"
#include "debugger.hpp"
#include "macros.hpp"
#include "stack.hpp"
#include "utility.hpp"
#include <cassert>

void eval(Stack stack) {
    // TODO: Split this function into more sensible sub-functions.
    // If we are told to evaluate a forwarder, instead evaluate its target.
    while (auto fwd = stack.try_extract_as<Forwarder>())
        stack.push(fwd->target);

    assert(stack.singleton() && "incorrect number of args");
    // Save the expression we are evaluating; we'll want to turn it into a forwarder later.
    stack.push(stack.top());

    Debugger::step();

    // If there is an application at the top of the stack, we need to push the args and
    // evaluate the result.  If there are more than two objects on the stack, this is implicitly
    // an application.
    while (is<Application>(stack.top()) || stack.size() > 2) {
        ASSERT_SANITY(stack.top());
        // Walk the left spine, putting everything on the spine and resolving forwarders.
        while (auto app = try_cast<Application>(stack.top())) {
            // We only ever fix one forwarder at a time.  This makes it easier to express
            // that we keep on fixing them until there are no more left.
            //
            // If possible, we'd like to guarantee that a forwarder never points to a forwarder.
            // In that case, we could easily split this off into a function and then remove the
            // branches.
            if (auto fwd = try_cast<Forwarder>(app->right))
                app->right = fwd->target;
            else if (auto fwd = try_cast<Forwarder>(app->left))
                app->left = fwd->target;
            else
                stack.push(app->left);
        }

        Debugger::step();

        // We've finally gotten the stack in the shape we want it; the end of the left spine
        // should contain the next function to apply.  Apply it.
        auto f = stack.extract_as<Function>();
        f->func(stack);

        // Try to save the result.
        auto result = stack.extract();
        if (stack.singleton()) {
            // If there's only one thing left on the stack, then that's the expression we were
            // initially going to evaluate, and we should rewrite that into a forwarder now.
            rewrite_as_forwarder(stack.extract(), result);
            stack.push(result);
            stack.push(result);
        } else {
            // Otherwise, we were evaluating the left child of the top expression on the stack.
            // Overwrite that.
            auto old_expr = cast<Application>(stack.top())->left;
            rewrite_as_forwarder(old_expr, result);
        }
    }

    Debugger::step();

    assert(!stack.singleton() && "no result available");
    // There's only two objects left on the stack.  The top is the result; the bottom is the
    // node we were originally given to evaluate (that is now probably a forwarder to our result).
    // The flip and pop together remove the bottom one.
    stack.flip();
    stack.pop();
}
