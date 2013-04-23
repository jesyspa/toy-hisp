#include "main.hpp"
#include "utility.hpp"
#include "stack.hpp"
#include "debug.hpp"
#include "garbage_collection.hpp"
#include <cassert>

ref update(application* app, ref newval) {
    if (auto* napp = try_cast<application>(newval)) {
        app->left = napp->left;
        app->right = napp->right;
    } else {
        app->right = newval;
        app->left = make_function(comb_i);
    }
    return app;
}

ref eval(ref r) {
    stack s;
    register_stack(s);
    while (is<application>(r) || !empty(s)) {
        while (auto app = try_cast<application>(r)) {
            push(s, app);
            r = app->left;
        }

        assert(is<function>(r) && "type error: trying to apply non-func");
        auto* f = cast<function>(r);
        bool id = f->func == comb_i;

        auto result = f->func(s);
        assert(result && "null result");
        if (empty(s))
            r = result;
        else if (id)
            r = cast<application>(top(s))->left = result;
        else
            r = update(cast<application>(cast<application>(top(s))->left), result);
    }
    unregister_stack();
    return r;
}
