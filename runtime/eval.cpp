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
        app->left = make_function(comb_i);
        app->right = newval;
    }
    return app;
}

safe_ref<object> eval(ref r) {
    stack s;
    while (is<application>(r) || s) {
        while (auto app = try_cast<application>(r)) {
            push(s, app);
            r = app->left;
        }

        assert(is<function>(r) && "type error: trying to apply non-func");
        auto* f = cast<function>(r);
        bool id = f->func == comb_i;

        auto result = f->func(s);
        assert(result && "null result");
        if (!s)
            r = result;
        else if (id)
            r = s->arg->left = result;
        else
            r = update(cast<application>(s->arg->left), result);
    }
    return r;
}
