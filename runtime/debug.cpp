#include "debug.hpp"
#include "utility.hpp"
#include <map>

std::map<func_t, char const*> func_names = {
#define ENTRY(name) {name, #name }
    ENTRY(comb_i),
    ENTRY(comb_k),
    ENTRY(comb_s),
    ENTRY(comb_l),
    ENTRY(comb_r),
    ENTRY(comb_y),
    ENTRY(print),
    ENTRY(add),
    ENTRY(sub),
    ENTRY(le)
#undef ENTRY
};

// Used to prevent recursion from going too far, making errors more readable
// when we end up with a cyclic graph.
#define MURDER_STACK volatile char arr[64*1024]; (void) arr

void internal_print(ref r, bool parens) {
    MURDER_STACK;
    if (auto* app = try_cast<application>(r)) {
        if (parens)
            printf("(");
        internal_print(app->left, false);
        printf(" ");
        internal_print(app->right, true);
        if (parens)
            printf(")");
    } else if (auto* n = try_cast<number>(r)) {
        printf("%d", n->value);
    } else if (auto* f = try_cast<function>(r)){
        printf("%s", func_names[f->func]);
    }
}

void graphviz_dump(graph g) {
    MURDER_STACK;
    if (auto* app = try_cast<application>(g.r)) {
        printf("c_%p -> c_%p;\n", (void*)app, (void*)app->left);
        printf("c_%p -> c_%p;\n", (void*)app, (void*)app->right);
        graphviz_dump(graph{app->left});
        graphviz_dump(graph{app->right});
    }
}

