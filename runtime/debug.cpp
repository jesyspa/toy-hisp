#include "debug.hpp"
#include "utility.hpp"
#include <map>
#include <set>
#include <vector>
#include <iterator>
#include <algorithm>

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

void internal_print_impl(ref r, std::vector<object*>& objs, bool parens) {
    if (auto* app = try_cast<application>(r)) {
        if (std::find(objs.begin(), objs.end(), r) != objs.end()) {
            printf("<loop>");
            return;
        }
        objs.push_back(r);
        if (parens)
            printf("(");
        internal_print_impl(app->left, objs, false);
        printf(" ");
        internal_print_impl(app->right, objs, true);
        if (parens)
            printf(")");
        objs.pop_back();
    } else if (auto* n = try_cast<number>(r)) {
        printf("%d", n->value);
    } else if (auto* f = try_cast<function>(r)){
        printf("%s", func_names[f->func]);
    } else if (auto* sl = try_cast<stack_link>(r)) {
        printf("sl [");
        internal_print_impl(sl->arg, objs, false);
        printf("]");
        if (sl->prev) {
            printf(" -> ");
            internal_print_impl(sl->prev, objs, false);
        }
    }
}

void internal_print(ref r) {
    std::vector<object*> objs;
    internal_print_impl(r, objs, false);
}

void graphviz_dump_impl(ref r, std::set<object*>& objs) {
    if (objs.count(r))
        return;
    else
        objs.insert(r);
    if (auto* app = try_cast<application>(r)) {
        printf("c_%p -> c_%p;\n", (void*)app, (void*)app->left);
        printf("c_%p -> c_%p;\n", (void*)app, (void*)app->right);
        graphviz_dump_impl(app->left, objs);
        graphviz_dump_impl(app->right, objs);
    }
}

void graphviz_dump(graph g) {
    std::set<object*> objs;
    printf("digraph {\n");
    graphviz_dump_impl(g.r, objs);
    printf("}");
}
