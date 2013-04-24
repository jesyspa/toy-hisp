#include "debug.hpp"
#include "utility.hpp"
#include "garbage_collection.hpp"
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
    ASSERT_SANITY(r);
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
    }
}

void internal_print(ref r) {
    std::vector<object*> objs;
    internal_print_impl(r, objs, false);
}

void graphviz_dump_impl(ref r, std::set<object*>& objs) {
    ASSERT_SANITY(r);
    if (objs.count(r))
        return;
    else
        objs.insert(r);
    if (r->used == garbage_state)
        printf("c_%p [color=red];\n", (void*)r);
    if (auto* app = try_cast<application>(r)) {
        printf("c_%p -> c_%p;\n", (void*)app, (void*)app->left);
        printf("c_%p -> c_%p [color=\"blue\"];\n", (void*)app, (void*)app->right);
        graphviz_dump_impl(app->left, objs);
        graphviz_dump_impl(app->right, objs);
    } else if (auto* num = try_cast<number>(r)) {
        printf("c_%p [label=\"%d\"];\n", (void*)num, num->value);
    } else if (auto* fun = try_cast<function>(r)) {
        if (!func_names[fun->func])
            func_names[fun->func] = "???";
        printf("c_%p [label=\"%s\"];\n", (void*)fun, func_names[fun->func]);
    }
}

void graphviz_dump(graph g) {
    static int i = 0;
    std::set<object*> objs;
    printf("cat << EOF > g_%04d.dot\n", i++);
    printf("digraph {\n");
    graphviz_dump_impl(g.r, objs);
    printf("}\n");
    printf("EOF\n");
}

void multi_graphviz_dump(multi_graph g) {
    static int i = 0;
    std::set<object*> objs;
    printf("cat << EOF > mg_%04d.dot\n", i++);
    printf("digraph {\n");
    for (auto p = g.r; p ; p = p->next)
        graphviz_dump_impl(p, objs);
    printf("}\n");
    printf("EOF\n");
}
