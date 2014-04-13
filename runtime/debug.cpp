#include "debug.hpp"
#include "utility.hpp"
#include "garbage_collection.hpp"
#include <map>
#include <set>
#include <vector>
#include <iterator>
#include <algorithm>
#include <iomanip>

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
            std::cerr << "<loop>";
            return;
        }
        objs.push_back(r);
        if (parens)
            std::cerr << '(';
        internal_print_impl(app->left, objs, false);
        std::cerr << ' ';
        internal_print_impl(app->right, objs, true);
        if (parens)
            std::cerr << ')';
        objs.pop_back();
    } else if (auto* n = try_cast<number>(r)) {
        std::cerr << n->value;
    } else if (auto* f = try_cast<function>(r)){
        std::cerr << func_names[f->func];
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
    objs.insert(r);
    if (r->used == garbage_state)
        std::cerr << "c_" << (void*)r << " [color=red];\n";
    if (auto* app = try_cast<application>(r)) {
        std::cerr << "c_" << (void*)app << " -> c_" << (void*)app->left << ";\n";
        std::cerr << "c_" << (void*)app << " -> c_" << (void*)app->right << " [color=blue];\n";
        graphviz_dump_impl(app->left, objs);
        graphviz_dump_impl(app->right, objs);
    } else if (auto* num = try_cast<number>(r)) {
        std::cerr << "c_" << (void*)num << " [label=\"" << num->value << "\"];\n";
    } else if (auto* fun = try_cast<function>(r)) {
        if (!func_names[fun->func])
            func_names[fun->func] = "???";
        std::cerr << "c_" << (void*)fun << " [label=\"" << func_names[fun->func] << "\"];\n";
    }
}

void graphviz_dump(graph g) {
    static int i = 0;
    std::set<object*> objs;
    std::cerr << "cat << EOF > g_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";
    graphviz_dump_impl(g.r, objs);
    std::cerr << "}\n";
    std::cerr << "EOF\n";
}

void multi_graphviz_dump(multi_graph g) {
    static int i = 0;
    std::set<object*> objs;
    std::cerr << "cat << EOF > mg_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";
    for (auto it = g.base; it != g.top; ++it) {
        auto i = it - g.base;
        auto p = *it;
        std::cerr << "s_" << i << " -> c_" << (void*)p << ";\n";
        graphviz_dump_impl(p, objs);
    }
    for (auto p = g.r; p ; p = p->next)
        graphviz_dump_impl(p, objs);
    std::cerr << "}\n";
    std::cerr << "EOF\n";
}
