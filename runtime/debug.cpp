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

void internal_print_impl(object const* r, std::vector<object const*>& objs, bool parens) {
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

void internal_print(object const* r) {
    std::vector<object const*> objs;
    internal_print_impl(r, objs, false);
}

void graphviz_dump_impl(object const* r, std::set<object const*>& objs) {
    ASSERT_SANITY(r);
    if (objs.count(r))
        return;
    objs.insert(r);
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
    std::set<object const*> objs;
    std::cerr << "cat << EOF > g_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";
    graphviz_dump_impl(g.r, objs);
    std::cerr << "}\n";
    std::cerr << "EOF\n";
}

void multi_graphviz_dump(multi_graph g) {
    static int i = 0;
    std::set<object const*> objs;
    std::cerr << "cat << EOF > mg_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";
    for (auto it = g.base; it != g.top; ++it) {
        auto i = it - g.base;
        auto p = *it;
        std::cerr << "s_" << i << " -> c_" << (void*)p << ";\n";
        graphviz_dump_impl(p, objs);
    }

    for (std::size_t i = 0; i < g.size;) {
        auto obj = reinterpret_cast<object const*>(g.space + i);
        graphviz_dump_impl(obj, objs);
        i += obj->size;
    }
    std::cerr << "}\n";
    std::cerr << "EOF\n";
}

void raw_dump(memory m) {
    std::cerr << "heap";
    for (std::size_t i = 0; i < m.size; ++i) {
        if (i % 8 == 0)
            std::cerr << '\n' << (void*)(m.space + i) << ": ";

        std::cerr << std::setfill('0') << std::setw(2) << std::hex << (unsigned int)(unsigned char)m.space[i];
        if (i % 8 != 7)
            std::cerr << ' ';
    }

    std::cerr << "\n\n";
    std::cerr << "stack\n";
    for (auto it = m.base; it != m.top; ++it)
        std::cerr << (void*)*it << '\n';

    std::cerr << '\n';
}
