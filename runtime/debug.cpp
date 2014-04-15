#include "debug.hpp"
#include "utility.hpp"
#include "serialisation.hpp"
#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <map>
#include <set>
#include <vector>

void print_expression_impl(CRef obj, std::vector<CRef>& objs, bool parens) {
    ASSERT_SANITY(obj);
    if (auto* app = try_cast<Application>(obj)) {
        if (std::find(objs.begin(), objs.end(), obj) != objs.end()) {
            std::cerr << "<loop>";
            return;
        }
        objs.push_back(obj);
        if (parens)
            std::cerr << '(';
        print_expression_impl(app->left, objs, false);
        std::cerr << ' ';
        print_expression_impl(app->right, objs, true);
        if (parens)
            std::cerr << ')';
        objs.pop_back();
    } else if (auto* n = try_cast<Number>(obj)) {
        std::cerr << n->value;
    } else if (auto* f = try_cast<Function>(obj)){
        std::cerr << func_names[f->func];
    }
}

void print_expression(CRef root) {
    std::vector<CRef> objs;
    print_expression_impl(root, objs, false);
}

void graphviz_dump_impl(CRef obj, std::set<CRef>& objs) {
    ASSERT_SANITY(obj);
    if (objs.count(obj))
        return;
    objs.insert(obj);
    if (auto* app = try_cast<Application>(obj)) {
        std::cerr << "c_" << (void*)app << " -> c_" << (void*)app->left << ";\n";
        std::cerr << "c_" << (void*)app << " -> c_" << (void*)app->right << " [color=blue];\n";
        graphviz_dump_impl(app->left, objs);
        graphviz_dump_impl(app->right, objs);
    } else if (auto* num = try_cast<Number>(obj)) {
        std::cerr << "c_" << (void*)num << " [label=\"" << num->value << "\"];\n";
    } else if (auto* fun = try_cast<Function>(obj)) {
        if (!func_names[fun->func])
            func_names[fun->func] = "???";
        std::cerr << "c_" << (void*)fun << " [label=\"" << func_names[fun->func] << "\"];\n";
    }
}

void graphviz_dump(GraphBag graph) {
    static int i = 0;
    std::set<CRef> objs;
    std::cerr << "cat << EOF > g_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";

    graphviz_dump_impl(graph.root, objs);

    std::cerr << "}\n";
    std::cerr << "EOF\n";
}

void multi_graphviz_dump(MultiGraphBag graph) {
    static int i = 0;
    std::set<CRef> objs;
    std::cerr << "cat << EOF > mg_" << std::setw(4) << std::setfill('0') << i++ << ".dot\n";
    std::cerr << "digraph {\n";

    int root_count = 0;
    for (auto root : graph.stack) {
        std::cerr << "s_" << root_count << " -> c_" << (void*)root << ";\n";
        graphviz_dump_impl(root, objs);
        ++root_count;
    }

    for (std::size_t i = 0; i < graph.size;) {
        auto obj = reinterpret_cast<CRef>(graph.space + i);
        graphviz_dump_impl(obj, objs);
        i += obj->size;
    }

    std::cerr << "}\n";
    std::cerr << "EOF\n";
}

void raw_dump(MemoryBag memory) {
    std::cerr << "heap";
    for (std::size_t i = 0; i < memory.size; ++i) {
        if (i % 8 == 0)
            std::cerr << '\n' << (void*)(memory.space + i) << ": ";

        unsigned val = static_cast<unsigned char>(memory.space[i]);
        std::cerr << std::setfill('0') << std::setw(2) << std::hex << val;
        if (i % 8 != 7)
            std::cerr << ' ';
    }

    std::cerr << "\n\n";
    std::cerr << "stack\n";
    for (auto obj : memory.stack)
        std::cerr << (void*)obj << '\n';

    std::cerr << '\n';
}


