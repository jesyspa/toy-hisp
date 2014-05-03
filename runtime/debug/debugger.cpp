#include "debug/debugger.hpp"
#include "configuration/read.hpp"
#include "memory/garbage_collection.hpp"
#include "macros.hpp"
#include "memory/serialisation.hpp"
#include "memory/stack.hpp"
#include "hisp/utility.hpp"
#include <algorithm>
#include <iostream>
#include <set>
#include <vector>

namespace {
void print_expression_impl(std::ostream& out, CRef obj, std::vector<CRef>& objs, bool parens) {
    ASSERT_SANITY(obj);
    if (auto app = try_cast<Application>(obj)) {
        if (std::find(objs.begin(), objs.end(), obj) != objs.end()) {
            out << "<loop>";
            return;
        }
        objs.push_back(obj);
        if (parens)
            out << '(';
        print_expression_impl(out, app->left, objs, false);
        out << ' ';
        print_expression_impl(out, app->right, objs, true);
        if (parens)
            out << ')';
        objs.pop_back();
    } else if (auto n = try_cast<Number>(obj)) {
        out << n->value;
    } else if (auto f = try_cast<Function>(obj)) {
        out << func_names[f->func];
    } else if (auto fwd = try_cast<Forwarder>(obj)) {
        out << '#';
        print_expression_impl(out, fwd->target, objs, true);
    }
}

void print_expression(std::ostream& out, CRef root) {
    std::vector<CRef> objs;
    print_expression_impl(out, root, objs, false);
}

void graphviz_dump_impl(std::ostream& out, CRef obj, std::set<CRef>& objs) {
    ASSERT_SANITY(obj);
    if (objs.count(obj))
        return;
    objs.insert(obj);
    if (auto app = try_cast<Application>(obj)) {
        out << "c_" << (void*)app << " -> c_" << (void*)app->left << ";\n";
        out << "c_" << (void*)app << " -> c_" << (void*)app->right << " [color=blue];\n";
        graphviz_dump_impl(out, app->left, objs);
        graphviz_dump_impl(out, app->right, objs);
    } else if (auto num = try_cast<Number>(obj)) {
        out << "c_" << (void*)num << " [label=\"" << num->value << "\"];\n";
    } else if (auto fun = try_cast<Function>(obj)) {
        if (!func_names[fun->func])
            func_names[fun->func] = "???";
        out << "c_" << (void*)fun << " [label=\"" << func_names[fun->func] << "\"];\n";
    } else if (auto fwd = try_cast<Forwarder>(obj)) {
        out << "c_" << (void*)fwd << " [label=\"f_" << (void*)fwd << "\"];\n";
        out << "c_" << (void*)fwd << " -> c_" << (void*)fwd->target << ";\n";
        graphviz_dump_impl(out, fwd->target, objs);
    }
}

void graphviz_dump(std::ostream& out, CRef root) {
    std::set<CRef> objs;

    out << "digraph {\n";
    graphviz_dump_impl(out, root, objs);
    out << "}\n";
}

void multi_graphviz_dump(std::ostream& out, StackStorage const& stack, Space const& space) {
    std::set<CRef> objs;

    out << "digraph {\n";

    int root_count = 0;
    for (auto root : stack) {
        out << "s_" << root_count << " -> c_" << (void*)root << ";\n";
        graphviz_dump_impl(out, root, objs);
        ++root_count;
    }

    for (auto& obj : space)
        graphviz_dump_impl(out, &obj, objs);

    out << "}\n";
}

void raw_dump(std::ostream& out, StackStorage const& stack, Space const& space) {
    out << "heap";
    space.print_hexdump(out);

    out << "\n";
    out << "stack\n";
    for (auto& obj : stack)
        out << (void*)&obj << '\n';

    out << '\n';
}
}

void Debugger::DebuggerImpl::dump_graph_beneath(CRef r) {
    auto stream = graph_streams.get_next();
    graphviz_dump(*stream, r);
}

void Debugger::DebuggerImpl::dump_memory_as_graph() {
    auto stream = graph_streams.get_next();
    auto info = get_debug_memory_info();
    multi_graphviz_dump(*stream, *info.stack, *info.space);
}

void Debugger::DebuggerImpl::dump_memory_as_array() {
    auto stream = array_streams.get_next();
    auto info = get_debug_memory_info();
    raw_dump(*stream, *info.stack, *info.space);
}

void Debugger::DebuggerImpl::print_expression(CRef r) {
    auto stream = take_pointer(std::cerr);
    // TODO: Rename this to not require explicit qualification.
    ::print_expression(*stream, r);
    *stream << std::endl;
}

void Debugger::DebuggerImpl::step() {
    switch (configuration::get_current_memory_dump_type()) {
    case configuration::MemoryDumpType::None:
        return;
    case configuration::MemoryDumpType::AsGraph:
        dump_memory_as_graph();
        return;
    }
}
