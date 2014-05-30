#include "debug/debugger.hpp"
#include "memory/garbage_collection.hpp"
#include "macros.hpp"
#include "serialisation/serialisation.hpp"
#include "memory/stack.hpp"
#include "hisp/utility.hpp"
#include "meta/apply.hpp"
#include <algorithm>
#include <iostream>
#include <set>
#include <vector>

namespace {

template <typename T>
struct ExpressionPrinter;

void print_expression_impl(Ref obj, std::ostream& out, std::vector<CRef>& objs, bool parens) {
    ASSERT_SANITY(obj);
    if (std::find(objs.begin(), objs.end(), obj) != objs.end()) {
        out << "<loop>";
        return;
    }
    objs.push_back(obj);
    RuntimeApply<ExpressionPrinter>(obj, out, objs, parens);
    objs.pop_back();
}

void print_expression(std::ostream& out, Ref root) {
    std::vector<CRef> objs;
    print_expression_impl(root, out, objs, false);
}

template <>
struct ExpressionPrinter<Application> {
    static void execute(Application* app, std::ostream& out, std::vector<CRef>& objs, bool parens) {
        if (parens)
            out << '(';
        print_expression_impl(app->left, out, objs, false);
        out << ' ';
        print_expression_impl(app->right, out, objs, true);
        if (parens)
            out << ')';
    }
};

template <>
struct ExpressionPrinter<Number> {
    static void execute(Number* num, std::ostream& out, std::vector<CRef>&, bool) { out << num->value; }
};

template <>
struct ExpressionPrinter<Function> {
    static void execute(Function* fun, std::ostream& out, std::vector<CRef>&, bool) { out << func_names.at(fun->func); }
};

template <>
struct ExpressionPrinter<Forwarder> {
    static void execute(Forwarder* fwd, std::ostream& out, std::vector<CRef>& objs, bool) {
        out << '#';
        print_expression_impl(fwd->target, out, objs, true);
    }
};

template <typename T>
struct GraphPrinter;

void graphviz_dump_impl(Ref obj, std::ostream& out, std::set<CRef>& objs) {
    ASSERT_SANITY(obj);
    if (objs.count(obj))
        return;
    objs.insert(obj);
    RuntimeApply<GraphPrinter>(obj, out, objs);
}
template <>
struct GraphPrinter<Application> {
    static void execute(Application* app, std::ostream& out, std::set<CRef>& objs) {
        out << "c_" << (void*)app << " -> c_" << (void*)app->left << ";\n";
        out << "c_" << (void*)app << " -> c_" << (void*)app->right << " [color=blue];\n";
        graphviz_dump_impl(app->left, out, objs);
        graphviz_dump_impl(app->right, out, objs);
    }
};

template <>
struct GraphPrinter<Number> {
    static void execute(Number* num, std::ostream& out, std::set<CRef>&) {
        out << "c_" << (void*)num << " [label=\"" << num->value << "\"];\n";
    }
};

template <>
struct GraphPrinter<Function> {
    static void execute(Function* fun, std::ostream& out, std::set<CRef>&) {
        if (!func_names[fun->func])
            func_names[fun->func] = "???";
        out << "c_" << (void*)fun << " [label=\"" << func_names[fun->func] << "\"];\n";
    }
};

template <>
struct GraphPrinter<Forwarder> {
    static void execute(Forwarder* fwd, std::ostream& out, std::set<CRef>& objs) {
        out << "c_" << (void*)fwd << " [label=\"f_" << (void*)fwd << "\"];\n";
        out << "c_" << (void*)fwd << " -> c_" << (void*)fwd->target << ";\n";
        graphviz_dump_impl(fwd->target, out, objs);
    }
};

void graphviz_dump(std::ostream& out, Ref root) {
    std::set<CRef> objs;

    out << "digraph {\n";
    graphviz_dump_impl(root, out, objs);
    out << "}\n";
}

void multi_graphviz_dump(std::ostream& out, StackStorage const& stack, Space const& space) {
    std::set<CRef> objs;

    out << "digraph {\n";

    int root_count = 0;
    for (auto root : stack) {
        out << "s_" << root_count << " -> c_" << (void*)root << ";\n";
        graphviz_dump_impl(root, out, objs);
        ++root_count;
    }

    for (auto& obj : space)
        graphviz_dump_impl(&obj, out, objs);

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

void Debugger::DebuggerImpl::dump_graph_beneath(Ref r) {
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

void Debugger::DebuggerImpl::print_expression(Ref r) {
    auto stream = take_pointer(std::cerr);
    // TODO: Rename this to not require explicit qualification.
    ::print_expression(*stream, r);
    *stream << std::endl;
}

void Debugger::DebuggerImpl::step() {
    switch (dump_type) {
    case MemoryDumpType::None:
        return;
    case MemoryDumpType::AsGraph:
        dump_memory_as_graph();
        return;
    }
}

void Debugger::DebuggerImpl::set_output_dir(std::string const& dir) {
    array_streams.set_output_dir(dir);
    graph_streams.set_output_dir(dir);
}
