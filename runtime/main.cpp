#include "hisp/eval.hpp"
#include "debug/debugger.hpp"
#include "memory/garbage_collection.hpp"
#include "memory/stack.hpp"
#include <boost/program_options.hpp>
#include <iostream>
#include <exception>

namespace options = boost::program_options;

namespace {
options::options_description make_options_description() {
    options::options_description desc("Supported options");
    desc.add_options()                                                                            //
        ("help", "view this help message")                                                        //
        ("input-file", options::value<std::string>()->default_value("out.hic"), "program to run") //
        ("dump-graph", "produce heap graphs (debug only)")                                        //
        ("debug-output-dir", options::value<std::string>()->default_value("output"), "directory for debug outut");
    return desc;
}

options::positional_options_description make_positional_options_description() {
    options::positional_options_description pos_desc;
    pos_desc.add("input-file", 1);
    return pos_desc;
}
}

int main(int argc, char** argv) try {
    auto desc = make_options_description();
    auto pos_desc = make_positional_options_description();

    options::command_line_parser parser(argc, argv);
    parser.options(desc);
    parser.positional(pos_desc);

    options::variables_map variables;
    options::store(parser.run(), variables);
    options::notify(variables);

    if (variables.count("help")) {
        std::cout << "Usage: " << argv[0] << " input-file\n";
        std::cout << desc;
        return 0;
    }

    Debugger::set_output_dir(variables["debug-output-dir"].as<std::string>());
    if (variables.count("dump-graph"))
        Debugger::set_dump_type(MemoryDumpType::AsGraph);

    auto root = use_init_file(variables["input-file"].as<std::string>());
    create_init_file();
    eval(root);
    collect_garbage();
    deinit_gc();
}
catch (std::exception& e) {
    std::cerr << "error: " << e.what() << std::endl;
    return -1;
}
catch (...) {
    std::cerr << "unknown fatal error" << std::endl;
    return -2;
}
