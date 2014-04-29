#include "eval.hpp"
#include "configuration_write.hpp"
#include "configuration_read.hpp"
#include "garbage_collection.hpp"
#include "stack.hpp"
#include <boost/program_options.hpp>
#include <iostream>

namespace options = boost::program_options;

int main(int argc, char** argv) {
    options::options_description desc("Supported options");
    desc.add_options()                                                                            //
        ("help", "view this help message")                                                        //
        ("input-file", options::value<std::string>()->default_value("out.hic"), "program to run") //
        ("dump-graph", "produce heap graphs")                                                     //
        ("debug-output-dir", options::value<std::string>()->default_value("output"), "directory for debug outut");
    options::positional_options_description pos_desc;
    pos_desc.add("input-file", 1);

    options::command_line_parser parser(argc, argv);
    parser.options(desc);
    parser.positional(pos_desc);

    configuration::store(parser.run());

    if (configuration::is_help_requested()) {
        std::cout << "Usage: " << argv[0] << " input-file\n";
        std::cout << desc;
        return 0;
    }

    auto root = use_init_file(configuration::get_input_file());
    eval(root);
    collect_garbage();
    deinit_gc();
}
