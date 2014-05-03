#include "configuration/read.hpp"
#include "configuration/write.hpp"

namespace configuration {
namespace {
    // TODO: Cache the results instead of querying the map every time an option is requested.
    options::variables_map variables;
}

void store(options::parsed_options const& parsed_options) {
    options::store(parsed_options, variables);
    options::notify(variables);
}

MemoryDumpType get_current_memory_dump_type() {
    // TODO: Support other graph types.
    if (variables["dump-graph"].empty())
        return MemoryDumpType::None;
    return MemoryDumpType::AsGraph;
}

bool is_help_requested() { return !variables["help"].empty(); }

std::string get_input_file() { return variables["input-file"].as<std::string>(); }
std::string get_output_dir() { return variables["debug-output-dir"].as<std::string>(); }
}
