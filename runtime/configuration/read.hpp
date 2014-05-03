#pragma once

#include <string>

//! \brief Functions for working with the program's configuration.
namespace configuration {
//! \brief Type of memory dump to perform.
enum class MemoryDumpType { None, AsGraph };

//! \brief Return the current choice for dumps that are performed each step.
MemoryDumpType get_current_memory_dump_type();

//! \brief Return whether the user requested help in config options.
bool is_help_requested();

//! \brief Return the input file that we will be parsing.
std::string get_input_file();

//! \brief Return the directory to which we should write (debug) output.
std::string get_output_dir();
}
