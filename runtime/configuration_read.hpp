#pragma once

#include <string>

// TODO: Refactor this into something neater.
namespace configuration {
enum class MemoryDumpType { None, AsGraph };

MemoryDumpType get_current_memory_dump_type();

bool is_help_requested();

std::string get_input_file();
std::string get_output_dir();
}
