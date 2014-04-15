#pragma once

#include "object.hpp"
#include "main.hpp"
#include <map>

// Map of function addresses to their names, and another map back.
extern std::map<Func, char const*> func_names;
extern std::map<std::string, Func> funcs_by_name;

struct MemoryInfo {
    Ref root;
    char* space;
    std::size_t size;
};

// Create a file from which we'll later be able to initialize the program.
// Should only really be called from create_init_file, seeing as we only have
// the relevant info there.
//
// Currently mostly a stub to bootstrap the binary file option; we need a
// compiled file to test the loading code and we need loading code for such a
// dump to be much use.
void write_init_file(MemoryInfo memory);

// Read a file cerated with write_init_file.  Currently very fragile and mostly
// for testing purposes; we'll clean it up into something presentable later.
MemoryInfo read_init_file();
