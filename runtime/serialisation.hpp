#pragma once

/*! \file
 *  \brief Tools for serializing and deserializing the program.
 */

#include "object.hpp"
#include "space.hpp"
#include <map>
#include <string>

//! \brief Map of function pointers to their names.
extern std::map<Func, char const*> func_names;
//! \brief Map of function names to pointers to their implementations.
extern std::map<std::string, Func> funcs_by_name;

//! \brief Initialization info for a new evaluation execution.
struct ProgramInitInfo {
    Ref root;
    Space space;
};

/*! \brief Create an initialization file that we'll be able to load later.
 *
 *  Assumes that no evaluation is occuring at the moment.
 *
 *  \deprecated This was written to be able to better test read_init_file.  There's not much use for it outside that,
 *  seeing as it cannot accurately capture the state of a running program.
 */
void write_init_file(CRef root, Space const& space);

//! \brief Read program initialization info from a file.
ProgramInitInfo read_init_file(std::string name);
