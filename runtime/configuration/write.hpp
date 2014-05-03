#pragma once

#include <boost/program_options.hpp>

namespace configuration {
namespace options = boost::program_options;
//! \brief Append the given options to the active configuration.
void store(options::parsed_options const& parsed_options);
}
