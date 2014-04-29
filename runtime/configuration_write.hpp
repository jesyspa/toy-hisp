#pragma once

#include <boost/program_options.hpp>

namespace configuration {
namespace options = boost::program_options;
void store(options::parsed_options const& parsed_options);
}
