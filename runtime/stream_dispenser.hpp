#pragma once

#include "stream_ptr.hpp"
#include <string>

//! \brief Utility class for getting streams on-demand.
class StreamDispenser {
    // TODO: Allow for multiple implementations.
    // Can currently only create files of the form basename_num.ext
    int next_file_number_ = 0;
    std::string base_name_;
    std::string extension_;

public:
    // We allow moving, but not copying, to prevent accidantally overwriting files.
    // This isn't a fool-proof way, but it should save us some trouble.
    StreamDispenser(StreamDispenser const&) = delete;
    StreamDispenser& operator=(StreamDispenser const&) = delete;
    StreamDispenser(StreamDispenser&&) = default;
    StreamDispenser& operator=(StreamDispenser&&) = default;

    //! \brief Create a dispenser that will create files with the given base name and extension.
    StreamDispenser(std::string base_name, std::string extension);

    //! \brief Get a pointer to a new stream.
    OStreamPtr get_next();
};
