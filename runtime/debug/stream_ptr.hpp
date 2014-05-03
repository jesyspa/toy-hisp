#pragma once

#include <ostream>
#include <memory>

/*! \brief Encapsulates a pointer to a stream.
 *
 *  Guaranteed to point to a valid stream, and will clean up after itself.
 */
class OStreamPtr {
    using deleter_func = void(std::ostream*);
    std::unique_ptr<std::ostream, deleter_func*> stream_;

    OStreamPtr() = delete;
    OStreamPtr(std::ostream* stream, deleter_func* deleter)
        : stream_(stream, deleter) {}

public:
    //! \brief Get a reference to the stream.
    std::ostream& operator*() const { return *stream_; }

    //! \brief Get a pointer to the stream.
    std::ostream* operator->() const { return stream_.get(); }

    //! \brief Swap two stream pointers, so that they each refer to the other's stream.
    void swap(OStreamPtr& other) { stream_.swap(other.stream_); }

    friend OStreamPtr open_file(std::string filename);

    friend OStreamPtr take_pointer(std::ostream& os);
};

//! \brief Open an output stream to the given file and return a pointer to it.
OStreamPtr open_file(std::string filename);
/*! \brief Create a pointer to the given (existing) stream.
 *
 *  \todo Come up with a better name for this.
 */
OStreamPtr take_pointer(std::ostream& os);
