set(RUNTIME_SOURCE_FILES debug/debugger.cpp hisp/builtins.cpp main.cpp memory/space_iterators.cpp
    serialisation/serialised_hic.cpp debug/stream_dispenser.cpp hisp/eval.cpp memory/garbage_collection.cpp
    memory/stack.cpp debug/stream_ptr.cpp hisp/object.cpp memory/space.cpp serialisation/serialisation.cpp
    serialisation/binary_io.cpp)

include_directories(.)
add_executable(hisp ${RUNTIME_SOURCE_FILES})
target_link_libraries(hisp boost_program_options)
