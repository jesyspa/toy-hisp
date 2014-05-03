<!-- vim: set tw=80: -->
# Hisp

A toy compiler for and learning about functional language implementation.  Oh,
and Haskell.

The language is, roughly speaking, the lambda calculus with a few built-in
functions and support for numbers.  The output of the compiler is currently a
snapshot of the initial program heap; the runtime then performs graph-reduction
on this heap until it reaches an answer.

## Plans
- Support for multiple functions
- Support for types!
- Some sort of bytecode to compile to
- Self-hosting

## Installation notes
- I use a truckload of GHC extensions, including Template Haskell, so I doubt
  this'll work on any other compilers.
- C++ compilation is done with Scons and relies heavily on C++11.  GCC 4.8.2
  should be enough.

