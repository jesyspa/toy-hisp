#pragma once
/*! \file
 *  \brief Declarations of built-in Hisp functions.
 *
 *  All functions accept their arguments and return their results on the Stack they are passed.
 */

class Stack;

//! \brief Identity combinator.  I x = x
void comb_i(Stack stack);

//! \brief Constant combinator.  K x y = x
void comb_k(Stack stack);

//! \brief Sequence combinator.  S f g x = f x (g x)
void comb_s(Stack stack);

//! \brief Left combinator.  L f g x = f x g
void comb_l(Stack stack);

//! \brief Right combinator.  R f g x = f (g x)
void comb_r(Stack stack);

//! \brief Y combinator.  Y f = f (Y f)
void comb_y(Stack stack);

//! \brief Print number and return it.
void print(Stack stack);

//! \brief Add two numbers and return the result.
void add(Stack stack);

//! \brief Subtract the second argument from the first and return.
void sub(Stack stack);

//! \brief Return whether the first argument is less than or equal to the second.
void le(Stack stack);
