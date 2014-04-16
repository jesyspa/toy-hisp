#pragma once

class SubStack;

// Evaluate the expression at the top of stack_ref, returning the result on the
// same stack.  We assume the expression is the only thing currently on the
// stack.
void eval(SubStack stack);

