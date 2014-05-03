#pragma once

class Stack;

/*! \brief Evaluate the expression expression on the stack.
 *
 *  The result is placed on the stack.  The expression is also overwritten with a forwarder to the result, ensuring that
 *  a second call to eval on the same expression will not reevaluate it.
 *
 *  \remark The stack should not contain any other objects.
 */
void eval(Stack stack);
