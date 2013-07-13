        .text
        .align 8
        .globl eval

// Evaluate the passed-in expression.
// IN:
//   rdi: expression to evaluate
// OUT:
//   rax: resulting object
eval:
        pushq %rbp
        movq %rsp,%rbp

        xor %rax,%rax

        // Construct a stack of the applications.
.stack:
        pushq %rdi
        movb 0x0E(%rdi),%al
        movq 0x10(%rdi),%rdi
        cmpb $0,%al
        jz .stack

        movq %rdi,%rax
        movq %rsp,%rdi
        call *%rax



        // For debugging purposes, this lets us forget we trashed the stack.
        movq %rbp,%rsp
        popq %rbp
        ret
