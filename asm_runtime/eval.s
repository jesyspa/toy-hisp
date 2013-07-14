        .text
        .align 8
        .globl eval
        .globl reenter_eval

// Evaluate the passed-in expression.
// IN:
//   rdi: expression to evaluate
// OUT:
//   rax: resulting object
eval:
        pushq %rbp
        movq %rsp,%rbp
        pushq %rbx
        pushq %r12

        xor %rax,%rax

        // Construct a stack of the applications.
        jmp .check
.stack:
        pushq %rdi
        movq 0x10(%rdi),%rdi
.check:
        movb 0x09(%rdi),%al
        cmpb $0,%al
        jz .stack

        addq $0x10,%rdi
        jmp *%rdi

reenter_eval:
        

        popq %r12
        popq %rbx
        // For debugging purposes, this lets us forget we trashed the stack.
        movq %rbp,%rsp
        popq %rbp
        ret
