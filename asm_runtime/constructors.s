// Functions for creating numbers and applications.

        .text
        .align 8
        .globl mk_app
        .globl mk_num

// Allocate and initialise an application object.
//
// IN:
//   rdi: left value
//   rsi: right value
// OUT:
//   rax: pointer to app
mk_app:
        pushq %rbp
        movq %rsp,%rbp
        pushq %rdi
        pushq %rsi

        movq $0x10,%rdi
        call allocate
        popq 0x18(%rax)
        popq 0x10(%rax)
        movb $0,0x09(%rax)

        popq %rbp
        ret

// Allocate and initialise a number object.
//
// IN:
//   rdi: value
// OUT:
//   rax: pointer to num
mk_num:
        pushq %rbp
        movq %rsp,%rbp
        pushq %rdi

        movq $0x08,%rdi
        call allocate
        popq 0x10(%rax)
        movb $1,0x09(%rax)

        popq %rbp
        ret
