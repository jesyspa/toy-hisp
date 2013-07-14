// Some builtin functions that can be used from within the language.

        .text
        .align 8
        .globl print
        .globl add

        .bss
// Reserved space for converting numbers to strings.
num_buffer:
        .space 16

        .text

// Print a single number.
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: value that was printed
print:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
print_real:
        popq %rdi
        movq 0x18(%rdi),%rdi
        movb 0x09(%rdi),%al
        cmpb $0,%al
        jne .skipeval
        call eval
        movq %rax,%rdi
.skipeval:

        movq %rdi,%r12
        // This had better be a number.
        movq 0x10(%rdi),%rax

        // Convert to string
        movq $10,%rcx
        movq $15,%rbx
        movq $10,num_buffer(%rbx)
.output_loop:
        decq %rbx
        movq $0,%rdx
        divq %rcx
        addb $48,%dl
        movb %dl,num_buffer(%rbx)
        cmpq $0,%rax
        jne .output_loop
.output_end:

        // Print string
        movq $1,%rax
        movq $1,%rdi
        movq %rbx,%rsi
        addq $num_buffer,%rsi
        movq $16,%rdx
        subq %rbx,%rdx
        syscall

        movq %r12,%rax
        jmp reenter_eval


// Add two numbers
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: sum of the numbers
add:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
add_real:
        popq %rdi
        movq 0x18(%rdi),%rdi
        movb 0x09(%rdi),%al
        cmpb $0,%al
        jne .skipeval_one
        call eval
        movq %rax,%rdi
.skipeval_one:
        movq 0x10(%rdi),%r12
        popq %rdi
        movq 0x18(%rdi),%rdi
        movb 0x09(%rdi),%al
        cmpb $0,%al
        jne .skipeval_two
        call eval
        movq %rax,%rdi
.skipeval_two:
        movq 0x10(%rdi),%rdi
        addq %r12,%rdi
        call mk_num
        jmp reenter_eval
