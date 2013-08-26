// Some builtin functions that can be used from within the language.

        .text
        .align 8
        .globl print
        .globl add
        .globl sub
        .globl le
        .globl comb_s
        .globl comb_k
        .globl comb_i
        .globl comb_l
        .globl comb_r
        .globl comb_y

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

// Subtract the second object on the stack from the first
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: difference
sub:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
sub_real:
        hlt
        jmp reenter_eval


// Check whether the top of the stack is less than the second-top.
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: true or false
le:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
le_real:
        hlt
        jmp reenter_eval


// Apply the S combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_s:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_s_real:
        hlt
        jmp reenter_eval


// Apply the K combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_k:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_k_real:
        hlt
        jmp reenter_eval


// Apply the I combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_i:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_i_real:
        hlt
        jmp reenter_eval


// Apply the R combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_r:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_r_real:
        hlt
        jmp reenter_eval


// Apply the L combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_l:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_l_real:
        hlt
        jmp reenter_eval


// Apply the Y combinator
//
// MAY TRASH ALL REGS
// IN:
//   rsp: top of eval stack
// OUT:
//   rax: resulting expression
comb_y:
        .quad 0
        .byte 2
        .byte 2
        .word 0
        .long 0
comb_y_real:
        hlt
        jmp reenter_eval

