// Various functions for getting current program state.

        .text
        .align 8
        .globl dbg_print_tree

        .bss
dbg_print_tree_buffer:
        .space 128

        .text

// Print a small tree.  Passing big trees is undefined behaviour.
//
// IN:
//    rdi: object with header
// OUT: Nothing
dbg_print_tree:
        pushq %rbp
        movq %rsp,%rbp

        movq %rdi,%rsi
        movq $dbg_print_tree_buffer,%rdi
        xorq %rax,%rax

        call dbg_print_tree_impl
        movb $'\n',(%rdi)
        incq %rdi

        movq $1,%rax
        movq $dbg_print_tree_buffer,%rsi
        movq %rdi,%rdx
        subq %rsi,%rdx
        movq $1,%rdi
        syscall

        popq %rbp
        ret


// Recursive part of dbg_print_tree.
//
// IN:
//   rdi: next address to write to
//   rsi: object with header
// OUT:
//   rdi: one past last address written to
dbg_print_tree_impl:
        movb 0x09(%rsi),%al
        cmpb $0,%al
        je .app
        cmpb $1,%al
        je .num
        cmpb $2,%al
        je .fun

.app:
        movb $'(',(%rdi)
        incq %rdi
        pushq %rsi
        movq 0x10(%rsi),%rsi
        call dbg_print_tree_impl
        popq %rsi
        movb $' ',(%rdi)
        incq %rdi
        movq 0x18(%rsi),%rsi
        call dbg_print_tree_impl
        movb $')',(%rdi)
        incq %rdi
        ret
.num:
        movb $'i',(%rdi)
        incq %rdi
        movq 0x10(%rsi),%rax
        call dbg_print_int_as_hex
        ret
.fun:
        movb $'f',(%rdi)
        incq %rdi
        movq %rsi,%rax
        call dbg_print_int_as_hex
        ret


// Write an int in hex.  Backwards.
//
// IN:
//  rdi: next address to write to
//  rax: object to write
dbg_print_int_as_hex:
        cmp $0,%rax
        jz .end
        movb %al,%dl
        andb $0xF,%dl
        shrq $4,%rax
        cmpb $10,%dl
        jl .underten
        add $'8',%dl
        jmp .print
.underten:
        add $'0',%dl
.print:
        movb %dl,(%rdi)
        incq %rdi
        jmp dbg_print_int_as_hex
.end:   ret
