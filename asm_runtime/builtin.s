# Some builtin functions that can be used from within the language.

        .text
        .globl print
        .globl add

        .bss
# Reserved space for converting numbers to strings.
num_buffer:
        .space 16

        .text

# Print a single number.
#
# IN:
#   rdi: top of the stack
# OUT: Nothing
print:
        pushq %rbp
        movq %rsp,%rbp
        pushq %rbx

        movq (%rdi),%rdi
        movq 0x18(%rdi),%rdi
        call eval

        # Convert to string
        movq    $10,%rcx
        movq    $15,%rbx
        movq    $10,num_buffer(%rbx)
.output_loop:
        decq    %rbx
        movq    $0,%rdx
        divq    %rcx
        addb    $48,%dl
        movb    %dl,num_buffer(%rbx)
        cmpq    $0,%rax
        jne     .output_loop
.output_end:

        # Print string
        movq    $1,%rax
        movq    $1,%rdi
        movq    %rbx,%rsi
        addq    $num_buffer,%rsi
        movq    $16,%rdx
        subq    %rbx,%rdx
        syscall


        popq %rbx
        movq %rbp,%rsp
        popq %rbp
        ret


# Add two numbers
#
# IN:
#   rdi: top of the stack
# OUT:
#   rax: sum of the numbers
add:
        ret
