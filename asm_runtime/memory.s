# Dynamic memory is necessary only for app objects.  These have a constant
# size, plus we'd like to have some space reserved for the header.
#
# Header format:
#
# 00-07  next object
# 08     alive
# 09-0D  reserved
# 0E     left type
# 0F     right type
# 10-17  left
# 18-1F  right
#


        .text
        .global mem_init
        .global allocate
        .global collect_garbage

        .data
mem_bottom:
        .space 8
mem_next:
        .space 8


        .text

# Initialises the memory system.
# I: Nothing
# O: Nothing
# T: All
mem_init:
        movq $12,%rax
        movq $0,%rdi
        syscall
        movq %rax,mem_bottom
        movq %rax,mem_next
        ret

# Create a new object and return a pointer to it.
#
# I: Nothing
# O:
#   rax: Pointer to allocated memory
# T: All
allocate:
        pushq %rdx
        pushq %rdi
        pushq %r14

        movq $12,%rax
        movq mem_next,%rdi
        movq %rdi,%r14
        addq $0x20,%rdi
        syscall
        movq %rax,mem_next
        movq %r14,%rax

        popq %r14
        popq %rdi
        popq %rdx
        ret


# Reclaim whatever memory can be reclaimed.
# I: Nothing
# O: Nothing
# T: All
collect_garbage:
        ret
