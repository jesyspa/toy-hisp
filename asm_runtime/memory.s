// Dynamic memory is necessary only for app objects.  These have a constant
// size, plus we'd like to have some space reserved for the header.
//
// Header format:
//
// 00-07  next object
// 08     alive
// 09-0D  reserved
// 0E     left type
// 0F     right type
// 10-17  left
// 18-1F  right
//

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

        // Initialises the memory system.
mem_init:
        movq $12,%rax
        movq $0,%rdi
        syscall
        movq %rax,mem_bottom
        movq %rax,mem_next
        ret

        // Creates a new object and returns a pointer to it.
        //
        // Takes: (nothing)
        //
        // Returns:
        //   - rax: Pointer to allocated memory
allocate:
        movq $12,%rax
        movq %rdi,%r15
        movq %rdx,%r13
        movq mem_next,%rdi
        movq %rdi,%r14
        addq $0x20,%rdi
        syscall
        movq %rax,mem_next
        movq %r15,%rdi
        movq %r14,%rax
        movq %r13,%rdx
        ret


        // Reclaim whatever memory can be reclaimed.
collect_garbage:
        ret
