// Dynamic memory is necessary only for app objects.  These have a constant
// size, plus we'd like to have some space reserved for the header.
//
// Header format:
//
// 00-07  next object
// 08     alive
// 09     type
// 0A     size
// 0B-0F  reserved
//
// Application:
// 10-17  left
// 18-1F  right
//
// Number:
// 10-17  value
//
// Type values:
// 0:  App
// 1:  Number
// 2:  Function
//
        .text
        .align 8
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
// I: Nothing
// O: Nothing
mem_init:
        movq $12,%rax
        movq $0,%rdi
        syscall
        movq %rax,mem_bottom
        movq %rax,mem_next
        ret

// Create a new object and return a pointer to it.
//
// I:
//   rdi: Size to allocate.
// O:
//   rax: Pointer to allocated memory
allocate:
        pushq %r12

        movq $12,%rax
        movq %rdi,%r12
        addq mem_next,%rdi
        // Add header size.
        addq $0x10,%rdi
        syscall
        movq mem_next,%rdx
        movq %rax,mem_next
        movq %rdx,%rax
        movq %r12,%rdx
        movb %dl,0x0A(%rax)
        movb $1,0x08(%rax)

        popq %r12
        ret


// Reclaim whatever memory can be reclaimed.
// I: Nothing
// O: Nothing
// T: All
collect_garbage:
        ret
