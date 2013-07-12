        .text
        .global _start

_start:
        # We need to do the following:
        #  - Set up heap allocation and garbage collection.
        #  - Call make_expr to construct the expression we'll be evaluating.
        #    * Note that make_expr is a C function/ need to check how exactly
        #      that's linked to properly call it.
        #  - Tell the garbage collection code that we'll be calling eval soon.
        #    * We'll probably just need to set a pointer to where the stack
        #      will start.
        #  - Call eval on the object we got.
        #  - Discard the result.
        #  - Clean up the heap.
        call mem_init
        call make_expr
        movq %rax,%rdi
        call eval
        jmp exit


exit:
        movq $60,%rax
        movq $0,%rdi
        syscall
