        // All of these are meant to be called from C.  They should set some
        // register to indicate what type we're working on and then jump to
        // mk_app, which will do the actual work based on that.
        .text
        .globl mk_app_aa
        .globl mk_app_af
        .globl mk_app_an
        .globl mk_app_fa
        .globl mk_app_ff
        .globl mk_app_fn
        .globl mk_app_na
        .globl mk_app_nf
        .globl mk_app_nn

mk_app_aa:
        movq $0x000,%rdx
        jmp mk_app
mk_app_af:
        movq $0x001,%rdx
        jmp mk_app
mk_app_an:
        movq $0x002,%rdx
        jmp mk_app
mk_app_fa:
        movq $0x100,%rdx
        jmp mk_app
mk_app_ff:
        movq $0x101,%rdx
        jmp mk_app
mk_app_fn:
        movq $0x102,%rdx
        jmp mk_app
mk_app_na:
        movq $0x200,%rdx
        jmp mk_app
mk_app_nf:
        movq $0x201,%rdx
        jmp mk_app
mk_app_nn:
        movq $0x202,%rdx
        jmp mk_app

        // Allocates and initialises an application object.
        //
        // Takes:
        //  - rdi: right value
        //  - rsi: right value
        //  - dh : left type
        //  - dl : right type
        //
        // Returns:
        //  - Pointer to the new app
mk_app:
        call allocate
        movw %dx,0x0e(%rax)
        movq %rdi,0x10(%rax)
        movq %rsi,0x18(%rax)
        ret
