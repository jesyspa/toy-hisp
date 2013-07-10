// Some builtin functions that can be used from within the language.

        .text
        .globl print
        .globl add

        // Print a single number.
        //
        // Takes:
        //  - 64 bit integer to be printed
        //
        // Returns:
        //  - Indication of garbage
print:
        ret


        // Add two numbers
        //
        // Takes:
        // - 64 bit integer
        // - 64 bit integer
        //
        // Returns:
        // - Sum of the above
        // - Indication of number
add:
        ret
