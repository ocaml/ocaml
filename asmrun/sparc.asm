/* Asm part of the runtime system for the Sparc processor.  */

        .common _young_start, 4, "data"
        .common _young_end, 4, "data"
        .common _young_ptr, 4, "data"
        .common _gc_entry_regs, 22 * 4, "data"
        .common _gc_entry_float_regs, 30 * 4, "data"
        .common _caml_top_of_stack, 4, "data"
        .common _caml_bottom_of_stack, 4, "data"
        .common _caml_last_return_address, 4, "data"
        .common _caml_exception_pointer, 4, "data"
        .common _caml_required_size, 4, "data"

/* libc functions appear to clobber %g2 ... %g7 */
/* Remember to save and restore %g5 %g6 %g7. */

#define Load(symb,reg)  sethi %hi(symb), %g1; ld [%g1 + %lo(symb)], reg
#define Store(reg,symb) sethi %hi(symb), %g1; st reg, [%g1 + %lo(symb)]

/* Allocation functions */

        .text
        .global _caml_alloc
        .global _caml_call_gc

/* Required size in %g4 */
_caml_alloc:
        sub     %g6, %g4, %g6
        cmp     %g6, %g7
        blu     _caml_call_gc
        nop
        retl
        nop

/* Required size in %g4 */
_caml_call_gc:
    /* Save %g4 (required size) */
        Store(%g4, _caml_required_size)
    /* Save %g5 (exception pointer) */
        Store(%g5, _caml_exception_pointer)
    /* Save current allocation pointer for debugging purposes */
        Store(%g6, _young_ptr)
    /* Record lowest stack address */
        Store(%sp, _caml_bottom_of_stack)
    /* Record last return address */
        Store(%o7, _caml_last_return_address)
    /* Save all regs used by the code generator */
        sethi   %hi(_gc_entry_regs), %g1
        or      %g1, %lo(_gc_entry_regs), %g1
        std     %l0, [%g1]
        std     %l2, [%g1 + 0x8]
        std     %l4, [%g1 + 0x10]
        std     %l6, [%g1 + 0x18]
        std     %o0, [%g1 + 0x20]
        std     %o2, [%g1 + 0x28]
        std     %o4, [%g1 + 0x30]
        std     %i0, [%g1 + 0x38]
        std     %i2, [%g1 + 0x40]
        std     %i4, [%g1 + 0x48]
        std     %g2, [%g1 + 0x50]
        sethi   %hi(_gc_entry_float_regs), %g1
        or      %g1, %lo(_gc_entry_float_regs), %g1
        std     %f0, [%g1]
        std     %f2, [%g1 + 0x8]
        std     %f4, [%g1 + 0x10]
        std     %f6, [%g1 + 0x18]
        std     %f8, [%g1 + 0x20]
        std     %f10, [%g1 + 0x28]
        std     %f12, [%g1 + 0x30]
        std     %f14, [%g1 + 0x38]
        std     %f16, [%g1 + 0x40]
        std     %f18, [%g1 + 0x48]
        std     %f20, [%g1 + 0x50]
        std     %f22, [%g1 + 0x58]
        std     %f24, [%g1 + 0x60]
        std     %f26, [%g1 + 0x68]
        std     %f28, [%g1 + 0x70]
    /* Call the garbage collector */
        call    _minor_collection
        nop
    /* Restore all regs used by the code generator */
        sethi   %hi(_gc_entry_regs), %g1
        or      %g1, %lo(_gc_entry_regs), %g1
        ldd     [%g1], %l0
        ldd     [%g1 + 0x8], %l2
        ldd     [%g1 + 0x10], %l4
        ldd     [%g1 + 0x18], %l6
        ldd     [%g1 + 0x20], %o0
        ldd     [%g1 + 0x28], %o2
        ldd     [%g1 + 0x30], %o4
        ldd     [%g1 + 0x38], %i0
        ldd     [%g1 + 0x40], %i2
        ldd     [%g1 + 0x48], %i4
        ldd     [%g1 + 0x50], %g2
        sethi   %hi(_gc_entry_float_regs), %g1
        or      %g1, %lo(_gc_entry_float_regs), %g1
        ldd     [%g1], %f0
        ldd     [%g1 + 0x8], %f2
        ldd     [%g1 + 0x10], %f4
        ldd     [%g1 + 0x18], %f6
        ldd     [%g1 + 0x20], %f8
        ldd     [%g1 + 0x28], %f10
        ldd     [%g1 + 0x30], %f12
        ldd     [%g1 + 0x38], %f14
        ldd     [%g1 + 0x40], %f16
        ldd     [%g1 + 0x48], %f18
        ldd     [%g1 + 0x50], %f20
        ldd     [%g1 + 0x58], %f22
        ldd     [%g1 + 0x60], %f24
        ldd     [%g1 + 0x68], %f26
        ldd     [%g1 + 0x70], %f28
    /* Reload %g5 - %g7 registers */
        Load(_caml_exception_pointer, %g5)
        Load(_young_ptr, %g6)
        Load(_young_start, %g7)
    /* Allocate space for block */
        Load(_caml_required_size, %g4)
        sub     %g6, %g4, %g6
    /* Return to caller */
        Load(_caml_last_return_address, %o7)
        retl
        nop

/* Call a C function from Caml */

        .global _caml_c_call
/* Function to call is in %g4 */
_caml_c_call:
    /* Record lowest stack address and return address */
        Store(%sp, _caml_bottom_of_stack)
        Store(%o7, _caml_last_return_address)
    /* Save the exception handler and alloc pointer */
        Store(%g5, _caml_exception_pointer)
        sethi   %hi(_young_ptr), %g1
    /* Call the C function */
        call    %g4
        st      %g6, [%g1 + %lo(_young_ptr)]            /* in delay slot */
    /* Reload return address */
        Load(_caml_last_return_address, %o7)
    /* Reload %g5 - %g7 */
        Load(_caml_exception_pointer, %g5)
        Load(_young_ptr, %g6)
        sethi   %hi(_young_start), %g1
    /* Return to caller */
        retl
        ld      [%g1 + %lo(_young_start)], %g7          /* in delay slot */

/* Start the Caml program */

        .global _caml_start_program
_caml_start_program:
    /* Save all callee-save registers */
        save    %sp, -96, %sp
    /* Build an exception handler */
        call    L100
        nop
        b       L101
        mov     %o0, %i0                /* return exn bucket */
L100:   sub     %sp, 8, %sp
        st      %o7, [%sp + 96]
        mov     %sp, %g5
    /* Record highest stack address */
        Store(%sp, _caml_top_of_stack)
    /* Initialize allocation registers */
        Load(_young_ptr, %g6)
        Load(_young_start, %g7)
    /* Go for it */
        call    _caml_program
        nop
    /* Pop handler */
        add     %sp, 8, %sp
    /* Return with zero code */
        mov     %g0, %i0
L101:   ret
        restore

/* Raise an exception from C */

        .global _raise_caml_exception
_raise_caml_exception:
    /* Reload %g5 - %g7 */
        Load(_caml_exception_pointer, %g5)
        Load(_young_ptr, %g6)
        Load(_young_start, %g7)
    /* Save exception bucket in a register outside the reg windows */
        mov     %o0, %g4
    /* Pop some frames until the trap pointer is in the current frame. */
	cmp     %g5, %fp
        blt     L107                    /* if Trap_handler_reg < %fp, over */
	nop
L106:   restore
        cmp     %fp, %g5                /* if %fp <= Trap_handler_reg, loop */
        ble     L106
        nop
L107:   mov     %g5, %sp
        ldd     [%sp+96], %g4
        add     %sp, 8, %sp
        jmp     %g4 + 8
    /* Restore bucket, in delay slot */
	mov     %g4, %o0
