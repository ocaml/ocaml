/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system for the Sparc processor.  */
/* Must be preprocessed by cpp */

/* SunOS 4 and BSD prefix identifiers with _, Solaris does not */

#ifndef SYS_solaris

        .common _young_limit, 4, "bss"
        .common _young_ptr, 4, "bss"
        .common _gc_entry_regs, 22 * 4, "bss"
        .common _gc_entry_float_regs, 30 * 4, "bss"
        .common _caml_top_of_stack, 4, "bss"
        .common _caml_bottom_of_stack, 4, "bss"
        .common _caml_last_return_address, 4, "bss"
        .common _caml_exception_pointer, 4, "bss"
        .common _caml_required_size, 4, "bss"

#define Young_limit _young_limit
#define Young_ptr _young_ptr
#define Gc_entry_regs _gc_entry_regs
#define Gc_entry_float_regs _gc_entry_float_regs
#define Caml_top_of_stack _caml_top_of_stack
#define Caml_bottom_of_stack _caml_bottom_of_stack
#define Caml_last_return_address _caml_last_return_address
#define Caml_exception_pointer _caml_exception_pointer
#define Caml_required_size _caml_required_size
#define Caml_alloc _caml_alloc
#define Caml_call_gc _caml_call_gc
#define Garbage_collection _garbage_collection
#define Caml_c_call _caml_c_call
#define Caml_start_program _caml_start_program
#define Caml_program _caml_program
#define Raise_caml_exception _raise_caml_exception
#define Callback _callback
#define Callback2 _callback2
#define Callback3 _callback3
#define Caml_apply2 _caml_apply2
#define Caml_apply3 _caml_apply3
#define Mlraise _mlraise
#define System_frametable _system_frametable

#else

        .common young_limit, 4, 4
        .common young_end, 4, 4
        .common young_ptr, 4, 4
        .common gc_entry_regs, 22 * 4, 8
        .common gc_entry_float_regs, 30 * 4, 8
        .common caml_top_of_stack, 4, 4
        .common caml_bottom_of_stack, 4, 4
        .common caml_last_return_address, 4, 4
        .common caml_exception_pointer, 4, 4
        .common caml_required_size, 4, 4

#define Young_limit young_limit
#define Young_ptr young_ptr
#define Gc_entry_regs gc_entry_regs
#define Gc_entry_float_regs gc_entry_float_regs
#define Caml_top_of_stack caml_top_of_stack
#define Caml_bottom_of_stack caml_bottom_of_stack
#define Caml_last_return_address caml_last_return_address
#define Caml_exception_pointer caml_exception_pointer
#define Caml_required_size caml_required_size
#define Caml_alloc caml_alloc
#define Caml_call_gc caml_call_gc
#define Garbage_collection garbage_collection
#define Caml_c_call caml_c_call
#define Caml_start_program caml_start_program
#define Caml_program caml_program
#define Raise_caml_exception raise_caml_exception
#define Callback callback
#define Callback2 callback2
#define Callback3 callback3
#define Caml_apply2 caml_apply2
#define Caml_apply3 caml_apply3
#define Mlraise mlraise
#define System_frametable system_frametable

#endif

/* libc functions appear to clobber %g2 ... %g7 */
/* Remember to save and restore %g5 %g6 %g7. */

#define Load(symb,reg)  sethi %hi(symb), %g1; ld [%g1 + %lo(symb)], reg
#define Store(reg,symb) sethi %hi(symb), %g1; st reg, [%g1 + %lo(symb)]
#define Address(symb,reg) sethi %hi(symb), reg; or reg, %lo(symb), reg

/* Allocation functions */

        .text
        .global Caml_alloc
        .global Caml_call_gc

/* Required size in %g4 */
Caml_alloc:
	ld	[%g7], %g1
        sub     %g6, %g4, %g6
        cmp     %g6, %g1
        blu     Caml_call_gc
        nop
        retl
        nop

/* Required size in %g4 */
Caml_call_gc:
    /* Save %g4 (required size) */
        Store(%g4, Caml_required_size)
    /* Save %g5 (exception pointer) */
        Store(%g5, Caml_exception_pointer)
    /* Save current allocation pointer for debugging purposes */
        Store(%g6, Young_ptr)
    /* Record lowest stack address */
        Store(%sp, Caml_bottom_of_stack)
    /* Record last return address */
        Store(%o7, Caml_last_return_address)
    /* Save all regs used by the code generator */
        Address(Gc_entry_regs, %g1)
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
        Address(Gc_entry_float_regs, %g1)
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
        call    Garbage_collection
        nop
    /* Restore all regs used by the code generator */
        Address(Gc_entry_regs, %g1)
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
        Address(Gc_entry_float_regs, %g1)
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
        Load(Caml_exception_pointer, %g5)
        Load(Young_ptr, %g6)
        Address(Young_limit, %g7)
    /* Allocate space for block */
        Load(Caml_required_size, %g4)
        sub     %g6, %g4, %g6
    /* Return to caller */
        Load(Caml_last_return_address, %o7)
        retl
        nop

/* Call a C function from Caml */

        .global Caml_c_call
/* Function to call is in %g4 */
Caml_c_call:
    /* Record lowest stack address and return address */
        Store(%sp, Caml_bottom_of_stack)
        Store(%o7, Caml_last_return_address)
    /* Save the exception handler and alloc pointer */
        Store(%g5, Caml_exception_pointer)
        sethi   %hi(Young_ptr), %g1
    /* Call the C function */
        call    %g4
        st      %g6, [%g1 + %lo(Young_ptr)]        /* in delay slot */
    /* Reload return address */
        Load(Caml_last_return_address, %o7)
    /* Reload %g5 - %g7 */
        Load(Caml_exception_pointer, %g5)
        Load(Young_ptr, %g6)
        sethi   %hi(Young_limit), %g7
    /* Return to caller */
        retl
        or      %g7, %lo(Young_limit), %g7      /* in delay slot */

/* Start the Caml program */

        .global Caml_start_program
Caml_start_program:
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
        Store(%sp, Caml_top_of_stack)
    /* Initialize allocation registers */
        Load(Young_ptr, %g6)
        Address(Young_limit, %g7)
    /* Go for it */
        call    Caml_program
        nop
    /* Pop handler */
        add     %sp, 8, %sp
    /* Return with zero code */
        mov     %g0, %i0
L101:   ret
        restore

/* Raise an exception from C */

        .global Raise_caml_exception
Raise_caml_exception:
    /* Reload %g5 - %g7 */
        Load(Caml_exception_pointer, %g5)
        Load(Young_ptr, %g6)
        Address(Young_limit, %g7)
    /* Save exception bucket in a register outside the reg windows */
        mov     %o0, %g1
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
	mov     %g1, %o0

/* Callbacks C -> ML */

        .global Callback
Callback:
    /* Save callee-save registers and return address */
        save    %sp, -96, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %g1, %i1        /* environment */
        ld      [%g1], %l2      /* code pointer */
L108:
    /* Set up a callback link on the stack. */
        sub     %sp, 8, %sp
        Load(Caml_bottom_of_stack, %l0)
        Load(Caml_last_return_address, %l1)
        std     %l0, [%sp + 96]
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        call    L111
        mov     %o7, %g4        /* in delay slot */
        b       L110
        nop
L111:   sub     %sp, 8, %sp
        Load(Caml_exception_pointer, %g5)
        std     %g4, [%sp + 96]
        mov     %sp, %g5
    /* Reload allocation pointers */
        Load(Young_ptr, %g6)
        Address(Young_limit, %g7)
    /* Call the Caml code */
L109:   call    %l2
        nop
    /* Pop trap frame and restore caml_exception_pointer */
        ld      [%sp + 100], %g5
        add     %sp, 8, %sp
        Store(%g5, Caml_exception_pointer)
    /* Pop callback link, restoring the global variables used by caml_c_call */
        ldd     [%sp + 96], %l0
        add     %sp, 8, %sp
        Store(%l0, Caml_bottom_of_stack)
        Store(%l1, Caml_last_return_address)
    /* Save allocation pointer */
        Store(%g6, Young_ptr)
    /* Move result where the C function expects it */
        mov     %o0, %i0        /* %i0 will become %o0 after restore */
    /* Reload callee-save registers and return */
        ret
        restore
L110:
    /* The trap handler */
        Store(%g5, Caml_exception_pointer)
        Store(%g6, Young_ptr)
        ldd     [%sp + 96], %l0
        Store(%l0, Caml_bottom_of_stack)
        Store(%l1, Caml_last_return_address)
    /* Re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
        call    Mlraise         /* never returns */
        nop

        .global Callback2
Callback2:
    /* Save callee-save registers and return address */
        save    %sp, -104, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %i2, %i1        /* second arg */
        mov     %g1, %i2        /* environment */
        sethi   %hi(Caml_apply2), %l2
        b       L108
        or      %l2, %lo(Caml_apply2), %l2

        .global Callback3
Callback3:
    /* Save callee-save registers and return address */
        save    %sp, -104, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %i2, %i1        /* second arg */
        mov     %i3, %i2        /* third arg */
        mov     %g1, %i3        /* environment */
        sethi   %hi(Caml_apply3), %l2
        b       L108
        or      %l2, %lo(Caml_apply3), %l2

        .data
        .global System_frametable
System_frametable:
        .word   1               /* one descriptor */
        .word   L109            /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots */
