/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system for the Sparc processor.  */
/* Must be preprocessed by cpp */

/* SunOS 4 prefixes identifiers with _ */

#if defined(SYS_sunos)

#define Caml_young_limit _caml_young_limit
#define Caml_young_ptr _caml_young_ptr
#define Caml_bottom_of_stack _caml_bottom_of_stack
#define Caml_last_return_address _caml_last_return_address
#define Caml_gc_regs _caml_gc_regs
#define Caml_exception_pointer _caml_exception_pointer
#define Caml_allocN _caml_allocN
#define Caml_call_gc _caml_call_gc
#define Caml_garbage_collection _caml_garbage_collection
#define Caml_c_call _caml_c_call
#define Caml_start_program _caml_start_program
#define Caml_program _caml_program
#define Caml_raise_exception _caml_raise_exception
#define Caml_callback_exn _caml_callback_exn
#define Caml_callback2_exn _caml_callback2_exn
#define Caml_callback3_exn _caml_callback3_exn
#define Caml_apply2 _caml_apply2
#define Caml_apply3 _caml_apply3
#define Caml_raise _caml_raise
#define Caml_system__frametable _caml_system__frametable
#define Caml_ml_array_bound_error _caml_ml_array_bound_error
#define Caml_array_bound_error _caml_array_bound_error

#else

#define Caml_young_limit caml_young_limit
#define Caml_young_ptr caml_young_ptr
#define Caml_bottom_of_stack caml_bottom_of_stack
#define Caml_last_return_address caml_last_return_address
#define Caml_gc_regs caml_gc_regs
#define Caml_exception_pointer caml_exception_pointer
#define Caml_allocN caml_allocN
#define Caml_call_gc caml_call_gc
#define Caml_garbage_collection caml_garbage_collection
#define Caml_c_call caml_c_call
#define Caml_start_program caml_start_program
#define Caml_program caml_program
#define Caml_raise_exception caml_raise_exception
#define Caml_callback_exn caml_callback_exn
#define Caml_callback2_exn caml_callback2_exn
#define Caml_callback3_exn caml_callback3_exn
#define Caml_apply2 caml_apply2
#define Caml_apply3 caml_apply3
#define Caml_raise caml_raise
#define Caml_system__frametable caml_system__frametable
#define Caml_ml_array_bound_error caml_ml_array_bound_error
#define Caml_array_bound_error caml_array_bound_error

#endif

#ifndef SYS_solaris
#define INDIRECT_LIMIT
#endif

#define Exn_ptr %l5
#define Alloc_ptr %l6
#define Alloc_limit %l7

#define Load(symb,reg)  sethi %hi(symb), %g1; ld [%g1 + %lo(symb)], reg
#define Store(reg,symb) sethi %hi(symb), %g1; st reg, [%g1 + %lo(symb)]
#define Address(symb,reg) sethi %hi(symb), reg; or reg, %lo(symb), reg

/* Allocation functions */

        .text
        .global Caml_allocN
        .global Caml_call_gc

/* Required size in %g2 */
Caml_allocN:
#ifdef INDIRECT_LIMIT
        ld      [Alloc_limit], %g1
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, %g1
#else
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, Alloc_limit
#endif
        /*blu,pt  %icc, Caml_call_gc*/
        blu     Caml_call_gc
        nop
        retl
        nop

/* Required size in %g2 */
Caml_call_gc:
    /* Save exception pointer if GC raises */
        Store(Exn_ptr, Caml_exception_pointer)
    /* Save current allocation pointer for debugging purposes */
        Store(Alloc_ptr, Caml_young_ptr)
    /* Record lowest stack address */
        Store(%sp, Caml_bottom_of_stack)
    /* Record last return address */
        Store(%o7, Caml_last_return_address)
    /* Allocate space on stack for caml_context structure and float regs */
        sub     %sp, 20*4 + 15*8, %sp
    /* Save int regs on stack and save it into caml_gc_regs */
L100:   add     %sp, 96 + 15*8, %g1
        st      %o0, [%g1]
        st      %o1, [%g1 + 0x4]
        st      %o2, [%g1 + 0x8]
        st      %o3, [%g1 + 0xc]
        st      %o4, [%g1 + 0x10]
        st      %o5, [%g1 + 0x14]
        st      %i0, [%g1 + 0x18]
        st      %i1, [%g1 + 0x1c]
        st      %i2, [%g1 + 0x20]
        st      %i3, [%g1 + 0x24]
        st      %i4, [%g1 + 0x28]
        st      %i5, [%g1 + 0x2c]
        st      %l0, [%g1 + 0x30]
        st      %l1, [%g1 + 0x34]
        st      %l2, [%g1 + 0x38]
        st      %l3, [%g1 + 0x3c]
        st      %l4, [%g1 + 0x40]
        st      %g3, [%g1 + 0x44]
        st      %g4, [%g1 + 0x48]
        st      %g2, [%g1 + 0x4C]       /* Save required size */
        mov     %g1, %g2
        Store(%g2, Caml_gc_regs)
    /* Save the floating-point registers */
        add     %sp, 96, %g1
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
        call    Caml_garbage_collection
        nop
    /* Restore all regs used by the code generator */
        add     %sp, 96 + 15*8, %g1
        ld      [%g1], %o0
        ld      [%g1 + 0x4], %o1
        ld      [%g1 + 0x8], %o2
        ld      [%g1 + 0xc], %o3
        ld      [%g1 + 0x10], %o4
        ld      [%g1 + 0x14], %o5
        ld      [%g1 + 0x18], %i0
        ld      [%g1 + 0x1c], %i1
        ld      [%g1 + 0x20], %i2
        ld      [%g1 + 0x24], %i3
        ld      [%g1 + 0x28], %i4
        ld      [%g1 + 0x2c], %i5
        ld      [%g1 + 0x30], %l0
        ld      [%g1 + 0x34], %l1
        ld      [%g1 + 0x38], %l2
        ld      [%g1 + 0x3c], %l3
        ld      [%g1 + 0x40], %l4
        ld      [%g1 + 0x44], %g3
        ld      [%g1 + 0x48], %g4
        ld      [%g1 + 0x4C], %g2     /* Recover desired size */
        add     %sp, 96, %g1
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
    /* Reload alloc ptr */
        Load(Caml_young_ptr, Alloc_ptr)
    /* Allocate space for block */
#ifdef INDIRECT_LIMIT
        ld      [Alloc_limit], %g1
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, %g1      /* Check that we have enough free space */
#else
        Load(Caml_young_limit,Alloc_limit)
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, Alloc_limit
#endif
        blu     L100                /* If not, call GC again */
        nop
    /* Return to caller */
        Load(Caml_last_return_address, %o7)
        retl
        add     %sp, 20*4 + 15*8, %sp       /* in delay slot */

/* Call a C function from Caml */

        .global Caml_c_call
/* Function to call is in %g2 */
Caml_c_call:
    /* Record lowest stack address and return address */
        Store(%sp, Caml_bottom_of_stack)
        Store(%o7, Caml_last_return_address)
    /* Save the exception handler and alloc pointer */
        Store(Exn_ptr, Caml_exception_pointer)
        sethi   %hi(Caml_young_ptr), %g1
    /* Call the C function */
        call    %g2
        st      Alloc_ptr, [%g1 + %lo(Caml_young_ptr)]   /* in delay slot */
    /* Reload return address */
        Load(Caml_last_return_address, %o7)
    /* Reload alloc pointer */
        sethi   %hi(Caml_young_ptr), %g1
    /* Return to caller */
        retl
        ld      [%g1 + %lo(Caml_young_ptr)], Alloc_ptr   /* in delay slot */

/* Start the Caml program */

        .global Caml_start_program
Caml_start_program:
    /* Save all callee-save registers */
        save    %sp, -96, %sp
    /* Address of code to call */
        Address(Caml_program, %l2)

    /* Code shared with caml_callback* */
L108:
    /* Set up a callback link on the stack. */
        sub     %sp, 16, %sp
        Load(Caml_bottom_of_stack, %l0)
        Load(Caml_last_return_address, %l1)
        Load(Caml_gc_regs, %l3)
        st      %l0, [%sp + 96]
        st      %l1, [%sp + 100]
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        call    L111
        st      %l3, [%sp + 104]
        b       L110
        nop
L111:   sub     %sp, 8, %sp
        Load(Caml_exception_pointer, Exn_ptr)
        st      %o7, [%sp + 96]
        st      Exn_ptr, [%sp + 100]
        mov     %sp, Exn_ptr
    /* Reload allocation pointers */
        Load(Caml_young_ptr, Alloc_ptr)
#ifdef INDIRECT_LIMIT
        Address(Caml_young_limit, Alloc_limit)
#else
        Load(Caml_young_limit, Alloc_limit)
#endif
    /* Call the Caml code */
L109:   call    %l2
        nop
    /* Pop trap frame and restore caml_exception_pointer */
        ld      [%sp + 100], Exn_ptr
        add     %sp, 8, %sp
        Store(Exn_ptr, Caml_exception_pointer)
    /* Pop callback link, restoring the global variables */
L112:   ld      [%sp + 96], %l0
        ld      [%sp + 100], %l1
        ld      [%sp + 104], %l2
        Store(%l0, Caml_bottom_of_stack)
        Store(%l1, Caml_last_return_address)
        Store(%l2, Caml_gc_regs)
        add     %sp, 16, %sp
    /* Save allocation pointer */
        Store(Alloc_ptr, Caml_young_ptr)
    /* Reload callee-save registers and return */
        ret
        restore %o0, 0, %o0     /* copy %o0 in this window to caller's %o0 */
L110:
    /* The trap handler */
        Store(Exn_ptr, Caml_exception_pointer)
    /* Encode exception bucket as an exception result */
        b       L112
        or      %o0, 2, %o0

/* Raise an exception from C */

        .global Caml_raise_exception
Caml_raise_exception:
    /* Save exception bucket in a register outside the reg windows */
        mov     %o0, %g2
    /* Load exception pointer in a register outside the reg windows */
        Load(Caml_exception_pointer, %g3)
    /* Pop some frames until the trap pointer is in the current frame. */
        cmp     %g3, %fp
        blt     L107                    /* if Exn_ptr < %fp, over */
        nop
L106:   restore
        cmp     %fp, %g3                /* if %fp <= Exn_ptr, loop */
        ble     L106
        nop
L107:
    /* Reload allocation registers */
        Load(Caml_young_ptr, Alloc_ptr)
#ifdef INDIRECT_LIMIT
        Address(Caml_young_limit, Alloc_limit)
#else
        Load(Caml_young_limit, Alloc_limit)
#endif
    /* Branch to exception handler */
        mov     %g3, %sp
        ld      [%sp + 96], %g1
        ld      [%sp + 100], Exn_ptr
        add     %sp, 8, %sp
        jmp     %g1 + 8
    /* Restore bucket, in delay slot */
        mov     %g2, %o0

/* Callbacks C -> ML */

        .global Caml_callback_exn
Caml_callback_exn:
    /* Save callee-save registers and return address */
        save    %sp, -96, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %g1, %i1        /* environment */
        b       L108
        ld      [%g1], %l2      /* code pointer */

        .global Caml_callback2_exn
Caml_callback2_exn:
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

        .global Caml_callback3_exn
Caml_callback3_exn:
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

#ifndef SYS_solaris
/* Glue code to call [caml_array_bound_error] */

        .global Caml_ml_array_bound_error
Caml_ml_array_bound_error:
        Address(Caml_array_bound_error, %g2)
        b       Caml_c_call
        nop
#endif

#ifdef SYS_solaris
        .section ".rodata"
#else
        .data
#endif
        .global Caml_system__frametable
        .align  4               /* required for gas? */
Caml_system__frametable:
        .word   1               /* one descriptor */
        .word   L109            /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots */

#ifdef SYS_solaris
	.type Caml_allocN, #function
	.type Caml_call_gc, #function
        .type Caml_c_call, #function
        .type Caml_start_program, #function
        .type Caml_raise_exception, #function
	.type Caml_system__frametable, #object
#endif
