/*********************************************************************/
/*                                                                   */
/*                          Objective Caml                           */
/*                                                                   */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        */
/*                                                                   */
/* Copyright 1996 Institut National de Recherche en Informatique et  */
/* en Automatique.  All rights reserved.  This file is distributed   */
/* under the terms of the GNU Library General Public License, with   */
/* the special exception on linking described in file ../LICENSE.    */
/*                                                                   */
/*********************************************************************/

/* $Id$ */

.macro Addrglobal       /* reg, glob */
        addis   $0, 0, ha16($1)
        addi    $0, $0, lo16($1)
.endmacro
.macro Loadglobal       /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        lwz     $0, lo16($1)($2)
.endmacro
.macro Storeglobal      /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        stw     $0, lo16($1)($2)
.endmacro

        .text

/* Invoke the garbage collector. */

        .globl  _caml_call_gc
_caml_call_gc:
    /* Set up stack frame */
        stwu    r1, -0x1A0(r1)
    /* 0x1A0 = 4*32 (int regs) + 8*32 (float regs) + 32 (space for C call) */
    /* Record return address into Caml code */
        mflr    r0
        Storeglobal r0, _caml_last_return_address, r11
    /* Record lowest stack address */
        addi    r0, r1, 0x1A0
        Storeglobal r0, _caml_bottom_of_stack, r11
    /* Record pointer to register array */
        addi    r0, r1, 8*32 + 32
        Storeglobal r0, _caml_gc_regs, r11
    /* Save current allocation pointer for debugging purposes */
        Storeglobal r31, _caml_young_ptr, r11
    /* Save exception pointer (if e.g. a sighandler raises) */
        Storeglobal r29, _caml_exception_pointer, r11
    /* Save all registers used by the code generator */
        addi    r11, r1, 8*32 + 32 - 4
        stwu    r3, 4(r11)
        stwu    r4, 4(r11)
        stwu    r5, 4(r11)
        stwu    r6, 4(r11)
        stwu    r7, 4(r11)
        stwu    r8, 4(r11)
        stwu    r9, 4(r11)
        stwu    r10, 4(r11)
        stwu    r14, 4(r11)
        stwu    r15, 4(r11)
        stwu    r16, 4(r11)
        stwu    r17, 4(r11)
        stwu    r18, 4(r11)
        stwu    r19, 4(r11)
        stwu    r20, 4(r11)
        stwu    r21, 4(r11)
        stwu    r22, 4(r11)
        stwu    r23, 4(r11)
        stwu    r24, 4(r11)
        stwu    r25, 4(r11)
        stwu    r26, 4(r11)
        stwu    r27, 4(r11)
        stwu    r28, 4(r11)
        addi    r11, r1, 32 - 8
        stfdu   f1, 8(r11)
        stfdu   f2, 8(r11)
        stfdu   f3, 8(r11)
        stfdu   f4, 8(r11)
        stfdu   f5, 8(r11)
        stfdu   f6, 8(r11)
        stfdu   f7, 8(r11)
        stfdu   f8, 8(r11)
        stfdu   f9, 8(r11)
        stfdu   f10, 8(r11)
        stfdu   f11, 8(r11)
        stfdu   f12, 8(r11)
        stfdu   f13, 8(r11)
        stfdu   f14, 8(r11)
        stfdu   f15, 8(r11)
        stfdu   f16, 8(r11)
        stfdu   f17, 8(r11)
        stfdu   f18, 8(r11)
        stfdu   f19, 8(r11)
        stfdu   f20, 8(r11)
        stfdu   f21, 8(r11)
        stfdu   f22, 8(r11)
        stfdu   f23, 8(r11)
        stfdu   f24, 8(r11)
        stfdu   f25, 8(r11)
        stfdu   f26, 8(r11)
        stfdu   f27, 8(r11)
        stfdu   f28, 8(r11)
        stfdu   f29, 8(r11)
        stfdu   f30, 8(r11)
        stfdu   f31, 8(r11)
    /* Call the GC */
        bl      _caml_garbage_collection
    /* Reload new allocation pointer and allocation limit */
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Restore all regs used by the code generator */
        addi    r11, r1, 8*32 + 32 - 4
        lwzu    r3, 4(r11)
        lwzu    r4, 4(r11)
        lwzu    r5, 4(r11)
        lwzu    r6, 4(r11)
        lwzu    r7, 4(r11)
        lwzu    r8, 4(r11)
        lwzu    r9, 4(r11)
        lwzu    r10, 4(r11)
        lwzu    r14, 4(r11)
        lwzu    r15, 4(r11)
        lwzu    r16, 4(r11)
        lwzu    r17, 4(r11)
        lwzu    r18, 4(r11)
        lwzu    r19, 4(r11)
        lwzu    r20, 4(r11)
        lwzu    r21, 4(r11)
        lwzu    r22, 4(r11)
        lwzu    r23, 4(r11)
        lwzu    r24, 4(r11)
        lwzu    r25, 4(r11)
        lwzu    r26, 4(r11)
        lwzu    r27, 4(r11)
        lwzu    r28, 4(r11)
        addi    r11, r1, 32 - 8
        lfdu    f1, 8(r11)
        lfdu    f2, 8(r11)
        lfdu    f3, 8(r11)
        lfdu    f4, 8(r11)
        lfdu    f5, 8(r11)
        lfdu    f6, 8(r11)
        lfdu    f7, 8(r11)
        lfdu    f8, 8(r11)
        lfdu    f9, 8(r11)
        lfdu    f10, 8(r11)
        lfdu    f11, 8(r11)
        lfdu    f12, 8(r11)
        lfdu    f13, 8(r11)
        lfdu    f14, 8(r11)
        lfdu    f15, 8(r11)
        lfdu    f16, 8(r11)
        lfdu    f17, 8(r11)
        lfdu    f18, 8(r11)
        lfdu    f19, 8(r11)
        lfdu    f20, 8(r11)
        lfdu    f21, 8(r11)
        lfdu    f22, 8(r11)
        lfdu    f23, 8(r11)
        lfdu    f24, 8(r11)
        lfdu    f25, 8(r11)
        lfdu    f26, 8(r11)
        lfdu    f27, 8(r11)
        lfdu    f28, 8(r11)
        lfdu    f29, 8(r11)
        lfdu    f30, 8(r11)
        lfdu    f31, 8(r11)
    /* Return to caller, restarting the allocation */
        Loadglobal r0, _caml_last_return_address, r11
        addic   r0, r0, -16     /* Restart the allocation (4 instructions) */
        mtlr    r0
    /* Say we are back into Caml code */
        li      r12, 0
        Storeglobal r12, _caml_last_return_address, r11
    /* Deallocate stack frame */
        addi    r1, r1, 0x1A0
    /* Return */
        blr

/* Call a C function from Caml */

        .globl  _caml_c_call
_caml_c_call:
    /* Save return address */
        mflr    r25
    /* Get ready to call C function (address in 11) */
        mtctr   r11
    /* Record lowest stack address and return address */
        Storeglobal r1, _caml_bottom_of_stack, r12
        Storeglobal r25, _caml_last_return_address, r12
    /* Make the exception handler and alloc ptr available to the C code */
        Storeglobal r31, _caml_young_ptr, r11
        Storeglobal r29, _caml_exception_pointer, r11
    /* Call the function (address in link register) */
        bctrl
    /* Restore return address (in 25, preserved by the C function) */
        mtlr    r25
    /* Reload allocation pointer and allocation limit*/
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into Caml code */
        li      r12, 0
        Storeglobal r12, _caml_last_return_address, r11
    /* Return to caller */
        blr

/* Raise an exception from C */

        .globl  _caml_raise_exception
_caml_raise_exception:
    /* Reload Caml global registers */
        Loadglobal r1, _caml_exception_pointer, r11
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into Caml code */
        li      r0, 0
        Storeglobal r0, _caml_last_return_address, r11
    /* Pop trap frame */
        lwz     r0, 0(r1)
        lwz     r29, 4(r1)
        mtlr    r0
        addi    r1, r1, 16
    /* Branch to handler */
        blr

/* Start the Caml program */

        .globl  _caml_start_program
_caml_start_program:
        Addrglobal r12, _caml_program

/* Code shared between caml_start_program and caml_callback */
L102:
    /* Allocate and link stack frame */
        stwu    r1, -256(r1)
    /* Save return address */
        mflr    r0
        stw r0,  256+4(r1)
    /* Save all callee-save registers */
    /* GPR 14 at sp+16 ... GPR 31 at sp+84
       FPR 14 at sp+92 ... FPR 31 at sp+228 */
        addi    r11, r1, 16-4
        stwu    r14, 4(r11)
        stwu    r15, 4(r11)
        stwu    r16, 4(r11)
        stwu    r17, 4(r11)
        stwu    r18, 4(r11)
        stwu    r19, 4(r11)
        stwu    r20, 4(r11)
        stwu    r21, 4(r11)
        stwu    r22, 4(r11)
        stwu    r23, 4(r11)
        stwu    r24, 4(r11)
        stwu    r25, 4(r11)
        stwu    r26, 4(r11)
        stwu    r27, 4(r11)
        stwu    r28, 4(r11)
        stwu    r29, 4(r11)
        stwu    r30, 4(r11)
        stwu    r31, 4(r11)
        stfdu   f14, 8(r11)
        stfdu   f15, 8(r11)
        stfdu   f16, 8(r11)
        stfdu   f17, 8(r11)
        stfdu   f18, 8(r11)
        stfdu   f19, 8(r11)
        stfdu   f20, 8(r11)
        stfdu   f21, 8(r11)
        stfdu   f22, 8(r11)
        stfdu   f23, 8(r11)
        stfdu   f24, 8(r11)
        stfdu   f25, 8(r11)
        stfdu   f26, 8(r11)
        stfdu   f27, 8(r11)
        stfdu   f28, 8(r11)
        stfdu   f29, 8(r11)
        stfdu   f30, 8(r11)
        stfdu   f31, 8(r11)
    /* Set up a callback link */
        addi    r1, r1, -16
        Loadglobal r9, _caml_bottom_of_stack, r11
        Loadglobal r10, _caml_last_return_address, r11
        Loadglobal r11, _caml_gc_regs, r11
        stw     r9, 0(r1)
        stw     r10, 4(r1)
        stw     r11, 8(r1)
    /* Build an exception handler to catch exceptions escaping out of Caml */
        bl      L103
        b       L104
L103:
        addi    r1, r1, -16
        mflr    r0
        stw     r0, 0(r1)
        Loadglobal r11, _caml_exception_pointer, r11
        stw     r11, 4(r1)
        mr      r29, r1
    /* Reload allocation pointers */
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into Caml code */
        li      r0, 0
        Storeglobal r0, _caml_last_return_address, r11
    /* Call the Caml code */
        mtctr    r12
L105:
        bctrl
    /* Pop the trap frame, restoring caml_exception_pointer */
        lwz     r9, 4(r1)
        Storeglobal r9, _caml_exception_pointer, r11
        addi    r1, r1, 16
    /* Pop the callback link, restoring the global variables */
L106:
        lwz     r9, 0(r1)
        lwz     r10, 4(r1)
        lwz     r11, 8(r1)
        Storeglobal r9, _caml_bottom_of_stack, r12
        Storeglobal r10, _caml_last_return_address, r12
        Storeglobal r11, _caml_gc_regs, r12
        addi    r1, r1, 16
    /* Update allocation pointer */
        Storeglobal r31, _caml_young_ptr, r11
    /* Restore callee-save registers */
        addi    r11, r1, 16-4
        lwzu    r14, 4(r11)
        lwzu    r15, 4(r11)
        lwzu    r16, 4(r11)
        lwzu    r17, 4(r11)
        lwzu    r18, 4(r11)
        lwzu    r19, 4(r11)
        lwzu    r20, 4(r11)
        lwzu    r21, 4(r11)
        lwzu    r22, 4(r11)
        lwzu    r23, 4(r11)
        lwzu    r24, 4(r11)
        lwzu    r25, 4(r11)
        lwzu    r26, 4(r11)
        lwzu    r27, 4(r11)
        lwzu    r28, 4(r11)
        lwzu    r29, 4(r11)
        lwzu    r30, 4(r11)
        lwzu    r31, 4(r11)
        lfdu    f14, 8(r11)
        lfdu    f15, 8(r11)
        lfdu    f16, 8(r11)
        lfdu    f17, 8(r11)
        lfdu    f18, 8(r11)
        lfdu    f19, 8(r11)
        lfdu    f20, 8(r11)
        lfdu    f21, 8(r11)
        lfdu    f22, 8(r11)
        lfdu    f23, 8(r11)
        lfdu    f24, 8(r11)
        lfdu    f25, 8(r11)
        lfdu    f26, 8(r11)
        lfdu    f27, 8(r11)
        lfdu    f28, 8(r11)
        lfdu    f29, 8(r11)
        lfdu    f30, 8(r11)
        lfdu    f31, 8(r11)
    /* Reload return address */
        lwz     r0, 256+4(r1)
        mtlr    r0
    /* Return */
        addi    r1, r1, 256
        blr

    /* The trap handler: */
L104:
    /* Update caml_exception_pointer */
        Storeglobal r29, _caml_exception_pointer, r11
    /* Encode exception bucket as an exception result and return it */
        ori     r3, r3, 2
        b       L106

/* Callback from C to Caml */

        .globl  _caml_callback_exn
_caml_callback_exn:
    /* Initial shuffling of arguments */
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* Argument */
        mr      r4, r0
        lwz     r12, 0(r4)        /* Code pointer */
        b       L102

        .globl  _caml_callback2_exn
_caml_callback2_exn:
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* First argument */
        mr      r4, r5            /* Second argument */
        mr      r5, r0
        Addrglobal r12, _caml_apply2
        b       L102

        .globl  _caml_callback3_exn
_caml_callback3_exn:
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* First argument */
        mr      r4, r5            /* Second argument */
        mr      r5, r6            /* Third argument */
        mr      r6, r0
        Addrglobal r12, _caml_apply3
        b       L102

/* Frame table */

        .const
        .globl  _caml_system__frametable
_caml_system__frametable:
        .long   1               /* one descriptor */
        .long   L105 + 4       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */

