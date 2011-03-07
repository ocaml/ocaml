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

/* Asm part of the runtime system, Alpha processor */

/* Allocation */

        .text
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_allocN
        .globl  caml_call_gc

/* Note: the profiling code sets $27 to the address of the "normal" entrypoint.
   So don't pass parameters to those routines in $27. */

/* caml_alloc* : all code generator registers preserved,
   $gp preserved, $27 not necessarily valid on entry */

        .globl  caml_alloc1
        .ent    caml_alloc1
        .align  3
caml_alloc1:
        .prologue 0
        subq    $13, 16, $13
        cmpult  $13, $14, $25
        bne     $25, $100
        ret     ($26)
$100:   ldiq    $25, 16
        br      $110
        .end    caml_alloc1

        .globl  caml_alloc2
        .ent    caml_alloc2
        .align  3
caml_alloc2:
        .prologue 0
        subq    $13, 24, $13
        cmpult  $13, $14, $25
        bne     $25, $101
        ret     ($26)
$101:   ldiq    $25, 24
        br      $110
        .end    caml_alloc2

        .globl  caml_alloc3
        .ent    caml_alloc3
        .align  3
caml_alloc3:
        .prologue 0
        subq    $13, 32, $13
        cmpult  $13, $14, $25
        bne     $25, $102
        ret     ($26)
$102:   ldiq    $25, 32
        br      $110
        .end    caml_alloc3

        .globl  caml_allocN
        .ent    caml_allocN
        .align  3
caml_allocN:
        .prologue 0
        subq    $13, $25, $13
        .set    noat
        cmpult  $13, $14, $at
        bne     $at, $110
        .set    at
        ret     ($26)
        .end    caml_allocN

        .globl  caml_call_gc
        .ent    caml_call_gc
        .align  3
caml_call_gc:
        .prologue 0
        ldiq    $25, 0
$110:   lda     $sp, -0x200($sp)
    /* 0x200 = 32*8 (ints) + 32*8 (floats) */
        stq     $26, 0x1F8($sp)         /* return address */
        stq     $gp, 0x1F0($sp)         /* caller's $gp */
        stq     $25, 0x1E8($sp)         /* desired size */
    /* Rebuild $gp */
        br      $27, $103
$103:   ldgp    $gp, 0($27)
    /* Record lowest stack address, return address, GC regs */
        stq     $26, caml_last_return_address
        lda     $24, 0x200($sp)
        stq     $24, caml_bottom_of_stack
        lda     $24, 0x100($sp)
        stq     $24, caml_gc_regs
    /* Save current allocation pointer for debugging purposes */
$113:   stq     $13, caml_young_ptr
    /* Save trap pointer in case an exception is raised (e.g. sighandler) */
        stq     $15, caml_exception_pointer
    /* Save all integer regs used by the code generator in the context */
        stq     $0, 0 * 8 ($24)
        stq     $1, 1 * 8 ($24)
        stq     $2, 2 * 8 ($24)
        stq     $3, 3 * 8 ($24)
        stq     $4, 4 * 8 ($24)
        stq     $5, 5 * 8 ($24)
        stq     $6, 6 * 8 ($24)
        stq     $7, 7 * 8 ($24)
        stq     $8, 8 * 8 ($24)
        stq     $9, 9 * 8 ($24)
        stq     $10, 10 * 8 ($24)
        stq     $11, 11 * 8 ($24)
        stq     $12, 12 * 8 ($24)
        stq     $16, 16 * 8 ($24)
        stq     $17, 17 * 8 ($24)
        stq     $18, 18 * 8 ($24)
        stq     $19, 19 * 8 ($24)
        stq     $20, 20 * 8 ($24)
        stq     $21, 21 * 8 ($24)
        stq     $22, 22 * 8 ($24)
    /* Save all float regs that are not callee-save on the stack */
        stt     $f0, 0 * 8 ($sp)
        stt     $f1, 1 * 8 ($sp)
        stt     $f10, 10 * 8 ($sp)
        stt     $f11, 11 * 8 ($sp)
        stt     $f12, 12 * 8 ($sp)
        stt     $f13, 13 * 8 ($sp)
        stt     $f14, 14 * 8 ($sp)
        stt     $f15, 15 * 8 ($sp)
        stt     $f16, 16 * 8 ($sp)
        stt     $f17, 17 * 8 ($sp)
        stt     $f18, 18 * 8 ($sp)
        stt     $f19, 19 * 8 ($sp)
        stt     $f20, 20 * 8 ($sp)
        stt     $f21, 21 * 8 ($sp)
        stt     $f22, 22 * 8 ($sp)
        stt     $f23, 23 * 8 ($sp)
        stt     $f24, 24 * 8 ($sp)
        stt     $f25, 25 * 8 ($sp)
        stt     $f26, 26 * 8 ($sp)
        stt     $f27, 27 * 8 ($sp)
        stt     $f29, 29 * 8 ($sp)
        stt     $f30, 30 * 8 ($sp)
    /* Call the garbage collector */
        jsr     caml_garbage_collection
        ldgp    $gp, 0($26)
    /* Restore all regs used by the code generator */
        lda     $24, 0x100($sp)
        ldq     $0, 0 * 8 ($24)
        ldq     $1, 1 * 8 ($24)
        ldq     $2, 2 * 8 ($24)
        ldq     $3, 3 * 8 ($24)
        ldq     $4, 4 * 8 ($24)
        ldq     $5, 5 * 8 ($24)
        ldq     $6, 6 * 8 ($24)
        ldq     $7, 7 * 8 ($24)
        ldq     $8, 8 * 8 ($24)
        ldq     $9, 9 * 8 ($24)
        ldq     $10, 10 * 8 ($24)
        ldq     $11, 11 * 8 ($24)
        ldq     $12, 12 * 8 ($24)
        ldq     $16, 16 * 8 ($24)
        ldq     $17, 17 * 8 ($24)
        ldq     $18, 18 * 8 ($24)
        ldq     $19, 19 * 8 ($24)
        ldq     $20, 20 * 8 ($24)
        ldq     $21, 21 * 8 ($24)
        ldq     $22, 22 * 8 ($24)
        ldt     $f0, 0 * 8 ($sp)
        ldt     $f1, 1 * 8 ($sp)
        ldt     $f10, 10 * 8 ($sp)
        ldt     $f11, 11 * 8 ($sp)
        ldt     $f12, 12 * 8 ($sp)
        ldt     $f13, 13 * 8 ($sp)
        ldt     $f14, 14 * 8 ($sp)
        ldt     $f15, 15 * 8 ($sp)
        ldt     $f16, 16 * 8 ($sp)
        ldt     $f17, 17 * 8 ($sp)
        ldt     $f18, 18 * 8 ($sp)
        ldt     $f19, 19 * 8 ($sp)
        ldt     $f20, 20 * 8 ($sp)
        ldt     $f21, 21 * 8 ($sp)
        ldt     $f22, 22 * 8 ($sp)
        ldt     $f23, 23 * 8 ($sp)
        ldt     $f24, 24 * 8 ($sp)
        ldt     $f25, 25 * 8 ($sp)
        ldt     $f26, 26 * 8 ($sp)
        ldt     $f27, 27 * 8 ($sp)
        ldt     $f29, 29 * 8 ($sp)
        ldt     $f30, 30 * 8 ($sp)
    /* Reload new allocation pointer and allocation limit */
        ldq     $13, caml_young_ptr
        ldq     $14, caml_young_limit
    /* Allocate space for the block */
        ldq     $25, 0x1E8($sp)
        subq    $13, $25, $13
        cmpult  $13, $14, $25   /* Check that we have enough free space */
        bne     $25, $113       /* If not, call GC again */
    /* Say that we are back into Caml code */
        stq     $31, caml_last_return_address
    /* Return to caller */
        ldq     $26, 0x1F8($sp)
        ldq     $gp, 0x1F0($sp)
        lda     $sp, 0x200($sp)
        ret     ($26)

        .end    caml_call_gc

/* Call a C function from Caml */
/* Function to call is in $25 */

        .globl  caml_c_call
        .ent    caml_c_call
        .align  3
caml_c_call:
        .prologue 0
    /* Preserve return address and caller's $gp in callee-save registers */
        mov     $26, $9
        mov     $gp, $10
    /* Rebuild $gp */
        br      $27, $104
$104:   ldgp    $gp, 0($27)
    /* Record lowest stack address and return address */
        lda     $11, caml_last_return_address
        stq     $26, 0($11)
        stq     $sp, caml_bottom_of_stack
    /* Make the exception handler and alloc ptr available to the C code */
        lda     $12, caml_young_ptr
        stq     $13, 0($12)
        lda     $14, caml_young_limit
        stq     $15, caml_exception_pointer
    /* Call the function */
        mov     $25, $27
        jsr     ($25)
    /* Reload alloc ptr and alloc limit */
        ldq     $13, 0($12)  /* $12 still points to caml_young_ptr */
        ldq     $14, 0($14)  /* $14 still points to caml_young_limit */
    /* Say that we are back into Caml code */
        stq     $31, 0($11)  /* $11 still points to caml_last_return_address */
    /* Restore $gp */
        mov     $10, $gp
    /* Return */
        ret     ($9)

        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .ent    caml_start_program
        .align  3
caml_start_program:
        ldgp    $gp, 0($27)
        lda     $25, caml_program

/* Code shared with caml_callback* */
$107:
    /* Save return address */
        lda     $sp, -128($sp)
        stq     $26, 0($sp)
    /* Save all callee-save registers */
        stq     $9, 8($sp)
        stq     $10, 16($sp)
        stq     $11, 24($sp)
        stq     $12, 32($sp)
        stq     $13, 40($sp)
        stq     $14, 48($sp)
        stq     $15, 56($sp)
        stt     $f2, 64($sp)
        stt     $f3, 72($sp)
        stt     $f4, 80($sp)
        stt     $f5, 88($sp)
        stt     $f6, 96($sp)
        stt     $f7, 104($sp)
        stt     $f8, 112($sp)
        stt     $f9, 120($sp)
    /* Set up a callback link on the stack. */
        lda     $sp, -32($sp)
        ldq     $0, caml_bottom_of_stack
        stq     $0, 0($sp)
        ldq     $1, caml_last_return_address
        stq     $1, 8($sp)
        ldq     $1, caml_gc_regs
        stq     $1, 16($sp)
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        lda     $sp, -16($sp)
        ldq     $15, caml_exception_pointer
        stq     $15, 0($sp)
        lda     $0, $109
        stq     $0, 8($sp)
        mov     $sp, $15
    /* Reload allocation pointers */
        ldq     $13, caml_young_ptr
        ldq     $14, caml_young_limit
    /* We are back into Caml code */
        stq     $31, caml_last_return_address
    /* Call the Caml code */
        mov     $25, $27
$108:   jsr     ($25)
    /* Reload $gp, masking off low bit in retaddr (might have been marked) */
        bic     $26, 1, $26
        ldgp    $gp, 4($26)
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldq     $15, 0($sp)
        stq     $15, caml_exception_pointer
        lda     $sp, 16($sp)
    /* Pop the callback link, restoring the global variables */
$112:   ldq     $24, 0($sp)
        stq     $24, caml_bottom_of_stack
        ldq     $25, 8($sp)
        stq     $25, caml_last_return_address
        ldq     $24, 16($sp)
        stq     $24, caml_gc_regs
        lda     $sp, 32($sp)
    /* Update allocation pointer */
        stq     $13, caml_young_ptr
    /* Reload callee-save registers */
        ldq     $9, 8($sp)
        ldq     $10, 16($sp)
        ldq     $11, 24($sp)
        ldq     $12, 32($sp)
        ldq     $13, 40($sp)
        ldq     $14, 48($sp)
        ldq     $15, 56($sp)
        ldt     $f2, 64($sp)
        ldt     $f3, 72($sp)
        ldt     $f4, 80($sp)
        ldt     $f5, 88($sp)
        ldt     $f6, 96($sp)
        ldt     $f7, 104($sp)
        ldt     $f8, 112($sp)
        ldt     $f9, 120($sp)
    /* Return to caller */
        ldq     $26, 0($sp)
        lda     $sp, 128($sp)
        ret     ($26)

    /* The trap handler */
$109:   ldgp    $gp, 0($26)
    /* Save exception pointer */
        stq     $15, caml_exception_pointer
    /* Encode exception bucket as an exception result */
        or      $0, 2, $0
    /* Return it */
        br      $112

        .end    caml_start_program

/* Raise an exception from C */

        .globl  caml_raise_exception
        .ent    caml_raise_exception
        .align  3
caml_raise_exception:
        ldgp    $gp, 0($27)
        mov     $16, $0                         /* Move exn bucket */
        ldq     $13, caml_young_ptr
        ldq     $14, caml_young_limit
        stq     $31, caml_last_return_address   /* We're back into Caml */
        ldq     $sp, caml_exception_pointer
        ldq     $15, 0($sp)
        ldq     $26, 8($sp)
        lda     $sp, 16($sp)
        jmp     $25, ($26)      /* Keep retaddr in $25 to help debugging */
        .end    caml_raise_exception

/* Callback from C to Caml */

        .globl  caml_callback_exn
        .ent    caml_callback_exn
        .align  3
caml_callback_exn:
    /* Initial shuffling of arguments */
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $25, $17        /* environment */
        ldq     $25, 0($25)     /* code pointer */
        br      $107
        .end    caml_callback_exn

        .globl  caml_callback2_exn
        .ent    caml_callback2_exn
        .align  3
caml_callback2_exn:
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $18, $17        /* second arg */
        mov     $25, $18        /* environment */
        lda     $25, caml_apply2
        br      $107
        .end    caml_callback2_exn

        .globl  caml_callback3_exn
        .ent    caml_callback3_exn
        .align  3
caml_callback3_exn:
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $18, $17        /* second arg */
        mov     $19, $18        /* third arg */
        mov     $25, $19        /* environment */
        lda     $25, caml_apply3
        br      $107
        .end    caml_callback3_exn

/* Glue code to call [caml_array_bound_error] */

        .globl  caml_ml_array_bound_error
        .ent    caml_ml_array_bound_error
        .align  3
caml_ml_array_bound_error:
        br      $27, $111
$111:   ldgp    $gp, 0($27)
        lda     $25, caml_array_bound_error
        br      caml_c_call             /* never returns */
        .end    caml_ml_array_bound_error

#if defined(SYS_digital)
        .rdata
#else
        .section .rodata
#endif
        .globl  caml_system__frametable
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   $108 + 4        /* return address into callback */
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
        .align  3
