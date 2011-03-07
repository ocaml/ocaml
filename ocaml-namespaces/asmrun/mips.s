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

/* Asm part of the runtime system, Mips processor, IRIX n32 conventions */

/* Allocation */

        .text

        .globl  caml_call_gc
        .ent    caml_call_gc

caml_call_gc:
    /* Reserve stack space for registers and saved $gp */
    /* 32 * 8 = 0x100 for float regs
       22 * 4 = 0x58  for integer regs
            8 = 0x8   for saved $gp ====> 0x160 total */
        subu    $sp, $sp, 0x160
    /* Reinit $gp */
        .cpsetup $25, 0x158, caml_call_gc
    /* Record return address */
        sw      $31, caml_last_return_address
    /* Record lowest stack address */
        addu    $24, $sp, 0x160
        sw      $24, caml_bottom_of_stack
    /* Save pointer to register array */
        addu    $24, $sp, 0x100
        sw      $24, caml_gc_regs
    /* Save current allocation pointer for debugging purposes */
        sw      $22, caml_young_ptr
    /* Save the exception handler (if e.g. a sighandler raises) */
        sw      $30, caml_exception_pointer
    /* Save all regs used by the code generator on the stack */
        sw      $2, 2 * 4($24)
        sw      $3, 3 * 4($24)
        sw      $4, 4 * 4($24)
        sw      $5, 5 * 4($24)
        sw      $6, 6 * 4($24)
        sw      $7, 7 * 4($24)
        sw      $8, 8 * 4($24)
        sw      $9, 9 * 4($24)
        sw      $10, 10 * 4($24)
        sw      $11, 11 * 4($24)
        sw      $12, 12 * 4($24)
        sw      $13, 13 * 4($24)
        sw      $14, 14 * 4($24)
        sw      $15, 15 * 4($24)
        sw      $16, 16 * 4($24)
        sw      $17, 17 * 4($24)
        sw      $18, 18 * 4($24)
        sw      $19, 19 * 4($24)
        sw      $20, 20 * 4($24)
        sw      $21, 21 * 4($24)
        s.d     $f0, 0 * 8($sp)
        s.d     $f1, 1 * 8($sp)
        s.d     $f2, 2 * 8($sp)
        s.d     $f3, 3 * 8($sp)
        s.d     $f4, 4 * 8($sp)
        s.d     $f5, 5 * 8($sp)
        s.d     $f6, 6 * 8($sp)
        s.d     $f7, 7 * 8($sp)
        s.d     $f8, 8 * 8($sp)
        s.d     $f9, 9 * 8($sp)
        s.d     $f10, 10 * 8($sp)
        s.d     $f11, 11 * 8($sp)
        s.d     $f12, 12 * 8($sp)
        s.d     $f13, 13 * 8($sp)
        s.d     $f14, 14 * 8($sp)
        s.d     $f15, 15 * 8($sp)
        s.d     $f16, 16 * 8($sp)
        s.d     $f17, 17 * 8($sp)
        s.d     $f18, 18 * 8($sp)
        s.d     $f19, 19 * 8($sp)
        s.d     $f20, 20 * 8($sp)
        s.d     $f21, 21 * 8($sp)
        s.d     $f22, 22 * 8($sp)
        s.d     $f23, 23 * 8($sp)
        s.d     $f24, 24 * 8($sp)
        s.d     $f25, 25 * 8($sp)
        s.d     $f26, 26 * 8($sp)
        s.d     $f27, 27 * 8($sp)
        s.d     $f28, 28 * 8($sp)
        s.d     $f29, 29 * 8($sp)
        s.d     $f30, 30 * 8($sp)
        s.d     $f31, 31 * 8($sp)
    /* Call the garbage collector */
        jal     caml_garbage_collection
    /* Restore all regs used by the code generator */
        addu    $24, $sp, 0x100
        lw      $2, 2 * 4($24)
        lw      $3, 3 * 4($24)
        lw      $4, 4 * 4($24)
        lw      $5, 5 * 4($24)
        lw      $6, 6 * 4($24)
        lw      $7, 7 * 4($24)
        lw      $8, 8 * 4($24)
        lw      $9, 9 * 4($24)
        lw      $10, 10 * 4($24)
        lw      $11, 11 * 4($24)
        lw      $12, 12 * 4($24)
        lw      $13, 13 * 4($24)
        lw      $14, 14 * 4($24)
        lw      $15, 15 * 4($24)
        lw      $16, 16 * 4($24)
        lw      $17, 17 * 4($24)
        lw      $18, 18 * 4($24)
        lw      $19, 19 * 4($24)
        lw      $20, 20 * 4($24)
        lw      $21, 21 * 4($24)
        l.d     $f0, 0 * 8($sp)
        l.d     $f1, 1 * 8($sp)
        l.d     $f2, 2 * 8($sp)
        l.d     $f3, 3 * 8($sp)
        l.d     $f4, 4 * 8($sp)
        l.d     $f5, 5 * 8($sp)
        l.d     $f6, 6 * 8($sp)
        l.d     $f7, 7 * 8($sp)
        l.d     $f8, 8 * 8($sp)
        l.d     $f9, 9 * 8($sp)
        l.d     $f10, 10 * 8($sp)
        l.d     $f11, 11 * 8($sp)
        l.d     $f12, 12 * 8($sp)
        l.d     $f13, 13 * 8($sp)
        l.d     $f14, 14 * 8($sp)
        l.d     $f15, 15 * 8($sp)
        l.d     $f16, 16 * 8($sp)
        l.d     $f17, 17 * 8($sp)
        l.d     $f18, 18 * 8($sp)
        l.d     $f19, 19 * 8($sp)
        l.d     $f20, 20 * 8($sp)
        l.d     $f21, 21 * 8($sp)
        l.d     $f22, 22 * 8($sp)
        l.d     $f23, 23 * 8($sp)
        l.d     $f24, 24 * 8($sp)
        l.d     $f25, 25 * 8($sp)
        l.d     $f26, 26 * 8($sp)
        l.d     $f27, 27 * 8($sp)
        l.d     $f28, 28 * 8($sp)
        l.d     $f29, 29 * 8($sp)
        l.d     $f30, 30 * 8($sp)
        l.d     $f31, 31 * 8($sp)
    /* Reload new allocation pointer and allocation limit */
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
    /* Reload return address */
        lw      $31, caml_last_return_address
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Adjust return address to restart the allocation sequence */
        subu    $31, $31, 16
    /* Return */
        .cpreturn
        addu    $sp, $sp, 0x160
        j       $31

        .end    caml_call_gc

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call

caml_c_call:
    /* Function to call is in $24 */
    /* Set up $gp, saving caller's $gp in callee-save register $19 */
        .cpsetup $25, $19, caml_c_call
    /* Preload addresses of interesting global variables
       in callee-save registers */
        la      $16, caml_last_return_address
        la      $17, caml_young_ptr
    /* Save return address, bottom of stack, alloc ptr, exn ptr */
        sw      $31, 0($16)     /* caml_last_return_address */
        sw      $sp, caml_bottom_of_stack
        sw      $22, 0($17)     /* caml_young_ptr */
        sw      $30, caml_exception_pointer
    /* Call C function */
        move    $25, $24
        jal     $24
    /* Reload return address, alloc ptr, alloc limit */
        lw      $31, 0($16)     /* caml_last_return_address */
        lw      $22, 0($17)     /* caml_young_ptr */
        lw      $23, caml_young_limit /* caml_young_limit */
    /* Zero caml_last_return_address, indicating we're back in Caml code */
        sw      $0, 0($16)      /* caml_last_return_address */
    /* Restore $gp and return */
        move    $gp, $19
        j       $31
        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .globl  stray_exn_handler
        .ent    caml_start_program
caml_start_program:
    /* Reserve space for callee-save registers */
        subu    $sp, $sp, 0x90
    /* Setup $gp */
        .cpsetup $25, 0x80, caml_start_program
    /* Load in $24 the code address to call */
        la      $24, caml_program
    /* Code shared with caml_callback* */
$103:
    /* Save return address */
        sd      $31, 0x88($sp)
    /* Save all callee-save registers */
        sd      $16, 0x0($sp)
        sd      $17, 0x8($sp)
        sd      $18, 0x10($sp)
        sd      $19, 0x18($sp)
        sd      $20, 0x20($sp)
        sd      $21, 0x28($sp)
        sd      $22, 0x30($sp)
        sd      $23, 0x38($sp)
        sd      $30, 0x40($sp)
        s.d     $f20, 0x48($sp)
        s.d     $f22, 0x50($sp)
        s.d     $f24, 0x58($sp)
        s.d     $f26, 0x60($sp)
        s.d     $f28, 0x68($sp)
        s.d     $f30, 0x70($sp)
    /* Set up a callback link on the stack. */
        subu    $sp, $sp, 16
        lw      $2, caml_bottom_of_stack
        sw      $2, 0($sp)
        lw      $3, caml_last_return_address
        sw      $3, 4($sp)
        lw      $4, caml_gc_regs
        sw      $4, 8($sp)
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        subu    $sp, $sp, 16
        lw      $30, caml_exception_pointer
        sw      $30, 0($sp)
        la      $2, $105
        sw      $2, 4($sp)
        sw      $gp, 8($sp)
        move    $30, $sp
    /* Reload allocation pointers */
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Call the Caml code */
        move    $25, $24
        jal     $24
$104:
    /* Pop the trap frame, restoring caml_exception_pointer */
        lw      $24, 0($sp)
        sw      $24, caml_exception_pointer
        addu    $sp, $sp, 16
$106:
    /* Pop the callback link, restoring the global variables */
        lw      $24, 0($sp)
        sw      $24, caml_bottom_of_stack
        lw      $25, 4($sp)
        sw      $25, caml_last_return_address
        lw      $24, 8($sp)
        sw      $24, caml_gc_regs
        addu    $sp, $sp, 16
    /* Update allocation pointer */
        sw      $22, caml_young_ptr
    /* Reload callee-save registers and return */
        ld      $31, 0x88($sp)
        ld      $16, 0x0($sp)
        ld      $17, 0x8($sp)
        ld      $18, 0x10($sp)
        ld      $19, 0x18($sp)
        ld      $20, 0x20($sp)
        ld      $21, 0x28($sp)
        ld      $22, 0x30($sp)
        ld      $23, 0x38($sp)
        ld      $30, 0x40($sp)
        l.d     $f20, 0x48($sp)
        l.d     $f22, 0x50($sp)
        l.d     $f24, 0x58($sp)
        l.d     $f26, 0x60($sp)
        l.d     $f28, 0x68($sp)
        l.d     $f30, 0x70($sp)
        .cpreturn
        addu    $sp, $sp, 0x90
        j       $31

    /* The trap handler: encode exception bucket as an exception result
       and return it */
$105:
        sw      $30, caml_exception_pointer
        or      $2, $2, 2
        b       $106

        .end    caml_start_program

/* Raise an exception from C */

        .globl  caml_raise_exception
        .ent    caml_raise_exception
caml_raise_exception:
    /* Setup $gp, discarding caller's $gp (we won't return) */
        .cpsetup $25, $24, caml_raise_exception
    /* Branch to exn handler */
        move    $2, $4
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
        lw      $sp, caml_exception_pointer
        lw      $30, 0($sp)
        lw      $24, 4($sp)
        lw      $gp, 8($sp)
        addu    $sp, $sp, 16
        j       $24

        .end    caml_raise_exception

/* Callback from C to Caml */

        .globl  caml_callback_exn
        .ent    caml_callback_exn
caml_callback_exn:
        subu    $sp, $sp, 0x90
        .cpsetup $25, 0x80, caml_callback_exn
    /* Initial shuffling of arguments */
        move    $9, $4          /* closure */
        move    $8, $5          /* argument */
        lw      $24, 0($4)      /* code pointer */
        b       $103
        .end    caml_callback_exn

        .globl  caml_callback2_exn
        .ent    caml_callback2_exn
caml_callback2_exn:
        subu    $sp, $sp, 0x90
        .cpsetup $25, 0x80, caml_callback2_exn
    /* Initial shuffling of arguments */
        move    $10, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        la      $24, caml_apply2        /* code pointer */
        b       $103

        .end    caml_callback2_exn

        .globl  caml_callback3_exn
        .ent    caml_callback3_exn
caml_callback3_exn:
        subu    $sp, $sp, 0x90
        .cpsetup $25, 0x80, caml_callback3_exn
    /* Initial shuffling of arguments */
        move    $11, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        move    $10, $7                 /* third argument */
        la      $24, caml_apply3        /* code pointer */
        b       $103

        .end    caml_callback3_exn

/* Glue code to call [caml_array_bound_error] */

        .globl  caml_ml_array_bound_error
        .ent    caml_ml_array_bound_error

caml_ml_array_bound_error:
    /* Setup $gp, discarding caller's $gp (we won't return) */
        .cpsetup $25, $24, caml_ml_array_bound_error
        la      $24, caml_array_bound_error
        jal     caml_c_call             /* never returns */

        .end    caml_ml_array_bound_error

        .rdata
        .globl  caml_system__frametable
caml_system__frametable:
        .word   1               /* one descriptor */
        .word   $104            /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots here */
