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

/* Asm part of the runtime system, Mips processor */

/* Allocation */

        .text
        .globl  caml_call_gc
#ifndef _PIC
        .globl  caml_alloc1
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_alloc
#endif
        .ent    caml_call_gc

caml_call_gc:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
    /* Record return address and adjust it to point back to
       the beginning of the allocation sequence */
        sw      $31, caml_last_return_address
        subu    $31, $31, 16
    /* Don't request any allocation, will be redone at return */
        li      $24, 0
        b       $110

#ifndef _PIC

/* caml_alloc* : all code generator registers preserved. */

caml_alloc1:
        subu    $22, $22, 8
        bltu    $22, $23, $100
        j       $31
$100:   li      $24, 8
        b       caml_call_gc_internal

caml_alloc2:
        subu    $22, $22, 12
        bltu    $22, $23, $101
        j       $31
$101:   li      $24, 12
        b       caml_call_gc_internal

caml_alloc3:
        subu    $22, $22, 16
        bltu    $22, $23, $102
        j       $31
$102:   li      $24, 16
        b       caml_call_gc_internal

caml_alloc:
        subu    $22, $22, $24
        bltu    $22, $23, caml_call_gc_internal
        j       $31
        
caml_call_gc_internal:
    /* Record return address */
        sw      $31, caml_last_return_address
#endif

$110:
    /* Record lowest stack address */
        sw      $sp, caml_bottom_of_stack
    /* Reserve stack space for the registers and the call to C */
        subu    $sp, $sp, 0x110
    /* 0x110 = 32*4 (int regs) + 32*4 (float regs) + 16 (call) */
    /* Save actual retaddr, $gp, requested size. */
        sw      $31, 0x10C($sp)
        sw      $24, 0x108($sp)
#ifdef _PIC
        .cprestore 0x104
#endif
    /* Save pointer to register array */
        addu    $24, $sp, 0x90
        sw      $24, caml_gc_regs
    /* Save current allocation pointer for debugging purposes */
        sw      $22, young_ptr
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
        s.d     $f0, 16 + 0 * 4($sp)
        s.d     $f2, 16 + 2 * 4($sp)
        s.d     $f4, 16 + 4 * 4($sp)
        s.d     $f6, 16 + 6 * 4($sp)
        s.d     $f8, 16 + 8 * 4($sp)
        s.d     $f12, 16 + 12 * 4($sp)
        s.d     $f14, 16 + 14 * 4($sp)
        s.d     $f16, 16 + 16 * 4($sp)
        s.d     $f18, 16 + 18 * 4($sp)
        s.d     $f20, 16 + 20 * 4($sp)
        s.d     $f22, 16 + 22 * 4($sp)
        s.d     $f24, 16 + 24 * 4($sp)
        s.d     $f26, 16 + 26 * 4($sp)
        s.d     $f28, 16 + 28 * 4($sp)
        s.d     $f30, 16 + 30 * 4($sp)
    /* Call the garbage collector */
        jal     garbage_collection
    /* Restore all regs used by the code generator */
        addu    $24, $sp, 0x90
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
        l.d     $f0, 16 + 0 * 4($sp)
        l.d     $f2, 16 + 2 * 4($sp)
        l.d     $f4, 16 + 4 * 4($sp)
        l.d     $f6, 16 + 6 * 4($sp)
        l.d     $f8, 16 + 8 * 4($sp)
        l.d     $f12, 16 + 12 * 4($sp)
        l.d     $f14, 16 + 14 * 4($sp)
        l.d     $f16, 16 + 16 * 4($sp)
        l.d     $f18, 16 + 18 * 4($sp)
        l.d     $f20, 16 + 20 * 4($sp)
        l.d     $f22, 16 + 22 * 4($sp)
        l.d     $f24, 16 + 24 * 4($sp)
        l.d     $f26, 16 + 26 * 4($sp)
        l.d     $f28, 16 + 28 * 4($sp)
        l.d     $f30, 16 + 30 * 4($sp)
    /* Reload new allocation pointer and allocation limit */
        lw      $22, young_ptr
        lw      $23, young_limit
    /* Allocate space for the block */
        lw      $24, 0x108($sp)
        subu    $22, $22, $24
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Return to caller */
        lw      $31, 0x10C($sp)
        addu    $sp, $sp, 0x110
        j       $31

        .end    caml_call_gc

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call

caml_c_call:
    /* Function to call is in $24 */
#ifndef _PIC
    /* Record lowest stack address and return address */
        sw      $31, caml_last_return_address
        sw      $sp, caml_bottom_of_stack
    /* Make the exception handler and alloc ptr available to the C code */
        sw      $22, young_ptr
        sw      $30, caml_exception_pointer
    /* Call the function */
        jal     $24
    /* Reload alloc ptr and alloc limit */
        lw      $22, young_ptr
        lw      $23, young_limit
    /* Reload return address */
        lw      $31, caml_last_return_address
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Return */
        j       $31
#else
    /* Slightly optimized form of the above when referencing
       global variables is expensive */
        .set    noreorder
        .cpload $25
        .set    reorder
        la      $16, caml_last_return_address
        la      $17, young_ptr
        la      $18, young_limit
        sw      $31, 0($16)     /* caml_last_return_address */
        sw      $sp, caml_bottom_of_stack
        sw      $22, 0($17)     /* young_ptr */
        sw      $30, caml_exception_pointer
        move    $25, $24
        jal     $24
        lw      $31, 0($16)     /* caml_last_return_address */
        lw      $22, 0($17)     /* young_ptr */
        lw      $23, 0($18)     /* young_limit */
        sw      $0, 0($16)      /* caml_last_return_address */
        j       $31
#endif
        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .globl  stray_exn_handler
        .ent    caml_start_program
caml_start_program:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
        la      $24, caml_program

    /* Code shared with callback* */
$103:
    /* Save return address */
        subu    $sp, $sp, 96
        sw      $31, 88($sp)
    /* Save all callee-save registers */
        sw      $16, 0($sp)
        sw      $17, 4($sp)
        sw      $18, 8($sp)
        sw      $19, 12($sp)
        sw      $20, 16($sp)
        sw      $21, 20($sp)
        sw      $22, 24($sp)
        sw      $23, 28($sp)
        sw      $30, 32($sp)
        s.d     $f20, 40($sp)
        s.d     $f22, 48($sp)
        s.d     $f24, 56($sp)
        s.d     $f26, 64($sp)
        s.d     $f28, 72($sp)
        s.d     $f30, 80($sp)
    /* Set up a callback link on the stack. */
        subu    $sp, $sp, 16
        lw      $2, caml_bottom_of_stack
        sw      $2, 0($sp)
        lw      $3, caml_last_return_address
        sw      $3, 4($sp)
        lw      $4, caml_gc_regs
        sw      $4, 8($sp)
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        subu    $sp, $sp, 8
        lw      $30, caml_exception_pointer
        sw      $30, 0($sp)
        la      $2, $105
        sw      $2, 4($sp)
        move    $30, $sp
    /* Reload allocation pointers */
        lw      $22, young_ptr
        lw      $23, young_limit
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Call the Caml code */
#ifdef _PIC
        move    $25, $24
#endif
$104:   jal     $24
#ifdef _PIC
    /* Reload $gp based on return address */
        .set    noreorder
        .cpload $31
        .set    reorder
#endif
    /* Pop the trap frame, restoring caml_exception_pointer */
        lw      $24, 0($sp)
        sw      $24, caml_exception_pointer
        addu    $sp, $sp, 8
    /* Pop the callback link, restoring the global variables */
        lw      $24, 0($sp)
        sw      $24, caml_bottom_of_stack
        lw      $25, 4($sp)
        sw      $25, caml_last_return_address
        lw      $24, 8($sp)
        sw      $24, caml_gc_regs
        addu    $sp, $sp, 16
    /* Update allocation pointer */
        sw      $22, young_ptr
    /* Reload callee-save registers and return */
        lw      $31, 88($sp)
        lw      $16, 0($sp)
        lw      $17, 4($sp)
        lw      $18, 8($sp)
        lw      $19, 12($sp)
        lw      $20, 16($sp)
        lw      $21, 20($sp)
        lw      $22, 24($sp)
        lw      $23, 28($sp)
        lw      $30, 32($sp)
        l.d     $f20, 40($sp)
        l.d     $f22, 48($sp)
        l.d     $f24, 56($sp)
        l.d     $f26, 64($sp)
        l.d     $f28, 72($sp)
        l.d     $f30, 80($sp)
        addu    $sp, $sp, 96
        j       $31
        
    /* The trap handler: re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
$105:
#ifdef _PIC
    /* Reload $gp based on trap address (still in $25) */
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
        sw      $22, young_ptr
        sw      $30, caml_exception_pointer
        lw      $24, 0($sp)
        sw      $24, caml_bottom_of_stack
        lw      $25, 4($sp)
        sw      $25, caml_last_return_address
        lw      $24, 8($sp)
        sw      $24, caml_gc_regs
        addu    $sp, $sp, 16
        move    $4, $2          /* bucket as first argument */
        jal     mlraise         /* never returns */

        .end    caml_start_program

/* Raise an exception from C */

        .globl  raise_caml_exception
        .ent    raise_caml_exception
raise_caml_exception:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
        move    $2, $4
        lw      $22, young_ptr
        lw      $23, young_limit
        lw      $sp, caml_exception_pointer
        lw      $30, 0($sp)
        lw      $24, 4($sp)
        addu    $sp, $sp, 8
        j       $24

        .end    raise_caml_exception

/* Callback from C to Caml */

        .globl  callback
        .ent    callback
callback:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
    /* Initial shuffling of arguments */
        move    $9, $4          /* closure */
        move    $8, $5          /* argument */
        lw      $24, 0($4)      /* code pointer */
        b       $103
        .end    callback

        .globl  callback2
        .ent    callback2
callback2:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
    /* Initial shuffling of arguments */
        move    $10, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        la      $24, caml_apply2        /* code pointer */
        b       $103

        .end    callback2

        .globl  callback3
        .ent    callback3
callback3:
#ifdef _PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
    /* Initial shuffling of arguments */
        move    $11, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        move    $10, $7                 /* third argument */
        la      $24, caml_apply3        /* code pointer */
        b       $103

        .end    callback3

/* Glue code to call array_bound_error */

        .globl  caml_array_bound_error
        .ent    caml_array_bound_error

caml_array_bound_error:
#ifdef PIC
        .set    noreorder
        .cpload $25
        .set    reorder
#endif
        la      $24, array_bound_error
        jal     caml_c_call             /* never returns */
        .end    caml_array_bound_error

        .rdata
        .globl  system_frametable
system_frametable:
        .word   1               /* one descriptor */
        .word   $104 + 8        /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots here */
