/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, Alpha processor */

        .comm   young_start 8
        .comm   young_end 8
        .comm   young_ptr 8
        .comm   gc_entry_regs 8 * 32
        .comm   gc_entry_float_regs 8 * 32
        .comm   remembered_ptr 8
        .comm   remembered_end 8

        .sdata
        .globl  caml_top_of_stack
        .globl  caml_bottom_of_stack
        .globl  caml_last_return_address
        .globl  caml_exception_pointer
        .globl  caml_saved_gp
caml_top_of_stack:              .quad   0
caml_bottom_of_stack:           .quad   0
caml_last_return_address:       .quad   0
caml_exception_pointer:         .quad   0
caml_saved_gp:                  .quad   0

/* These are offsets relative to caml_bottom_of_stack */

#define Ofs_last_return_address 8
#define Ofs_exception_pointer 16
#define Ofs_saved_gp 24

#define SAVE_ALL_REGS \
        lda     $24, gc_entry_regs; \
        stq     $0, 0 * 8 ($24); \
        stq     $1, 1 * 8 ($24); \
        stq     $2, 2 * 8 ($24); \
        stq     $3, 3 * 8 ($24); \
        stq     $4, 4 * 8 ($24); \
        stq     $5, 5 * 8 ($24); \
        stq     $6, 6 * 8 ($24); \
        stq     $7, 7 * 8 ($24); \
        stq     $8, 8 * 8 ($24); \
        stq     $9, 9 * 8 ($24); \
        stq     $10, 10 * 8 ($24); \
        stq     $11, 11 * 8 ($24); \
        stq     $12, 12 * 8 ($24); \
        stq     $16, 16 * 8 ($24); \
        stq     $17, 17 * 8 ($24); \
        stq     $18, 18 * 8 ($24); \
        stq     $19, 19 * 8 ($24); \
        stq     $20, 20 * 8 ($24); \
        stq     $21, 21 * 8 ($24); \
        stq     $22, 22 * 8 ($24); \
        lda     $24, gc_entry_float_regs; \
        stt     $f0, 0 * 8 ($24); \
        stt     $f1, 1 * 8 ($24); \
        stt     $f10, 10 * 8 ($24); \
        stt     $f11, 11 * 8 ($24); \
        stt     $f12, 12 * 8 ($24); \
        stt     $f13, 13 * 8 ($24); \
        stt     $f14, 14 * 8 ($24); \
        stt     $f15, 15 * 8 ($24); \
        stt     $f16, 16 * 8 ($24); \
        stt     $f17, 17 * 8 ($24); \
        stt     $f18, 18 * 8 ($24); \
        stt     $f19, 19 * 8 ($24); \
        stt     $f20, 20 * 8 ($24); \
        stt     $f21, 21 * 8 ($24); \
        stt     $f22, 22 * 8 ($24); \
        stt     $f23, 23 * 8 ($24); \
        stt     $f24, 24 * 8 ($24); \
        stt     $f25, 25 * 8 ($24); \
        stt     $f26, 26 * 8 ($24); \
        stt     $f27, 27 * 8 ($24); \
        stt     $f28, 28 * 8 ($24); \
        stt     $f29, 29 * 8 ($24)

#define LOAD_ALL_REGS \
        lda     $24, gc_entry_regs; \
        ldq     $0, 0 * 8 ($24); \
        ldq     $1, 1 * 8 ($24); \
        ldq     $2, 2 * 8 ($24); \
        ldq     $3, 3 * 8 ($24); \
        ldq     $4, 4 * 8 ($24); \
        ldq     $5, 5 * 8 ($24); \
        ldq     $6, 6 * 8 ($24); \
        ldq     $7, 7 * 8 ($24); \
        ldq     $8, 8 * 8 ($24); \
        ldq     $9, 9 * 8 ($24); \
        ldq     $10, 10 * 8 ($24); \
        ldq     $11, 11 * 8 ($24); \
        ldq     $12, 12 * 8 ($24); \
        ldq     $16, 16 * 8 ($24); \
        ldq     $17, 17 * 8 ($24); \
        ldq     $18, 18 * 8 ($24); \
        ldq     $19, 19 * 8 ($24); \
        ldq     $20, 20 * 8 ($24); \
        ldq     $21, 21 * 8 ($24); \
        ldq     $22, 22 * 8 ($24); \
        lda     $24, gc_entry_float_regs; \
        ldt     $f0, 0 * 8 ($24); \
        ldt     $f1, 1 * 8 ($24); \
        ldt     $f10, 10 * 8 ($24); \
        ldt     $f11, 11 * 8 ($24); \
        ldt     $f12, 12 * 8 ($24); \
        ldt     $f13, 13 * 8 ($24); \
        ldt     $f14, 14 * 8 ($24); \
        ldt     $f15, 15 * 8 ($24); \
        ldt     $f16, 16 * 8 ($24); \
        ldt     $f17, 17 * 8 ($24); \
        ldt     $f18, 18 * 8 ($24); \
        ldt     $f19, 19 * 8 ($24); \
        ldt     $f20, 20 * 8 ($24); \
        ldt     $f21, 21 * 8 ($24); \
        ldt     $f22, 22 * 8 ($24); \
        ldt     $f23, 23 * 8 ($24); \
        ldt     $f24, 24 * 8 ($24); \
        ldt     $f25, 25 * 8 ($24); \
        ldt     $f26, 26 * 8 ($24); \
        ldt     $f27, 27 * 8 ($24); \
        ldt     $f28, 28 * 8 ($24); \
        ldt     $f29, 29 * 8 ($24)

/* Allocation */

        .text
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_alloc
        .globl  caml_call_gc

/* caml_alloc* : all code generator registers preserved,
   $gp preserved, $27 not valid on entry */

        .globl  caml_alloc1
        .ent    caml_alloc1
        .align  3
caml_alloc1:
        subq    $13, 16, $13
        cmpult  $13, $14, $25
        bne     $25, $100
        ret     ($26)
$100:   ldiq    $25, 16
        br      caml_call_gc
        .end    caml_alloc1

        .globl  caml_alloc2
        .ent    caml_alloc2
        .align  3
caml_alloc2:
        subq    $13, 24, $13
        cmpult  $13, $14, $25
        bne     $25, $101
        ret     ($26)
$101:   ldiq    $25, 24
        br      caml_call_gc
        .end    caml_alloc2

        .globl  caml_alloc3
        .ent    caml_alloc3
        .align  3
caml_alloc3:
        subq    $13, 32, $13
        cmpult  $13, $14, $25
        bne     $25, $102
        ret     ($26)
$102:   ldiq    $25, 32
        br      caml_call_gc
        .end    caml_alloc3

        .globl  caml_alloc
        .ent    caml_alloc
        .align  3
caml_alloc:
        subq    $13, $25, $13
        .set    noat
        cmpult  $13, $14, $at
        bne     $at, caml_call_gc
        .set    at
        ret     ($26)
        .end    caml_alloc
        
        .globl  caml_call_gc
        .ent    caml_call_gc
        .align  3
caml_call_gc:
        lda     $sp, -32($sp)
        stq     $26, 0($sp)
        stq     $gp, 8($sp)
        stq     $25, 16($sp)
    /* Rebuild $gp */
        br      $27, $103
$103:   ldgp    $gp, 0($27)
    /* Record lowest stack address and return address */
        ldq     $24, 0($sp)
        stq     $24, caml_last_return_address
        lda     $24, 32($sp)
        stq     $24, caml_bottom_of_stack
    /* Save current allocation pointer for debugging purposes */
        stq     $13, young_ptr
    /* Save all regs used by the code generator in the arrays
    /* gc_entry_regs and gc_entry_float_regs. */
        SAVE_ALL_REGS
    /* Call the garbage collector */
        jsr     minor_collection
    /* Restore all regs used by the code generator */
        ldgp    $gp, 0($26)
        LOAD_ALL_REGS
    /* Reload new allocation pointer and allocation limit */
        ldq     $13, young_ptr
        ldq     $14, young_start
    /* Allocate space for the block */
        ldq     $25, 16($sp)
        subq    $13, $25, $13
    /* Return to caller */
        ldq     $26, 0($sp)
        ldq     $gp, 8($sp)
        lda     $sp, 32($sp)
        ret     ($26)

        .end    caml_call_gc

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call
        .align  3
caml_c_call:
    /* Function to call is in $27 */
        mov     $gp, $24
    /* Rebuild $gp */
        br      $25, $105
$105:   ldgp    $gp, 0($25)
    /* Record lowest stack address, return address, and caller's $gp */
        lda     $25, caml_bottom_of_stack
        stq     $sp, 0($25)
        stq     $26, Ofs_last_return_address($25)
        stq     $24, Ofs_saved_gp($25)
    /* Make the exception handler and alloc ptr available to the C code */
        stq     $13, young_ptr
        stq     $15, Ofs_exception_pointer($25)
    /* Call the function */
        jsr     ($27)
    /* Reload alloc ptr */
        ldgp    $gp, 0($26)
        ldq     $13, young_ptr
    /* Reload $gp and return address */
        lda     $25, caml_bottom_of_stack
        ldq     $26, Ofs_last_return_address($25)
        ldq     $gp, Ofs_saved_gp($25)
    /* Return */
        ret     ($26)

        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .globl  stray_exn_handler
        .ent    caml_start_program
        .align  3
caml_start_program:
        ldgp    $gp, 0($27)
	lda	$sp, -128($sp)
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
    /* Build an exception handler */
	lda	$sp, -16($sp)
	lda	$0, stray_exn_handler
	stq	$0, 8($sp)
	mov	$sp, $15
    /* Record highest stack address */
        stq     $sp, caml_top_of_stack
    /* Initialize allocation registers */
	ldq	$13, young_ptr
	ldq	$14, young_start
    /* Go for it */
        jsr     caml_program
    /* Pop handler */
        lda     $sp, 16($sp)
    /* Return with zero code */
        clr     $0
    /* Restore registers */
stray_exn_handler:
        ldq     $26, 0($sp)
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
        lda     $sp, 128($sp)
        ret     ($26)

        .end    caml_start_program

/* Raise an exception from C */

        .globl  raise_caml_exception
        .ent    raise_caml_exception
        .align  3
raise_caml_exception:
        ldgp    $gp, 0($27)
        mov     $16, $0
        ldq     $13, young_ptr
        ldq     $14, young_start
        ldq     $sp, caml_exception_pointer
        ldq     $15, 0($sp)
        ldq     $27, 8($sp)
        lda     $sp, 16($sp)
        jmp     $25, ($27)      /* Keep retaddr in $25 to help debugging */
        .end    raise_caml_exception

/* Glue code to jump to array_bound_error after reinitializing $gp */

        .globl  call_array_bound_error
        .ent    call_array_bound_error
        .align  3
call_array_bound_error:
    /* Rebuild $gp */
        br      $27, $106
$106:   ldgp    $gp, 0($27)
    /* Branch to array_bound_error -- never returns */
        jsr     array_bound_error
        .end    call_array_bound_error

/* Callback from C to Caml */

        .globl  callback
        .ent    callback
        .align  3
callback:
    /* Initial shuffling of arguments */
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $25, $17        /* environment */
        ldq     $25, 0($25)     /* code pointer */
$107:
    /* Save return address */
	lda	$sp, -128($sp)
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
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        lda     $sp, -16($sp)
        ldq     $15, caml_exception_pointer
        stq     $15, 0($sp)
        lda     $0, $109
        stq     $0, 8($sp)
        mov     $sp, $15
    /* Set up a callback link on the stack. Also save there
       everything put in global variables by caml_c_call. */
        lda     $sp, -32($sp)
        ldq     $0, caml_bottom_of_stack
        stq     $0, 0($sp)
        ldq     $1, caml_last_return_address
        stq     $1, 8($sp)
        ldq     $2, caml_saved_gp
        stq     $2, 16($sp)
    /* Reload allocation pointers */
        ldq     $13, young_ptr
        ldq     $14, young_start
    /* Call the Caml code */
$108:   jsr     ($25)
    /* Reload $gp */
        bic     $26, 1, $26     /* return address may have "scanned" bit set */
        ldgp    $gp, 4($26)
    /* Restore the global variables used by caml_c_call */
        ldq     $25, 8($sp)
        stq     $25, caml_last_return_address
        ldq     $22, 16($sp)
        stq     $22, caml_saved_gp
        lda     $sp, 32($sp)
    /* Pop the trap frame, restoring caml-exception_pointer */
        ldq     $15, 0($sp)
        stq     $15, caml_exception_pointer
        lda     $sp, 16($sp)
    /* Update allocation pointer */
        stq     $13, young_ptr
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
    /* The trap handler: re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
$109:   ldgp    $gp, 0($27)
        stq     $13, young_ptr
        stq     $15, caml_exception_pointer
        mov     $0, $16         /* bucket as first argument */
        jsr     mlraise         /* never returns */

        .end    callback

        .globl  callback2
        .ent    callback2
        .align  3
callback2:
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $18, $17        /* second arg */
        mov     $25, $18        /* environment */
        lda     $25, caml_apply2
        br      $107
        .end    callback2

        .globl  callback3
        .ent    callback3
        .align  3
callback3:
        ldgp    $gp, 0($27)
        mov     $16, $25
        mov     $17, $16        /* first arg */
        mov     $18, $17        /* second arg */
        mov     $19, $18        /* third arg */
        mov     $25, $19        /* environment */
        lda     $25, caml_apply3
        br      $107
        .end    callback3

        .rdata
        .globl  system_frametable
system_frametable:
        .quad   1               /* one descriptor */
        .quad   $108 + 4        /* return address into callback */
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
        .align  3
