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

        .globl  call_gen_code
        .ent    call_gen_code
call_gen_code:
	subu    $sp, $sp, 88
        sw      $31, 84($sp)
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
        s.d     $f20, 36($sp)
        s.d     $f22, 44($sp)
        s.d     $f24, 52($sp)
        s.d     $f26, 60($sp)
        s.d     $f28, 68($sp)
        s.d     $f30, 76($sp)
    /* Shuffle arguments */
        move    $8, $5
        move    $9, $6
        move    $10, $7
#ifdef _PIC
        move    $25, $4
#endif
        jal     $4
    /* Restore registers */
        lw      $31, 84($sp)
        lw      $16, 0($sp)
        lw      $17, 4($sp)
        lw      $18, 8($sp)
        lw      $19, 12($sp)
        lw      $20, 16($sp)
        lw      $21, 20($sp)
        lw      $22, 24($sp)
        lw      $23, 28($sp)
        lw      $30, 32($sp)
        l.d     $f20, 36($sp)
        l.d     $f22, 44($sp)
        l.d     $f24, 52($sp)
        l.d     $f26, 60($sp)
        l.d     $f28, 68($sp)
        l.d     $f30, 76($sp)
        addu    $sp, $sp, 88
        j       $31

        .end    call_gen_code

/* Call a C function */

        .globl  caml_c_call
        .ent    caml_c_call
caml_c_call:
#ifdef _PIC
        move    $25, $24
#endif
        j       $24
        .end    caml_c_call