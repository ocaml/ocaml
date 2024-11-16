/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>                 */
/*                                                                        */
/*   Copyright 2019 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define STORE st.d
#define LOAD ld.d

        .globl  call_gen_code
        .align  2
call_gen_code:
    /* Set up stack frame and save callee-save registers */
        addi.d    $sp, $sp, -208
        STORE   $ra, $sp, 192
        STORE   $s0, $sp, 0
        STORE   $s1, $sp, 8
        STORE   $s2, $sp, 16
        STORE   $s3, $sp, 24
        STORE   $s4, $sp, 32
        STORE   $s5, $sp, 40
        STORE   $s6, $sp, 48
        STORE   $s7, $sp, 56
        STORE   $s8, $sp, 64
        fst.d     $fs0, $sp, 96
        fst.d     $fs1, $sp, 104
        fst.d     $fs2, $sp, 112
        fst.d     $fs3, $sp, 120
        fst.d     $fs4, $sp, 128
        fst.d     $fs5, $sp, 136
        fst.d     $fs6, $sp, 144
        fst.d     $fs7, $sp, 152
    /* Shuffle arguments */
        move      $t0, $a0
        move      $a0, $a1
        move      $a1, $a2
        move      $a2, $a3
        move      $a3, $a4
    /* Call generated asm */
        jirl    $ra, $t0, 0
    /* Reload callee-save registers and return address */
        LOAD    $ra, $sp, 192
        LOAD    $s0, $sp, 0
        LOAD    $s1, $sp, 8
        LOAD    $s2, $sp ,16
        LOAD    $s3, $sp ,24
        LOAD    $s4, $sp ,32
        LOAD    $s5, $sp ,40
        LOAD    $s6, $sp ,48
        LOAD    $s7, $sp ,56
        LOAD    $s8, $sp ,64
        fld.d     $fs0, $sp, 96
        fld.d     $fs1, $sp, 104
        fld.d     $fs2, $sp, 112
        fld.d     $fs3, $sp, 120
        fld.d     $fs4, $sp, 128
        fld.d     $fs5, $sp, 136
        fld.d     $fs6, $sp, 144
        fld.d     $fs7, $sp, 152
        addi.d    $sp, $sp, 208
        jr      $ra

        .globl  caml_c_call
        .align  2
caml_c_call:
        jr      $t2
