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

#define STORE sd
#define LOAD ld

        .globl  call_gen_code
        .align  2
call_gen_code:
    /* Set up stack frame and save callee-save registers */
        ADDI    sp, sp, -208
        STORE   ra, 192(sp)
        STORE   s0, 0(sp)
        STORE   s1, 8(sp)
        STORE   s2, 16(sp)
        STORE   s3, 24(sp)
        STORE   s4, 32(sp)
        STORE   s5, 40(sp)
        STORE   s6, 48(sp)
        STORE   s7, 56(sp)
        STORE   s8, 64(sp)
        STORE   s9, 72(sp)
        STORE   s10, 80(sp)
        STORE   s11, 88(sp)
        fsd     fs0, 96(sp)
        fsd     fs1, 104(sp)
        fsd     fs2, 112(sp)
        fsd     fs3, 120(sp)
        fsd     fs4, 128(sp)
        fsd     fs5, 136(sp)
        fsd     fs6, 144(sp)
        fsd     fs7, 152(sp)
        fsd     fs8, 160(sp)
        fsd     fs9, 168(sp)
        fsd     fs10, 176(sp)
        fsd     fs11, 184(sp)
    /* Shuffle arguments */
        mv      t0, a0
        mv      a0, a1
        mv      a1, a2
        mv      a2, a3
        mv      a3, a4
    /* Call generated asm */
        jalr    t0
    /* Reload callee-save registers and return address */
        LOAD    ra, 192(sp)
        LOAD    s0, 0(sp)
        LOAD    s1, 8(sp)
        LOAD    s2, 16(sp)
        LOAD    s3, 24(sp)
        LOAD    s4, 32(sp)
        LOAD    s5, 40(sp)
        LOAD    s6, 48(sp)
        LOAD    s7, 56(sp)
        LOAD    s8, 64(sp)
        LOAD    s9, 72(sp)
        LOAD    s10, 80(sp)
        LOAD    s11, 88(sp)
        fld     fs0, 96(sp)
        fld     fs1, 104(sp)
        fld     fs2, 112(sp)
        fld     fs3, 120(sp)
        fld     fs4, 128(sp)
        fld     fs5, 136(sp)
        fld     fs6, 144(sp)
        fld     fs7, 152(sp)
        fld     fs8, 160(sp)
        fld     fs9, 168(sp)
        fld     fs10, 176(sp)
        fld     fs11, 184(sp)
        addi    sp, sp, 208
        ret

        .globl  caml_c_call
        .align  2
caml_c_call:
        jr      t2
