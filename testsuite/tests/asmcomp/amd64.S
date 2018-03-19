/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifdef SYS_macosx
#define ALIGN 4
#else
#define ALIGN 16
#endif

#ifdef SYS_macosx
#define CALL_GEN_CODE _call_gen_code
#else
#define CALL_GEN_CODE call_gen_code
#endif

        .globl  CALL_GEN_CODE
        .align  ALIGN
CALL_GEN_CODE:
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
        movq    %rdi, %r10
        movq    %rsi, %rax
        movq    %rdx, %rbx
        movq    %rcx, %rdi
        movq    %r8, %rsi
        call    *%r10
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
        ret
