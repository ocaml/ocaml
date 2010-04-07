/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id: amd64.S 5634 2003-06-30 08:28:48Z xleroy $ */

#ifdef SYS_macosx
#define ALIGN 4
#else
#define ALIGN 16
#endif

        .globl  _call_gen_code
        .align  ALIGN
_call_gen_code:
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	movq	%rdi, %r10
	movq	%rsi, %rax
	movq	%rdx, %rbx
	movq	%rcx, %rdi
	movq	%r8, %rsi
        call	*%r10
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
        ret

        .globl  caml_c_call
        .align  ALIGN
caml_c_call:
        jmp     *%rax

#ifdef SYS_macosx
	.literal16
#else
	.section	.rodata.cst8,"aM",@progbits,8
#endif
        .globl  _caml_negf_mask
        .align  ALIGN
_caml_negf_mask:
	.quad	0x8000000000000000, 0
        .globl  _caml_absf_mask
        .align  ALIGN
_caml_absf_mask:
	.quad	0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8
