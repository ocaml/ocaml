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

/* $Id$ */

        .globl  call_gen_code
        .align  16
call_gen_code:
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
        .align  16
caml_c_call:
        jmp     *%rax

	.section	.rodata.cst8,"aM",@progbits,8
        .globl  caml_negf_mask
        .align  16
caml_negf_mask:
	.quad	0x8000000000000000, 0
        .globl  caml_absf_mask
        .align  16
caml_absf_mask:
	.quad	0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8
