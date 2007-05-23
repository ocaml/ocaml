/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

#define FUNCTION_ALIGN 4

#define FUNCTION(name) \
        .globl name; \
        .type  name,@function; \
        .align FUNCTION_ALIGN; \
        name:

        .text

/* Allocation */

FUNCTION(caml_call_gc)
    /* Record lowest stack address and return address */
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
.L105:  
    /* Save caml_young_ptr, caml_exception_pointer */
	movq	%r15, caml_young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Build array of registers, save it into caml_gc_regs */
        pushq   %r13
        pushq   %r12
        pushq   %rbp
        pushq   %r11
        pushq   %r10
        pushq   %r9
        pushq   %r8
        pushq   %rcx
        pushq   %rdx
        pushq   %rsi
        pushq   %rdi
        pushq   %rbx
        pushq   %rax
        movq    %rsp, caml_gc_regs(%rip)
    /* Save floating-point registers */
        subq    $(16*8), %rsp
        movlpd  %xmm0, 0*8(%rsp)
        movlpd  %xmm1, 1*8(%rsp)
        movlpd  %xmm2, 2*8(%rsp)
        movlpd  %xmm3, 3*8(%rsp)
        movlpd  %xmm4, 4*8(%rsp)
        movlpd  %xmm5, 5*8(%rsp)
        movlpd  %xmm6, 6*8(%rsp)
        movlpd  %xmm7, 7*8(%rsp)
        movlpd  %xmm8, 8*8(%rsp)
        movlpd  %xmm9, 9*8(%rsp)
        movlpd  %xmm10, 10*8(%rsp)
        movlpd  %xmm11, 11*8(%rsp)
        movlpd  %xmm12, 12*8(%rsp)
        movlpd  %xmm13, 13*8(%rsp)
        movlpd  %xmm14, 14*8(%rsp)
        movlpd  %xmm15, 15*8(%rsp)
    /* Call the garbage collector */
        call    caml_garbage_collection
    /* Restore all regs used by the code generator */
        movlpd  0*8(%rsp), %xmm0
        movlpd  1*8(%rsp), %xmm1
        movlpd  2*8(%rsp), %xmm2
        movlpd  3*8(%rsp), %xmm3
        movlpd  4*8(%rsp), %xmm4
        movlpd  5*8(%rsp), %xmm5
        movlpd  6*8(%rsp), %xmm6
        movlpd  7*8(%rsp), %xmm7
        movlpd  8*8(%rsp), %xmm8
        movlpd  9*8(%rsp), %xmm9
        movlpd  10*8(%rsp), %xmm10
        movlpd  11*8(%rsp), %xmm11
        movlpd  12*8(%rsp), %xmm12
        movlpd  13*8(%rsp), %xmm13
        movlpd  14*8(%rsp), %xmm14
        movlpd  15*8(%rsp), %xmm15
        addq    $(16*8), %rsp
        popq    %rax
        popq    %rbx
        popq    %rdi
        popq    %rsi
        popq    %rdx
        popq    %rcx
        popq    %r8
        popq    %r9
        popq    %r10
        popq    %r11
        popq    %rbp
        popq    %r12
        popq    %r13
    /* Restore caml_young_ptr, caml_exception_pointer */
	movq	caml_young_ptr(%rip), %r15
	movq	caml_exception_pointer(%rip), %r14
    /* Return to caller */
        ret

FUNCTION(caml_alloc1)
        subq    $16, %r15
        cmpq    caml_young_limit(%rip), %r15
        jb      .L100
        ret
.L100:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc1

FUNCTION(caml_alloc2)
        subq    $24, %r15
        cmpq    caml_young_limit(%rip), %r15
        jb      .L101
        ret
.L101:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc2

FUNCTION(caml_alloc3)
        subq    $32, %r15
        cmpq    caml_young_limit(%rip), %r15
        jb      .L102
        ret
.L102:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc3

FUNCTION(caml_allocN)
        subq    %rax, %r15
        cmpq    caml_young_limit(%rip), %r15
        jb      .L103
        ret
.L103:
        pushq   %rax                       /* save desired size */
        movq    8(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    16(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
        call    .L105
        popq    %rax                      /* recover desired size */
        jmp     caml_allocN

/* Call a C function from Caml */

FUNCTION(caml_c_call)
    /* Record lowest stack address and return address */
        popq    %r12
        movq    %r12, caml_last_return_address(%rip)
        movq    %rsp, caml_bottom_of_stack(%rip)
    /* Make the exception handler and alloc ptr available to the C code */
	movq	%r15, caml_young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Call the function (address in %rax) */
        call    *%rax
    /* Reload alloc ptr */
	movq	caml_young_ptr(%rip), %r15
    /* Return to caller */
	pushq	%r12
	ret

/* Start the Caml program */

FUNCTION(caml_start_program)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial entry point is caml_program */
        leaq    caml_program(%rip), %r12
    /* Common code for caml_start_program and caml_callback* */
.L106:
    /* Build a callback link */
	subq	$8, %rsp	/* stack 16-aligned */
        pushq   caml_gc_regs(%rip)
        pushq   caml_last_return_address(%rip)
        pushq   caml_bottom_of_stack(%rip)
    /* Setup alloc ptr and exception ptr */
	movq	caml_young_ptr(%rip), %r15
	movq	caml_exception_pointer(%rip), %r14
    /* Build an exception handler */
        lea     .L108(%rip), %r13
        pushq   %r13
        pushq   %r14
        movq    %rsp, %r14
    /* Call the Caml code */
        call    *%r12
.L107:
    /* Pop the exception handler */
        popq    %r14
        popq    %r12    /* dummy register */
.L109:
    /* Update alloc ptr and exception ptr */
	movq	%r15, caml_young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Pop the callback link, restoring the global variables */
        popq    caml_bottom_of_stack(%rip)
        popq    caml_last_return_address(%rip)
        popq    caml_gc_regs(%rip)
	addq	$8, %rsp
    /* Restore callee-save registers. */
	addq	$8, %rsp
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
    /* Return to caller. */
        ret
.L108:
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        jmp     .L109

/* Raise an exception from Caml */

FUNCTION(caml_raise_exn)
        testl   $1, caml_backtrace_active(%rip)
        jne     .L110
        movq    %r14, %rsp
        popq    %r14
        ret
.L110:
        movq    %rax, %r12            /* Save exception bucket */
        movq    %rax, %rdi            /* arg 1: exception bucket */
        movq    0(%rsp), %rsi         /* arg 2: pc of raise */
        leaq    8(%rsp), %rdx         /* arg 3: sp of raise */
        movq    %r14, %rcx            /* arg 4: sp of handler */
        call    caml_stash_backtrace
        movq    %r12, %rax            /* Recover exception bucket */
        movq    %r14, %rsp
        popq    %r14
        ret

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        testl   $1, caml_backtrace_active(%rip)
        jne     .L111
        movq    %rdi, %rax
        movq    caml_exception_pointer(%rip), %rsp
        popq    %r14                  /* Recover previous exception handler */
        movq    caml_young_ptr(%rip), %r15 /* Reload alloc ptr */
        ret
.L111:
        movq    %rdi, %r12            /* Save exception bucket */
                                      /* arg 1: exception bucket */
        movq    caml_last_return_address(%rip), %rsi /* arg 2: pc of raise */
        movq    caml_bottom_of_stack(%rip), %rdx /* arg 3: sp of raise */
        movq    caml_exception_pointer(%rip), %rcx /* arg 4: sp of handler */
        call    caml_stash_backtrace
        movq    %r12, %rax            /* Recover exception bucket */
        movq    caml_exception_pointer(%rip), %rsp
        popq    %r14                  /* Recover previous exception handler */
        movq    caml_young_ptr(%rip), %r15 /* Reload alloc ptr */
        ret

/* Callback from C to Caml */

FUNCTION(caml_callback_exn)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        movq    %rdi, %rbx      /* closure */
        movq    %rsi, %rax      /* argument */
        movq    0(%rbx), %r12   /* code pointer */
        jmp     .L106

FUNCTION(caml_callback2_exn)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        /* closure stays in %rdi */
        movq    %rsi, %rax               /* first argument */
        movq    %rdx, %rbx               /* second argument */
        leaq    caml_apply2(%rip), %r12  /* code pointer */
        jmp     .L106

FUNCTION(caml_callback3_exn)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        movq    %rsi, %rax               /* first argument */
        movq    %rdx, %rbx               /* second argument */
        movq    %rdi, %rsi               /* closure */
        movq    %rcx, %rdi               /* third argument */
        leaq    caml_apply3(%rip), %r12  /* code pointer */
        jmp     .L106

FUNCTION(caml_ml_array_bound_error)
        leaq    caml_array_bound_error(%rip), %rax
        jmp     caml_c_call

        .data
        .globl  caml_system__frametable
        .type   caml_system__frametable,@object
        .align  8
caml_system__frametable:
        .quad   1           /* one descriptor */
        .quad   .L107       /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  8

	.section	.rodata.cst8,"a",@progbits
        .globl  caml_negf_mask
        .type   caml_negf_mask,@object
        .align  16
caml_negf_mask:
	.quad	0x8000000000000000, 0
        .globl  caml_absf_mask
        .type   caml_absf_mask,@object
        .align  16
caml_absf_mask:
	.quad	0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
