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

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#ifdef SYS_macosx

#define G(r) _##r
#define GREL(r) _##r@GOTPCREL
#define GCALL(r) _##r
#define FUNCTION_ALIGN 2
#define EIGHT_ALIGN 3
#define SIXTEEN_ALIGN 4
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#ifdef __PIC__

/* Position-independent operations on global variables. */

/* Store [srcreg] in global [dstlabel].  Clobbers %r11. */
#define STORE_VAR(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movq    srcreg, (%r11)

/* Load global [srclabel] in register [dstreg].  Clobbers %r11. */
#define LOAD_VAR(srclabel,dstreg) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        movq    (%r11), dstreg

/* Compare global [label] with register [reg].  Clobbers %rax. */
#define CMP_VAR(label,reg) \
        movq    GREL(label)(%rip), %rax ; \
        cmpq    (%rax), reg

/* Test 32-bit global [label] against mask [imm].  Clobbers %r11. */
#define TESTL_VAR(imm,label) \
        movq    GREL(label)(%rip), %r11 ; \
        testl   imm, (%r11)

/* Push global [label] on stack.  Clobbers %r11. */
#define PUSH_VAR(srclabel) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        pushq   (%r11)

/* Pop global [label] off stack.  Clobbers %r11. */
#define POP_VAR(dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        popq    (%r11)

/* Record lowest stack address and return address.  Clobbers %rax. */
#define RECORD_STACK_FRAME(OFFSET) \
	pushq   %r11 ; \
        movq    8+OFFSET(%rsp), %rax ; \
	STORE_VAR(%rax,caml_last_return_address) ; \
        leaq    16+OFFSET(%rsp), %rax ; \
	STORE_VAR(%rax,caml_bottom_of_stack) ; \
	popq    %r11

#else
        
/* Non-PIC operations on global variables.  Slightly faster. */

#define STORE_VAR(srcreg,dstlabel) \
        movq    srcreg, G(dstlabel)(%rip)

#define LOAD_VAR(srclabel,dstreg) \
        movq    G(srclabel)(%rip), dstreg

#define CMP_VAR(label,reg) \
        cmpq    G(label)(%rip), %r15

#define TESTL_VAR(imm,label) \
        testl   imm, G(label)(%rip)

#define PUSH_VAR(srclabel) \
        pushq   G(srclabel)(%rip)

#define POP_VAR(dstlabel) \
        popq    G(dstlabel)(%rip)

#define RECORD_STACK_FRAME(OFFSET) \
        movq    OFFSET(%rsp), %rax ; \
	STORE_VAR(%rax,caml_last_return_address) ; \
        leaq    8+OFFSET(%rsp), %rax ; \
	STORE_VAR(%rax,caml_bottom_of_stack)

#endif

        .text

/* Allocation */

FUNCTION(G(caml_call_gc))
        RECORD_STACK_FRAME(0)
.Lcaml_call_gc:
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
        STORE_VAR(%rsp, caml_gc_regs)
    /* Save caml_young_ptr, caml_exception_pointer */
	STORE_VAR(%r15, caml_young_ptr)
	STORE_VAR(%r14, caml_exception_pointer)
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
        call    GCALL(caml_garbage_collection)
    /* Restore caml_young_ptr, caml_exception_pointer */
	LOAD_VAR(caml_young_ptr, %r15)
	LOAD_VAR(caml_exception_pointer, %r14)
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
    /* Return to caller */
        ret

FUNCTION(G(caml_alloc1))
.Lcaml_alloc1:
        subq    $16, %r15
        CMP_VAR(caml_young_limit, %r15)
        jb      .L100
        ret
.L100:
        RECORD_STACK_FRAME(0)
	subq	$8, %rsp
        call    .Lcaml_call_gc
	addq	$8, %rsp
        jmp     .Lcaml_alloc1

FUNCTION(G(caml_alloc2))
.Lcaml_alloc2:
        subq    $24, %r15
        CMP_VAR(caml_young_limit, %r15)
        jb      .L101
        ret
.L101:
        RECORD_STACK_FRAME(0)
	subq	$8, %rsp
        call    .Lcaml_call_gc
	addq	$8, %rsp
        jmp     .Lcaml_alloc2

FUNCTION(G(caml_alloc3))
.Lcaml_alloc3:
        subq    $32, %r15
        CMP_VAR(caml_young_limit, %r15)
        jb      .L102
        ret
.L102:
        RECORD_STACK_FRAME(0)
	subq	$8, %rsp
        call    .Lcaml_call_gc
	addq	$8, %rsp
        jmp     .Lcaml_alloc3

FUNCTION(G(caml_allocN))
.Lcaml_allocN:
        pushq   %rax                       /* save desired size */
        subq    %rax, %r15
        CMP_VAR(caml_young_limit, %r15)
        jb      .L103
        addq    $8, %rsp                  /* drop desired size */
        ret
.L103:
        RECORD_STACK_FRAME(8)
        call    .Lcaml_call_gc
        popq    %rax                      /* recover desired size */
        jmp     .Lcaml_allocN

/* Call a C function from Caml */

FUNCTION(G(caml_c_call))
.Lcaml_c_call:
    /* Record lowest stack address and return address */
        popq    %r12
        STORE_VAR(%r12, caml_last_return_address)
        STORE_VAR(%rsp, caml_bottom_of_stack)
    /* Make the exception handler and alloc ptr available to the C code */
	STORE_VAR(%r15, caml_young_ptr)
	STORE_VAR(%r14, caml_exception_pointer)
    /* Call the function (address in %rax) */
        call    *%rax
    /* Reload alloc ptr */
	LOAD_VAR(caml_young_ptr, %r15)
    /* Return to caller */
	pushq	%r12
	ret

/* Start the Caml program */

FUNCTION(G(caml_start_program))
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial entry point is G(caml_program) */
        leaq    GCALL(caml_program)(%rip), %r12
    /* Common code for caml_start_program and caml_callback* */
.Lcaml_start_program:
    /* Build a callback link */
	subq	$8, %rsp	/* stack 16-aligned */
        PUSH_VAR(caml_gc_regs)
        PUSH_VAR(caml_last_return_address)
        PUSH_VAR(caml_bottom_of_stack)
    /* Setup alloc ptr and exception ptr */
	LOAD_VAR(caml_young_ptr, %r15)
	LOAD_VAR(caml_exception_pointer, %r14)
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
	STORE_VAR(%r15,caml_young_ptr)
	STORE_VAR(%r14,caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
	POP_VAR(caml_bottom_of_stack)
        POP_VAR(caml_last_return_address)
        POP_VAR(caml_gc_regs)
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

FUNCTION(G(caml_raise_exn))
        TESTL_VAR($1, caml_backtrace_active)
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
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    %r14, %rsp
        popq    %r14
        ret

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
        TESTL_VAR($1, caml_backtrace_active)
        jne     .L111
        movq    %rdi, %rax
        LOAD_VAR(caml_exception_pointer, %rsp)  /* Cut stack */
        popq    %r14                  /* Recover previous exception handler */
        LOAD_VAR(caml_young_ptr, %r15) /* Reload alloc ptr */
        ret
.L111:
        movq    %rdi, %r12            /* Save exception bucket */
                                      /* arg 1: exception bucket */
	LOAD_VAR(caml_last_return_address,%rsi)   /* arg 2: pc of raise */
        LOAD_VAR(caml_bottom_of_stack,%rdx)       /* arg 3: sp of raise */
        LOAD_VAR(caml_exception_pointer,%rcx)     /* arg 4: sp of handler */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
	LOAD_VAR(caml_exception_pointer,%rsp)
        popq    %r14                  /* Recover previous exception handler */
	LOAD_VAR(caml_young_ptr,%r15)  /* Reload alloc ptr */
        ret

/* Callback from C to Caml */

FUNCTION(G(caml_callback_exn))
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
        jmp     .Lcaml_start_program

FUNCTION(G(caml_callback2_exn))
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
        leaq    GCALL(caml_apply2)(%rip), %r12  /* code pointer */
        jmp     .Lcaml_start_program

FUNCTION(G(caml_callback3_exn))
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
        leaq    GCALL(caml_apply3)(%rip), %r12  /* code pointer */
        jmp     .Lcaml_start_program

FUNCTION(G(caml_ml_array_bound_error))
        leaq    GCALL(caml_array_bound_error)(%rip), %rax
        jmp     .Lcaml_c_call

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   1           /* one descriptor */
        .quad   .L107       /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN

#ifdef SYS_macosx
	.literal16
#else
	.section    .rodata.cst8,"a",@progbits
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
	.quad	0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
	.quad	0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
