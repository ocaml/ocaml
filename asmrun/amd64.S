/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2003 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#include "caml/m.h"

#if defined(SYS_macosx)

#define LBL(x) L##x
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

#elif defined(SYS_mingw64) || defined(SYS_cygwin)

#define LBL(x) .L##x
#define G(r) r
#undef  GREL
#define GCALL(r) r
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
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

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_OFFSET(r, n) .cfi_offset r, n
#define CFI_SAME_VALUE(r) .cfi_same_value r
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_OFFSET(r, n)
#define CFI_SAME_VALUE(r)
#endif

#ifdef WITH_FRAME_POINTERS

#define ENTER_FUNCTION \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp
#define LEAVE_FUNCTION \
        popq    %rbp; CFI_ADJUST(-8);

#else

#define ENTER_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8);
#define LEAVE_FUNCTION \
        addq    $8, %rsp; CFI_ADJUST (-8);

#endif

#ifdef DEBUG
#define CHECK_STACK_ALIGNMENT \
        test $0xf, %rsp; jz 9f; int3; 9:
#else
#define CHECK_STACK_ALIGNMENT
#endif

/******************************************************************************/
/* Access to the current domain state block. */
/******************************************************************************/

#define CAML_CONFIG_H_NO_TYPEDEFS
#include "../byterun/caml/config.h"

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../byterun/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) (8*domain_field_caml_##var)(%r14)

/* Load address of global [label] in register [dst]. */
#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst
#else
#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst
#endif

/* Push the current exception handler.
   (Stored as an offset, to survive stack reallocations). Clobbers REG */
#define PUSH_EXN_HANDLER(REG) \
        movq	Caml_state(exn_handler), REG; \
        pushq   REG; CFI_ADJUST(8); \
        subq    %rsp, 0(%rsp)

/* Pop the current exception handler. Undoes PUSH_EXN_HANDLER. Clobbers REG. */
#define POP_EXN_HANDLER(REG) \
        leaq	Caml_state(exn_handler), REG; \
        addq    %rsp, 0(%rsp); \
        popq    (REG); CFI_ADJUST(-8)

/******************************************************************************/
/* Stack switching operations */
/******************************************************************************/

/* Switch from OCaml to C stack. Clobbers REG.
 * PUSHED is the number of bytes already pushed by this function.
 * It is used to compute the backtrace. */
#define SWITCH_OCAML_TO_C_NO_CTXT(REG, PUSHED) \
    /* Save OCaml SP in the stack slot */ \
        movq	Caml_state(current_stack), REG; \
        movq    %rsp, Stack_sp(REG); \
    /* Switch to system stack */ \
        movq	Caml_state(c_stack), REG; \
        movq    %rsp, Cstack_sp(REG); \
        movq    REG, %rsp; \
        .cfi_remember_state; \
        .cfi_escape DW_CFA_def_cfa_expression, 5, \
          DW_OP_breg + DW_REG_rsp, Cstack_sp, DW_OP_deref, \
          DW_OP_plus_uconst, (PUSHED) + 8 /* retaddr */

/* Switch from OCaml to C stack. Also builds a context at
 * the bottom of the OCaml stack. Clobbers REG.
 * PUSHED is the number of bytes already pushed by this function.
 * It is used to compute the backtrace. */
#define SWITCH_OCAML_TO_C(REG, PUSHED) \
    /* Build caml_context at the bottom of the stack */ \
        pushq   $0 ; CFI_ADJUST(8); \
        PUSH_EXN_HANDLER(REG); \
        SWITCH_OCAML_TO_C_NO_CTXT(REG, (PUSHED) + 16)

/* Switch from C to OCaml stack.  Clobbers REG. */
#define SWITCH_C_TO_OCAML_NO_CTXT(REG) \
    /* Switch to OCaml stack */ \
        movq	Caml_state(current_stack), REG; \
    /* REG is Stack_sp(caml_current_stack) */ \
        movq    Stack_sp(REG), %rsp; \
        .cfi_restore_state

/* Switch from C to OCaml stack. Also pops the context
 * from the bottom of the OCaml stack. Clobbers REG. */
#define SWITCH_C_TO_OCAML(REG) \
        SWITCH_C_TO_OCAML_NO_CTXT(REG); \
    /* Pop the caml_context from the bottom of stack */ \
        POP_EXN_HANDLER(REG); \
        popq    REG; CFI_ADJUST(-8); \

/* Load Caml_state->exn_handler into %rsp and restores prior exn_handler. Clobbers %r11. */
#define RESTORE_EXN_HANDLER_OCAML \
        movq	Caml_state(exn_handler), %rsp; \
        .cfi_def_cfa_offset 16; \
        POP_EXN_HANDLER(%r11)

/* Switch between OCaml stacks.
 * arguments : target stack (%rdi)
 * preserves : %rbx, %r8, %r9, %r12, %r13 */
#define SWITCH_OCAML_STACKS \
    /* Build caml_context at the bottom of the stack */ \
        pushq   $0 ; CFI_ADJUST(8); \
        PUSH_EXN_HANDLER(%r10); \
    /* Save OCaml SP in the stack slot */ \
        movq	Caml_state(current_stack), %r10; \
        movq    %rsp, Stack_sp(%r10); \
        movq    %rdi, Caml_state(current_stack) ; \
        movq    Stack_sp(%rdi), %rsp; \
        .cfi_def_cfa_offset 24 ; /* FIXME */ \
    /* Pop the caml_context from the bottom of stack */ \
        POP_EXN_HANDLER(%r10); \
        popq    %r10; CFI_ADJUST(-8)

/******************************************************************************/
/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */
/******************************************************************************/

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST (8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST (8); CFI_OFFSET(rbp, -24); \
                      /* Allows debugger to walk the stack */ \
        pushq   %rsi; CFI_ADJUST (8); CFI_OFFSET(rsi, -32); \
        pushq   %rdi; CFI_ADJUST (8); CFI_OFFSET(rdi, -40); \
        pushq   %r12; CFI_ADJUST (8); CFI_OFFSET(r12, -48); \
        pushq   %r13; CFI_ADJUST (8); CFI_OFFSET(r13, -56); \
        pushq   %r14; CFI_ADJUST (8); CFI_OFFSET(r14, -64); \
        pushq   %r15; CFI_ADJUST (8); CFI_OFFSET(r15, -72); \
        subq    $(8+10*16), %rsp; CFI_ADJUST (8+10*16); \
        movupd  %xmm6, 0*16(%rsp); \
        movupd  %xmm7, 1*16(%rsp); \
        movupd  %xmm8, 2*16(%rsp); \
        movupd  %xmm9, 3*16(%rsp); \
        movupd  %xmm10, 4*16(%rsp); \
        movupd  %xmm11, 5*16(%rsp); \
        movupd  %xmm12, 6*16(%rsp); \
        movupd  %xmm13, 7*16(%rsp); \
        movupd  %xmm14, 8*16(%rsp); \
        movupd  %xmm15, 9*16(%rsp)

#define POP_CALLEE_SAVE_REGS \
        movupd  0*16(%rsp), %xmm6; \
        movupd  1*16(%rsp), %xmm7; \
        movupd  2*16(%rsp), %xmm8; \
        movupd  3*16(%rsp), %xmm9; \
        movupd  4*16(%rsp), %xmm10; \
        movupd  5*16(%rsp), %xmm11; \
        movupd  6*16(%rsp), %xmm12; \
        movupd  7*16(%rsp), %xmm13; \
        movupd  8*16(%rsp), %xmm14; \
        movupd  9*16(%rsp), %xmm15; \
        addq    $(8+10*16), %rsp; CFI_ADJUST (-8-10*16); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rdi; CFI_ADJUST(-8); CFI_SAME_VALUE(rdi); \
        popq    %rsi; CFI_ADJUST(-8); CFI_SAME_VALUE(rsi); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#else

/* Unix API: callee-save regs are rbx, rbp, r12-r15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST(8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST(8); CFI_OFFSET(rbp, -24); \
        pushq   %r12; CFI_ADJUST(8); CFI_OFFSET(r12, -32); \
        pushq   %r13; CFI_ADJUST(8); CFI_OFFSET(r13, -40); \
        pushq   %r14; CFI_ADJUST(8); CFI_OFFSET(r14, -48); \
        pushq   %r15; CFI_ADJUST(8); CFI_OFFSET(r15, -56)

#define POP_CALLEE_SAVE_REGS \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#endif

#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
#  define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
#  define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
   /* Stack probing mustn't be larger than the page size */
#  define STACK_PROBE_SIZE $4096
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#  define STACK_PROBE_SIZE $32768
#endif

#define C_call(target) \
  PREPARE_FOR_C_CALL; CHECK_STACK_ALIGNMENT; call target; CLEANUP_AFTER_C_CALL

/******************************************************************************/
/* Registers holding arguments of C functions. */
/******************************************************************************/

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#define C_ARG_1 %rcx
#define C_ARG_2 %rdx
#define C_ARG_3 %r8
#define C_ARG_4 %r9
#else
#define C_ARG_1 %rdi
#define C_ARG_2 %rsi
#define C_ARG_3 %rdx
#define C_ARG_4 %rcx
#endif

/* struct stack_info */
#define Stack_sp                 0
#define Stack_handler            8

/* struct stack_handler */
#define Handler_value(REG)       0(REG)
#define Handler_exception(REG)   8(REG)
#define Handler_effect(REG)     16(REG)
#define Handler_parent          24

/* struct c_stack_link */
#define Cstack_stack             0
#define Cstack_sp                8
#define Cstack_prev             16

/******************************************************************************/
/* text section
/******************************************************************************/

        .text

        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */


/******************************************************************************/
/* DWARF
/******************************************************************************/

#define DW_CFA_val_expression     0x16
#define DW_CFA_expression         0x10
#define DW_CFA_def_cfa_expression 0x0f
#define DW_CFA_restore            0x03
#define DW_CFA_val_offset         0x14
#define DW_REG_rbp                6
#define DW_REG_rsp                7
#define DW_REG_retaddr            16
#define DW_OP_breg                0x70
#define DW_OP_breg3               0x73  /* rbx */
#define DW_OP_breg7               0x77  /* rsp */
#define DW_OP_breg13              0x7d  /* r13 */
#define DW_OP_nop                 0x96
#define DW_OP_deref               0x06
#define DW_OP_const1u             0x08
#define DW_OP_plus                0x22
#define DW_OP_plus_uconst         0x23
#define DW_OP_minus               0x1c
#define DW_OP_shl                 0x24
#define DW_OP_shr                 0x25
#define DW_OP_dup                 0x12
#define DW_OP_piece               0x93
#define DW_OP_swap                0x16
#define DW_OP_const8u             0x0e
#define DW_OP_and                 0x1a

/******************************************************************************/
/* Allocation */
/******************************************************************************/

/* Save all of the registers that may be in use to a free gc_regs bucket.
   Returns: bucket in %r15. Clobbers %r11 (after saving it) */
#define SAVE_ALL_REGS                                  \
    /* First, save the young_ptr. */                   \
        movq    %r15, Caml_state(young_ptr);           \
    /* Now, use %r15 to point to the gc_regs bucket */ \
    /* We save %r11 first to allow it to be scratch */ \
        movq    Caml_state(gc_regs_buckets), %r15;     \
        movq    %r11, 11*8(%r15);                      \
        movq    0(%r15), %r11; /* next ptr */          \
        movq    %r11, Caml_state(gc_regs_buckets);     \
        movq    %rax,         0*8(%r15);               \
        movq    %rbx,         1*8(%r15);               \
        movq    %rdi,         2*8(%r15);               \
        movq    %rsi,         3*8(%r15);               \
        movq    %rdx,         4*8(%r15);               \
        movq    %rcx,         5*8(%r15);               \
        movq    %r8,          6*8(%r15);               \
        movq    %r9,          7*8(%r15);               \
        movq    %r12,         8*8(%r15);               \
        movq    %r13,         9*8(%r15);               \
        movq    %r10,        10*8(%r15);               \
             /* %r11 is at   11*8(%r15); */            \
        movq    %rbp,        12*8(%r15);               \
        movsd   %xmm0,   (0+13)*8(%r15);               \
        movsd   %xmm1,   (1+13)*8(%r15);               \
        movsd   %xmm2,   (2+13)*8(%r15);               \
        movsd   %xmm3,   (3+13)*8(%r15);               \
        movsd   %xmm4,   (4+13)*8(%r15);               \
        movsd   %xmm5,   (5+13)*8(%r15);               \
        movsd   %xmm6,   (6+13)*8(%r15);               \
        movsd   %xmm7,   (7+13)*8(%r15);               \
        movsd   %xmm8,   (8+13)*8(%r15);               \
        movsd   %xmm9,   (9+13)*8(%r15);               \
        movsd   %xmm10, (10+13)*8(%r15);               \
        movsd   %xmm11, (11+13)*8(%r15);               \
        movsd   %xmm12, (12+13)*8(%r15);               \
        movsd   %xmm13, (13+13)*8(%r15);               \
        movsd   %xmm14, (14+13)*8(%r15);               \
        movsd   %xmm15, (15+13)*8(%r15)

/* Undo SAVE_ALL_REGS. Expects gc_regs bucket in %r15 */
#define RESTORE_ALL_REGS                               \
    /* Restore %rax, freeing up the next ptr slot */   \
        movq    0*8(%r15), %rax;                       \
        movq    Caml_state(gc_regs_buckets), %r11;     \
        movq    %r11, 0(%r15); /* next ptr */          \
        movq    %r15, Caml_state(gc_regs_buckets);     \
           /* above:  0*8(%r15),%rax; */               \
        movq          1*8(%r15),%rbx;                  \
        movq          2*8(%r15),%rdi;                  \
        movq          3*8(%r15),%rsi;                  \
        movq          4*8(%r15),%rdx;                  \
        movq          5*8(%r15),%rcx;                  \
        movq          6*8(%r15),%r8;                   \
        movq          7*8(%r15),%r9;                   \
        movq          8*8(%r15),%r12;                  \
        movq          9*8(%r15),%r13;                  \
        movq         10*8(%r15),%r10;                  \
        movq         11*8(%r15),%r11;                  \
        movq         12*8(%r15),%rbp;                  \
        movsd    (0+13)*8(%r15),%xmm0;                 \
        movsd    (1+13)*8(%r15),%xmm1;                 \
        movsd    (2+13)*8(%r15),%xmm2;                 \
        movsd    (3+13)*8(%r15),%xmm3;                 \
        movsd    (4+13)*8(%r15),%xmm4;                 \
        movsd    (5+13)*8(%r15),%xmm5;                 \
        movsd    (6+13)*8(%r15),%xmm6;                 \
        movsd    (7+13)*8(%r15),%xmm7;                 \
        movsd    (8+13)*8(%r15),%xmm8;                 \
        movsd    (9+13)*8(%r15),%xmm9;                 \
        movsd   (10+13)*8(%r15),%xmm10;                \
        movsd   (11+13)*8(%r15),%xmm11;                \
        movsd   (12+13)*8(%r15),%xmm12;                \
        movsd   (13+13)*8(%r15),%xmm13;                \
        movsd   (14+13)*8(%r15),%xmm14;                \
        movsd   (15+13)*8(%r15),%xmm15;                \
        movq    Caml_state(young_ptr), %r15

#define ENTER_GC                                 \
        SAVE_ALL_REGS;                           \
        pushq   %r15;                            \
        PUSH_EXN_HANDLER(%r11);                  \
        SWITCH_OCAML_TO_C_NO_CTXT(%r11, 16);     \
        C_call (GCALL(caml_garbage_collection)); \
        SWITCH_C_TO_OCAML_NO_CTXT(%r11);         \
        POP_EXN_HANDLER(%r11);                   \
        popq    %r15;                            \
        RESTORE_ALL_REGS

FUNCTION(G(caml_call_gc))
CFI_STARTPROC
        .cfi_signal_frame
        ENTER_GC
        ret
CFI_ENDPROC

FUNCTION(G(caml_call_realloc_stack))
CFI_STARTPROC
        .cfi_signal_frame
        SAVE_ALL_REGS
        movq    8(%rsp), C_ARG_1 /* argument */
        SWITCH_OCAML_TO_C_NO_CTXT(%r11, 0)
        C_call  (GCALL(caml_try_realloc_stack))
        SWITCH_C_TO_OCAML_NO_CTXT(%r11)
        cmpq    $0, %rax
        jz      1f
        RESTORE_ALL_REGS
        ret
1:      RESTORE_ALL_REGS
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        add     $16, %rsp /* pop argument, retaddr */
        jmp     GCALL(caml_raise_exn)
CFI_ENDPROC

/* Arguments: %rax = base; %rdx = offset. */
FUNCTION(G(caml_call_read_barrier))
CFI_STARTPROC
        .cfi_signal_frame
        SAVE_ALL_REGS
     /* Build a caml_context */
        pushq   %r15
        PUSH_EXN_HANDLER(%r11)
        SWITCH_OCAML_TO_C_NO_CTXT(%r11, 16)
        movq    %rax, C_ARG_1
        movq    %rdx, C_ARG_2
        C_call (GCALL(caml_read_barrier))
        SWITCH_C_TO_OCAML_NO_CTXT(%r11)
        POP_EXN_HANDLER(%r11)
        popq    %r15
    /* Preserve the current value of %rax */
        pushq   %rax
        RESTORE_ALL_REGS
        popq    %rax
        ret
CFI_ENDPROC

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
LBL(caml_alloc1):
        subq    $16, %r15
        cmpq	Caml_state(young_limit), %r15
        jb      LBL(100)
        ret
LBL(100):
        movq    $16, %rax
        jmp     LBL(call_gc_and_retry_alloc)
CFI_ENDPROC

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
LBL(caml_alloc2):
        subq    $24, %r15
        cmpq	Caml_state(young_limit), %r15
        jb      LBL(101)
        ret
LBL(101):
        movq    $24, %rax
        jmp     LBL(call_gc_and_retry_alloc)
CFI_ENDPROC

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
LBL(caml_alloc3):
        subq    $32, %r15
        cmpq	Caml_state(young_limit), %r15
        jb      LBL(102)
        ret
LBL(102):
        movq    $32, %rax
        jmp     LBL(call_gc_and_retry_alloc)
CFI_ENDPROC

FUNCTION(G(caml_allocN))
CFI_STARTPROC
LBL(caml_allocN):
        subq    %rax, %r15
        cmpq	Caml_state(young_limit), %r15
        jb      LBL(call_gc_and_retry_alloc)
        ret
CFI_ENDPROC
CFI_STARTPROC
        .cfi_signal_frame
LBL(call_gc_and_retry_alloc):
        addq    %rax, %r15
        ENTER_GC
        jmp     LBL(caml_allocN)
CFI_ENDPROC

/******************************************************************************/
/* Call a C function from OCaml */
/******************************************************************************/

FUNCTION(G(caml_c_call))
CFI_STARTPROC
        .cfi_signal_frame
LBL(caml_c_call):
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax */
    /* Switch from OCaml to C */
        movq    Caml_state(current_stack), %r11
        SWITCH_OCAML_TO_C(%r10, 0)
        movq    %r11, Cstack_stack(%rsp)
    /* Make the alloc ptr available to the C code */
        movq	%r15, Caml_state(young_ptr)
    /* Call the function (address in %rax) */
        C_call  (*%rax)
    /* Prepare for return to OCaml */
        movq	Caml_state(young_ptr), %r15
        /* assert */
        movq    Cstack_sp(%rsp), %r11
        addq    $16, %r11 /* skip context */
    /* Load ocaml stack and restore global variables */
        SWITCH_C_TO_OCAML(%r10)
        cmpq    %r11, %rsp; je 1f; int3; 1:
    /* Return to OCaml caller */
        ret
CFI_ENDPROC

FUNCTION(G(caml_c_call_stack_args))
CFI_STARTPROC
        .cfi_signal_frame
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax
        C stack args        : begin=%r13 end=%r12 */
    /* Switch from OCaml to C */
        movq    Caml_state(current_stack), %r11
        SWITCH_OCAML_TO_C(%r10, 0)
        movq    %r11, Cstack_stack(%rsp)
    /* we use %rbp (otherwise unused) to enable backtraces */
        movq    %rsp, %rbp
        .cfi_escape DW_CFA_def_cfa_expression, 5, \
          DW_OP_breg + DW_REG_rbp, Cstack_sp, DW_OP_deref, \
          DW_OP_plus_uconst, 16 /* caml_context */ + 8 /* ret addr */
    /* Make the alloc ptr available to the C code */
        movq	%r15, Caml_state(young_ptr)
    /* Copy arguments from OCaml to C stack */
LBL(105):
        subq    $8, %r12
        cmpq    %r13,%r12
        jl      LBL(106)
        push    (%r12); CFI_ADJUST(8)
        jmp     LBL(105)
LBL(106):
    /* Call the function (address in %rax) */
        C_call  (*%rax)
    /* Prepare for return to OCaml */
        movq	Caml_state(young_ptr), %r15
    /* Load ocaml stack and restore global variables */
        SWITCH_C_TO_OCAML(%r10)
    /* Return to OCaml caller */
        ret
CFI_ENDPROC

/******************************************************************************/
/* Start the OCaml program */
/******************************************************************************/

FUNCTION(G(caml_start_program))
CFI_STARTPROC
        .cfi_signal_frame
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load Caml_state into r14 (was passed as an argument from C) */
        movq    C_ARG_1, %r14
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
        movq    $0, %rax  /* dummy */
        movq    $0, %rbx  /* dummy */
        movq    $0, %rdi  /* dummy */
        movq    $0, %rsi  /* dummy */
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Load young_ptr into %r15 */
        movq    Caml_state(young_ptr), %r15
    /* Build struct c_stack_link on the C stack */
        subq    $24 /* sizeof struct c_stack_link */, %rsp; CFI_ADJUST(24)
        movq    $0, Cstack_stack(%rsp)
        movq    $0, Cstack_sp(%rsp)
        movq    Caml_state(c_stack), %r10
        movq    %r10, Cstack_prev(%rsp)
        movq    %rsp, Caml_state(c_stack)
        CHECK_STACK_ALIGNMENT
    /* Load the OCaml stack. */
        movq    Caml_state(current_stack), %r11
        movq    Stack_sp(%r11), %r10
    /* Build a handler for exceptions raised in OCaml on the OCaml stack.
       There is no previous exception handler. Instead, we use the prev
       slot to store the C stack pointer, to allow DWARF backtraces */
        subq    $16, %r10
        lea     LBL(109)(%rip), %r11
        movq    %r11, 8(%r10)
        movq    %rsp, 0(%r10)
        movq    %r10, Caml_state(exn_handler)
    /* Switch stacks and call the OCaml code */
        movq    %r10, %rsp
        .cfi_remember_state
        .cfi_escape DW_CFA_def_cfa_expression, 3 + 2, \
          DW_OP_breg + DW_REG_rsp, 0, DW_OP_deref,\
          DW_OP_plus_uconst, \
             24 /* struct c_stack_link */ + 6*8 /* callee save regs */ + 8 /* ret addr */
        call    *%r12
LBL(108):
        leaq    16(%rsp), %r10 /* pop exn handler */
1:  /* Update alloc ptr */
        movq	%r15, Caml_state(young_ptr)
    /* Return to C stack. */
        movq    Caml_state(current_stack), %r11
        movq    %r10, Stack_sp(%r11)
        movq    Caml_state(c_stack), %rsp
        .cfi_restore_state
    /* Pop the struct c_stack_link */
        movq    Cstack_prev(%rsp), %r10
        movq    %r10, Caml_state(c_stack)
        addq    $24, %rsp; CFI_ADJUST(-24)
    /* Restore callee-save registers. */
        POP_CALLEE_SAVE_REGS
    /* Return to caller. */
        ret
LBL(109):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        movq    %rsp, %r10 /* exn handler already popped here */
        jmp     1b
CFI_ENDPROC

/******************************************************************************/
/* Exceptions */
/******************************************************************************/

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
LBL(caml_raise_exn):
        testq	$1, Caml_state(backtrace_active)
        jne   LBL(116)
        RESTORE_EXN_HANDLER_OCAML
        ret
LBL(116):
        movq	$0, Caml_state(backtrace_pos)
LBL(117):
        movq    %rsp, %r10        /* Save OCaml stack pointer */
        movq    %rax, %r12        /* Save exception bucket */
        movq	Caml_state(c_stack), %rsp
        movq    %rax, C_ARG_1     /* arg 1: exception bucket */
        movq    (%r10), C_ARG_2   /* arg 2: pc of raise */
        leaq    8(%r10), C_ARG_3  /* arg 3: sp at raise */
        movq	Caml_state(exn_handler), C_ARG_4
                                  /* arg 4: sp of handler */
        C_call  (GCALL(caml_stash_backtrace))
        movq    %r12, %rax        /* Recover exception bucket */
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC

FUNCTION(G(caml_reraise_exn))
CFI_STARTPROC
        testq	$1, Caml_state(backtrace_active)
        jne   LBL(117)
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        movq    C_ARG_1, %r14                /* Caml_state */
        movq    C_ARG_2, %rax
    /* Load young_ptr into %r15 */
        movq    Caml_state(young_ptr), %r15
    /* Load ocaml stack and restore global variables */
        .cfi_remember_state
        SWITCH_C_TO_OCAML(%r10)
    /* Raise the exception in OCaml */
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

/******************************************************************************/
/* Callback from C to OCaml */
/******************************************************************************/

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rbx      /* closure */
        movq    0(C_ARG_3), %rax   /* argument */
        movq    0(%rbx), %r12      /* code pointer */
        movq    $0, %rdi           /* dummy */
        movq    $0, %rsi           /* dummy */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rdi      /* closure */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        LEA_VAR(caml_apply2, %r12) /* code pointer */
        movq    $0, %rsi           /* dummy */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        movq    C_ARG_2, %rsi      /* closure */
        movq    16(C_ARG_3), %rdi  /* third argument */
        LEA_VAR(caml_apply3, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

/******************************************************************************/
/* Fibers */
/******************************************************************************/

FUNCTION(G(caml_perform))
CFI_STARTPROC
    /*  %rax: effect to perform
        %rbx: freshly allocated continuation */
        mov     %rax, %r12                      /* r8,r9,r12,r13 are preserved across C call */
        mov     %rbx, %r13
        movq	Caml_state(current_stack), %rbx /* Second argument */
        leaq    1(%rbx), %r8                    /* %r8 := Val_ptr(%rbx) */
        movq    Stack_handler(%rbx), %rbx
        movq    Handler_parent(%rbx), %rdi      /* Parent stack. */
        cmpq    $0, %rdi                        /* Parent is NULL? */
        je      LBL(112)
        movq    $0, Handler_parent(%rbx)        /* Set parent stack of performer to NULL */
        movq    %r8, 0(%r13)                    /* Initialise continuation */
        movq    Handler_effect(%rbx), %r9       /* effect handler */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        movq    %r12, %rax        /* Arg 1: effect */
        movq    %r13, %rbx        /* Arg 2: continuation */
        movq    %r8, %rdi         /* Arg 3: cont_tail */
        movq    %r9, %rsi         /* Effect handler */
        LEA_VAR(caml_apply3, %rcx)
        jmp     *%rcx
LBL(112):
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_exn_Unhandled, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

FUNCTION(G(caml_reperform))
CFI_STARTPROC
    /*  %rax: effect to reperform
        %rbx: continuation
        %rdi: continuation tail */
        movq    %rax, %r12                      /* r8,r9,r12,r13 are preserved across C call */
        movq    %rbx, %r13
        movq	Caml_state(current_stack), %rbx
        movq    (Stack_handler-1)(%rdi), %rdi
        movq    %rbx, Handler_parent(%rdi)       /* Append to cont_tail in Stack_parent(Ptr_val(r8)) */
        leaq    1(%rbx), %r8                     /* %r8 := Val_ptr(%rbx) */
        movq    Stack_handler(%rbx), %rbx
        movq    Handler_parent(%rbx), %rdi       /* Parent stack */
        cmpq    $0, %rdi                         /* Parent is Null? */
        je      LBL(113)
        movq    $0, Handler_parent(%rbx)         /* Set parent stack of performer to NULL */
        movq    Handler_effect(%rbx), %r9        /* effect handler */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        movq    %r12, %rax        /* Arg 1: effect */
        movq    %r13, %rbx        /* Arg 2: continuation */
        movq    %r8, %rdi         /* Arg 3: cont_tail */
        movq    %r9, %rsi         /* Effect handler */
        LEA_VAR(caml_apply3, %rcx)
        jmp     *%rcx
LBL(113):
    /* switch back to original performer before raising Unhandled */
        movq    0(%r13), %rdi                     /* load performer stack from continuation */
        subq    $1, %rdi                          /* rdi := Ptr_val(rdi) */
        SWITCH_OCAML_STACKS
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_exn_Unhandled, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

FUNCTION(G(caml_resume))
CFI_STARTPROC
    /* %rax -> stack, %rbx -> fun, %rdi -> arg */
        andq    $-2, %rax       /* %rax = Ptr_val(%rax) */
        movq    %rdi, %r12      /* Save arg (callee-saved) */
        movq    %rax, %rdi
    /* Find end of list of stacks and add current */
1:      movq    Stack_handler(%rax), %rcx
        cmpq    $0, Handler_parent(%rcx)
        je      2f
        movq    Handler_parent(%rcx), %rax
        jmp     1b
2:      movq	Caml_state(current_stack), %rax
        movq    %rax, Handler_parent(%rcx)
    /* %rdi has new stack */
        SWITCH_OCAML_STACKS
        movq    %r12, %rax    /* first argument */
        movq    (%rbx), %rsi  /* closure in %rbx (second argument) */
        jmp     *%rsi
CFI_ENDPROC

/* Run a function on a new stack,
   then invoke either the value or exception handler */
FUNCTION(G(caml_runstack))
CFI_STARTPROC
        .cfi_signal_frame
    /* %rax -> stack, %rbx -> fun, %rdi -> arg */
        andq    $-2, %rax       /* %rax = Ptr_val(%rax) */
        movq    %rdi, %r12      /* Save arg (callee-saved) */
        movq    %rax, %rdi
    /* Build caml_context at the bottom of the stack */
        pushq   $0 ; CFI_ADJUST(8);
        PUSH_EXN_HANDLER(%r10)
    /* Load new stack pointer into %r11 and set parent */
        movq	Caml_state(current_stack), %rax
        movq    %rsp, Stack_sp(%rax)
        movq    Stack_handler(%rdi), %rcx
        movq    %rax, Handler_parent(%rcx)
        movq    %rdi, Caml_state(current_stack)
        movq    Stack_sp(%rdi), %r11
    /* Create an exception handler on the target stack */
        subq    $16, %r11
        leaq    LBL(fiber_exn_handler)(%rip), %r10
        movq    %r10, 8(%r11)
        movq    $0, 0(%r11) /* dummy previous trap frame */
        movq    %r11, Caml_state(exn_handler)
    /* Switch to the new stack */
        movq    %r11, %rsp
        .cfi_remember_state
        .cfi_escape DW_CFA_def_cfa_expression, 3+3+2, \
          DW_OP_breg + DW_REG_rsp, 16 /* exn */ + Handler_parent, DW_OP_deref,\
          DW_OP_plus_uconst, Stack_sp, DW_OP_deref,\
          DW_OP_plus_uconst, 16 /* sizeof caml_context */ + 8 /* ret addr */
        movq    %r12, %rax /* first argument */
        callq   *(%rbx) /* closure in %rbx (second argument) */
LBL(frame_runstack):
        leaq    16(%rsp), %r11 /* SP with exn handler popped */
        movq    Handler_value(%r11), %rbx
1:  /* Switch back to parent stack and free current */
        movq    Handler_parent(%r11), %r10
        movq    Caml_state(current_stack), %rdi
        movq    %r10, Caml_state(current_stack)
        movq    Stack_sp(%r10), %r13 /* saved across C call */
        movq    %rax, %r12 /* save %rax across C call */
        movq    Caml_state(c_stack), %rsp
        /* FIXME: CFI here */
        call    GCALL(caml_free_stack)
        movq    %r13, %rsp
        .cfi_restore_state
        movq    %r12, %rax
    /* Pop the caml_context from the bottom of the stack */
        POP_EXN_HANDLER(%r10)
        popq    %r10; CFI_ADJUST(-8)
    /* Invoke handle_value (or handle_exn) */
        jmp     *(%rbx)
LBL(fiber_exn_handler):
        movq    %rsp, %r11
        movq    Handler_exception(%rsp), %rbx
        jmp     1b
CFI_ENDPROC


FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC

FUNCTION(G(caml_assert_stack_invariants))
CFI_STARTPROC
/*        test    $0xf, %rsp */
/*        jz      1f */
/*        int3 */
1:      movq	Caml_state(current_stack), %r11
        movq    %rsp, %r10
        subq    %r11, %r10      /* %r10: number of bytes left on stack */
        /* can be two words over: the return addresses */
        cmp     $((Stack_threshold_words + Stack_ctx_words - 2)*8), %r10
        jge     1f
        int3
1:      ret
CFI_ENDPROC

        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   2           /* three descriptors */
        .quad   LBL(108)    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN
        .quad   LBL(frame_runstack)    /* return address into fiber_val_handler */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst8,"a",@progbits
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
        .quad   0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
        .quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
