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
#define TEXT_SECTION(name) .text
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
#define TEXT_SECTION(name)
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#if defined(SYS_linux) || defined(SYS_gnu)
#define ENDFUNCTION(name) \
        .size name, . - name
#else
#define ENDFUNCTION(name)
#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_OFFSET(r, n) .cfi_offset r, n
#define CFI_DEF_CFA_OFFSET(n) .cfi_def_cfa_offset n
#define CFI_DEF_CFA_REGISTER(r) .cfi_def_cfa_register r
#define CFI_SAME_VALUE(r) .cfi_same_value r
#define CFI_SIGNAL_FRAME .cfi_signal_frame
#define CFI_REMEMBER_STATE .cfi_remember_state
#define CFI_RESTORE_STATE .cfi_restore_state
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_OFFSET(r, n)
#define CFI_DEF_CFA_OFFSET(n)
#define CFI_DEF_CFA_REGISTER(r)
#define CFI_SAME_VALUE(r)
#define CFI_SIGNAL_FRAME
#define CFI_REMEMBER_STATE
#define CFI_RESTORE_STATE
#endif

#ifdef WITH_FRAME_POINTERS
#define RETADDR_ENTRY_SIZE   16 /* rbp + retaddr */
#else
#define RETADDR_ENTRY_SIZE   8 /* retaddr */
#endif

#ifdef WITH_FRAME_POINTERS
#define ENTER_FUNCTION                      \
        pushq   %rbp; CFI_ADJUST(8);        \
        movq    %rsp, %rbp;
#define LEAVE_FUNCTION                      \
        popq    %rbp; CFI_ADJUST(-8);
#else
#define ENTER_FUNCTION
#define LEAVE_FUNCTION
#endif


#ifdef DEBUG
#define CHECK_STACK_ALIGNMENT \
        test $0xf, %rsp; jz 9f; int3; 9:
#define IF_DEBUG(...) __VA_ARGS__
#else
#define CHECK_STACK_ALIGNMENT
#define IF_DEBUG(...)
#endif

/* struct stack_info */
#define Stack_sp                 0
#define Stack_exception          8
#define Stack_handler            16

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
/* DWARF */
/******************************************************************************/

/* These constants are taken from:

     DWARF Debugging Information Format, Version 3
     http://dwarfstd.org/doc/Dwarf3.pdf

   with the amd64-specific register numbers coming from
   Fig. 3.36 ("DWARF Register Number Mapping") of:

     System V Application Binary Interface
     AMD64 Architecture Processor Supplement
     Version 1.0
     https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf */

#define DW_CFA_def_cfa_expression 0x0f
#define DW_REG_rbx                3
#define DW_REG_rsp                7
#define DW_REG_r13                13
#define DW_OP_breg                0x70
#define DW_OP_deref               0x06
#define DW_OP_plus_uconst         0x23

/******************************************************************************/
/* Access to the current domain state block. */
/******************************************************************************/

#define CAML_CONFIG_H_NO_TYPEDEFS
#include "../runtime/caml/config.h"

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
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

/* Push the current exception handler. Clobbers %r11 */
#define PUSH_EXN_HANDLER \
        movq    Caml_state(exn_handler), %r11; \
        pushq   %r11; CFI_ADJUST(8);

/* Pop the current exception handler. Undoes PUSH_EXN_HANDLER. Clobbers %r11 */
#define POP_EXN_HANDLER \
        leaq    Caml_state(exn_handler), %r11; \
        popq    (%r11); CFI_ADJUST(-8)

/******************************************************************************/
/* Stack switching operations */
/******************************************************************************/

/* Switch from OCaml to C stack. Clobbers %r10, %r11. */
#ifdef ASM_CFI_SUPPORTED
#define SWITCH_OCAML_TO_C_CFI                                   \
        CFI_REMEMBER_STATE;                                     \
          /* %rsp points to the c_stack_link. */                \
        .cfi_escape DW_CFA_def_cfa_expression, 5,               \
           DW_OP_breg + DW_REG_rsp, Cstack_sp, DW_OP_deref,     \
           DW_OP_plus_uconst, RETADDR_ENTRY_SIZE
#else
#define SWITCH_OCAML_TO_C_CFI
#endif
#define SWITCH_OCAML_TO_C                                  \
    /* Fill in Caml_state->current_stack->sp */            \
        movq    Caml_state(current_stack), %r10;           \
        movq    %rsp, Stack_sp(%r10);                      \
    /* Fill in Caml_state->c_stack */                      \
        movq    Caml_state(c_stack), %r11;                 \
        movq    %rsp, Cstack_sp(%r11);                     \
        movq    %r10, Cstack_stack(%r11);                  \
    /* Switch to C stack */                                \
        movq    %r11, %rsp;                                \
        SWITCH_OCAML_TO_C_CFI

/* Switch from C to OCaml stack.  Clobbers %r11. */
#define SWITCH_C_TO_OCAML                                           \
    /* Assert that %rsp == Caml_state->c_stack &&
        Caml_state->c_stack->sp == Caml_state->current_stack->sp */ \
        IF_DEBUG(cmpq %rsp, Caml_state(c_stack); je 8f; int3; 8:    \
                 movq Caml_state(current_stack), %r11;              \
                 movq Stack_sp(%r11), %r11;                         \
                 cmpq %r11, Cstack_sp(%rsp); je 8f; int3; 8:)       \
        movq    Cstack_sp(%rsp), %rsp;                              \
        CFI_RESTORE_STATE

/* Load Caml_state->exn_handler into %rsp and restores prior exn_handler.
   Clobbers %r10 and %r11. */
#define RESTORE_EXN_HANDLER_OCAML              \
        movq    Caml_state(exn_handler), %rsp; \
        CFI_DEF_CFA_OFFSET(16);                \
        POP_EXN_HANDLER

/* Switch between OCaml stacks. Clobbers %r12.
   Expects old stack in %rsi and target stack in %r10.
   Leaves old stack in %rsi and target stack in %r10. */
#define SWITCH_OCAML_STACKS                               \
        ENTER_FUNCTION                                    \
    /* Save OCaml SP and exn_handler in the stack info */ \
        movq    %rsp, Stack_sp(%rsi);                     \
        movq    Caml_state(exn_handler), %r12;            \
        movq    %r12, Stack_exception(%rsi);              \
    /* switch stacks */                                   \
        movq    %r10, Caml_state(current_stack);          \
        movq    Stack_sp(%r10), %rsp;                     \
        CFI_DEF_CFA_OFFSET(8);                            \
    /* restore exn_handler for new stack */               \
        movq    Stack_exception(%r10), %r12;              \
        movq    %r12, Caml_state(exn_handler);            \
        LEAVE_FUNCTION

/* Updates the oldest saved frame pointer in the target fiber.

   A fiber stack may need to grow, causing the reallocation of the entire fiber,
   including stack_info and stack_handler structures.
   caml_try_realloc_stack will not be able to update the linked list of
   frame-pointers if it has been split (ie, in a continuation).
   caml_resume and caml_reperform use this macro to update the oldest saved rbp
   (highest one in the stack) in case the fiber was reallocated to reattach the
   frame-pointer linked list.

   REG: Stack_handler(target_fiber)

   The frame pointer will be pushed into the stack immediately after these
   instructions. The offset of the oldest saved rbp in a fiber from the stack
   handler is 48 = 4 words (caml_runstack) + 2 words (rip and rbp).
   */
#ifdef WITH_FRAME_POINTERS
#define UPDATE_BASE_POINTER(REG)                          \
        leaq    -8(%rsp), %r11;                           \
        movq    %r11, -48(REG)
#else
#define UPDATE_BASE_POINTER(REG)
#endif

/******************************************************************************/
/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */
/******************************************************************************/

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

#define PUSH_CALLEE_SAVE_REGS                               \
        pushq   %rbx; CFI_ADJUST (8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST (8); CFI_OFFSET(rbp, -24); \
                    /* Allows debugger to walk the stack */ \
        pushq   %rsi; CFI_ADJUST (8); CFI_OFFSET(rsi, -32); \
        pushq   %rdi; CFI_ADJUST (8); CFI_OFFSET(rdi, -40); \
        pushq   %r12; CFI_ADJUST (8); CFI_OFFSET(r12, -48); \
        pushq   %r13; CFI_ADJUST (8); CFI_OFFSET(r13, -56); \
        pushq   %r14; CFI_ADJUST (8); CFI_OFFSET(r14, -64); \
        pushq   %r15; CFI_ADJUST (8); CFI_OFFSET(r15, -72); \
        subq    $(10*16), %rsp; CFI_ADJUST (10*16);         \
        movupd  %xmm6, 0*16(%rsp);                          \
        movupd  %xmm7, 1*16(%rsp);                          \
        movupd  %xmm8, 2*16(%rsp);                          \
        movupd  %xmm9, 3*16(%rsp);                          \
        movupd  %xmm10, 4*16(%rsp);                         \
        movupd  %xmm11, 5*16(%rsp);                         \
        movupd  %xmm12, 6*16(%rsp);                         \
        movupd  %xmm13, 7*16(%rsp);                         \
        movupd  %xmm14, 8*16(%rsp);                         \
        movupd  %xmm15, 9*16(%rsp)

#define POP_CALLEE_SAVE_REGS                                \
        movupd  0*16(%rsp), %xmm6;                          \
        movupd  1*16(%rsp), %xmm7;                          \
        movupd  2*16(%rsp), %xmm8;                          \
        movupd  3*16(%rsp), %xmm9;                          \
        movupd  4*16(%rsp), %xmm10;                         \
        movupd  5*16(%rsp), %xmm11;                         \
        movupd  6*16(%rsp), %xmm12;                         \
        movupd  7*16(%rsp), %xmm13;                         \
        movupd  8*16(%rsp), %xmm14;                         \
        movupd  9*16(%rsp), %xmm15;                         \
        addq    $(10*16), %rsp; CFI_ADJUST (-10*16);        \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15);  \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14);  \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13);  \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12);  \
        popq    %rdi; CFI_ADJUST(-8); CFI_SAME_VALUE(rdi);  \
        popq    %rsi; CFI_ADJUST(-8); CFI_SAME_VALUE(rsi);  \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp);  \
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
#  define STACK_PROBE_SIZE 4096
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#  define STACK_PROBE_SIZE 4096
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

        .text

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot.code_begin)
        .globl  G(caml_hot.code_begin)
G(caml_hot.code_begin):

        TEXT_SECTION(caml_hot.code_end)
        .globl  G(caml_hot.code_end)
G(caml_hot.code_end):
#endif

/******************************************************************************/
/* text section */
/******************************************************************************/

        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */

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

FUNCTION(G(caml_call_realloc_stack))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        SAVE_ALL_REGS
        movq    RETADDR_ENTRY_SIZE(%rsp), C_ARG_1 /* argument */
        SWITCH_OCAML_TO_C
        C_call  (GCALL(caml_try_realloc_stack))
        SWITCH_C_TO_OCAML
        cmpq    $0, %rax
        jz      1f
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        ret
1:      RESTORE_ALL_REGS
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        add     $16, %rsp /* pop argument, retaddr */
        jmp     GCALL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_call_realloc_stack))

FUNCTION(G(caml_call_gc))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
LBL(caml_call_gc):
        SAVE_ALL_REGS
        movq    %r15, Caml_state(gc_regs)
        SWITCH_OCAML_TO_C
        C_call (GCALL(caml_garbage_collection))
        SWITCH_C_TO_OCAML
        movq    Caml_state(gc_regs), %r15
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc))

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
        ENTER_FUNCTION
        subq    $16, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1))

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
        ENTER_FUNCTION
        subq    $24, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2))

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
        ENTER_FUNCTION
        subq    $32, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3))

FUNCTION(G(caml_allocN))
CFI_STARTPROC
        ENTER_FUNCTION
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN))

/******************************************************************************/
/* Call a C function from OCaml */
/******************************************************************************/

FUNCTION(G(caml_c_call))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
LBL(caml_c_call):
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the alloc ptr available to the C code */
        movq    %r15, Caml_state(young_ptr)
    /* Call the function (address in %rax) */
        C_call  (*%rax)
    /* Prepare for return to OCaml */
        movq    Caml_state(young_ptr), %r15
    /* Load ocaml stack and restore global variables */
        SWITCH_C_TO_OCAML
        LEAVE_FUNCTION
    /* Return to OCaml caller */
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call))

FUNCTION(G(caml_c_call_stack_args))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax
        C stack args        : begin=%r13 end=%r12 */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* we use %rbx (otherwise unused) to enable backtraces */
        movq    %rsp, %rbx
#ifdef ASM_CFI_SUPPORTED
        .cfi_escape DW_CFA_def_cfa_expression, 5,           \
          /* %rbp points to the c_stack_link structure */   \
          DW_OP_breg + DW_REG_rbx, Cstack_sp, DW_OP_deref,  \
          DW_OP_plus_uconst, RETADDR_ENTRY_SIZE
#endif
    /* Make the alloc ptr available to the C code */
        movq    %r15, Caml_state(young_ptr)
    /* Copy arguments from OCaml to C stack */
LBL(105):
        subq    $8, %r12
        cmpq    %r13,%r12
        jb      LBL(106)
        push    (%r12); CFI_ADJUST(8)
        jmp     LBL(105)
LBL(106):
    /* Call the function (address in %rax) */
        C_call  (*%rax)
    /* Pop arguments back off the stack */
        movq    Caml_state(c_stack), %rsp
    /* Prepare for return to OCaml */
        movq    Caml_state(young_ptr), %r15
    /* Load ocaml stack and restore global variables */
        SWITCH_C_TO_OCAML
    /* Return to OCaml caller */
        LEAVE_FUNCTION
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_stack_args))

/******************************************************************************/
/* Start the OCaml program */
/******************************************************************************/

FUNCTION(G(caml_start_program))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load Caml_state into r14 (was passed as an argument from C) */
        movq    C_ARG_1, %r14
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
#ifdef DEBUG
        movq    $0, %rax  /* dummy */
        movq    $0, %rbx  /* dummy */
        movq    $0, %rdi  /* dummy */
        movq    $0, %rsi  /* dummy */
#endif
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
    /* Store the stack pointer to allow DWARF unwind */
        subq    $16, %r10
        movq    %rsp, 0(%r10) /* C_STACK_SP */
    /* Store the gc_regs for callbacks during a GC */
        movq    Caml_state(gc_regs), %r11
        movq    %r11, 8(%r10)
    /* Build a handler for exceptions raised in OCaml on the OCaml stack. */
        subq    $16, %r10
        lea     LBL(109)(%rip), %r11
        movq    %r11, 8(%r10)
    /* link in the previous exn_handler so that copying stacks works */
        movq    Caml_state(exn_handler), %r11
        movq    %r11, 0(%r10)
        movq    %r10, Caml_state(exn_handler)
    /* Switch stacks and call the OCaml code */
        movq    %r10, %rsp
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        .cfi_escape DW_CFA_def_cfa_expression, 3 + 2,                 \
            /* %rsp points to the exn handler on the OCaml stack */   \
            /* %rsp + 16 contains the C_STACK_SP */                   \
          DW_OP_breg + DW_REG_rsp, 16 /* exn handler */, DW_OP_deref, \
          DW_OP_plus_uconst,                                          \
             24  /* struct c_stack_link */ +                          \
             6*8 /* callee save regs */ +                             \
             8   /* ret addr */
#endif
        call    *%r12
LBL(108):
    /* pop exn handler */
        movq    0(%rsp), %r11
        movq    %r11, Caml_state(exn_handler)
        leaq    16(%rsp), %r10
1:  /* restore GC regs */
        movq    8(%r10), %r11
        movq    %r11, Caml_state(gc_regs)
        addq    $16, %r10
    /* Update alloc ptr */
        movq    %r15, Caml_state(young_ptr)
    /* Return to C stack. */
        movq    Caml_state(current_stack), %r11
        movq    %r10, Stack_sp(%r11)
        movq    Caml_state(c_stack), %rsp
        CFI_RESTORE_STATE
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
        /* exn handler already popped here */
        movq    %rsp, %r10
        jmp     1b
CFI_ENDPROC
ENDFUNCTION(G(caml_start_program))

/******************************************************************************/
/* Exceptions */
/******************************************************************************/

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
        ENTER_FUNCTION
LBL(caml_raise_exn):
        testq   $1, Caml_state(backtrace_active)
        jne   LBL(116)
        RESTORE_EXN_HANDLER_OCAML
        ret
LBL(116):
        movq    $0, Caml_state(backtrace_pos)
LBL(117):
        movq    %rsp, %r10        /* Save OCaml stack pointer */
        movq    %rax, %r12        /* Save exception bucket */
        movq    Caml_state(c_stack), %rsp
        movq    %rax, C_ARG_1     /* arg 1: exception bucket */
#ifdef WITH_FRAME_POINTERS
        movq    8(%r10), C_ARG_2   /* arg 2: pc of raise */
        leaq    16(%r10), C_ARG_3  /* arg 3: sp at raise */
#else
        movq    (%r10), C_ARG_2   /* arg 2: pc of raise */
        leaq    8(%r10), C_ARG_3  /* arg 3: sp at raise */
#endif
        movq    Caml_state(exn_handler), C_ARG_4
                                  /* arg 4: sp of handler */
        C_call  (GCALL(caml_stash_backtrace))
        movq    %r12, %rax        /* Recover exception bucket */
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exn))

FUNCTION(G(caml_reraise_exn))
CFI_STARTPROC
        ENTER_FUNCTION
        testq   $1, Caml_state(backtrace_active)
        jne   LBL(117)
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_reraise_exn))

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        ENTER_FUNCTION
        movq    C_ARG_1, %r14                /* Caml_state */
        movq    C_ARG_2, %rax
    /* Load young_ptr into %r15 */
        movq    Caml_state(young_ptr), %r15
    /* Discard the C stack pointer and reset to ocaml stack */
        movq    Caml_state(current_stack), %r10
        movq    Stack_sp(%r10), %rsp         /* FIXME: CFI */
        jmp LBL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exception))

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
ENDFUNCTION(G(caml_callback_asm))

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
ENDFUNCTION(G(caml_callback2_asm))

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
ENDFUNCTION(G(caml_callback3_asm))

/******************************************************************************/
/* Fibers */
/*
 * A continuation is a one word object that points to a fiber. A fiber [f] will
 * point to its parent at Handler_parent(Stack_handler(f)). In the following,
 * the [last_fiber] refers to the last fiber in the linked-list formed by the
 * parent pointer.
 */
/******************************************************************************/

FUNCTION(G(caml_perform))
CFI_STARTPROC
    /*  %rax: effect to perform
        %rbx: freshly allocated continuation */
        movq    Caml_state(current_stack), %rsi /* %rsi := old stack */
        leaq    1(%rsi), %rdi /* %rdi (last_fiber) := Val_ptr(old stack) */
        movq    %rdi, 0(%rbx) /* Initialise continuation */
LBL(do_perform):
    /*  %rsi: old stack */
        movq    Stack_handler(%rsi), %r11  /* %r11 := old stack -> handler */
        movq    Handler_parent(%r11), %r10 /* %r10 := parent stack */
        cmpq    $0, %r10                   /* parent is NULL? */
        je      LBL(112)
        SWITCH_OCAML_STACKS /* preserves r11 and rsi */
     /* We have to null the Handler_parent after the switch because the
        Handler_parent is needed to unwind the stack for backtraces */
        movq    $0, Handler_parent(%r11) /* Set parent of performer to NULL */
        movq    Handler_effect(%r11), %rsi  /* %rsi := effect handler */
        jmp     GCALL(caml_apply3)
LBL(112):
    /* Switch back to original performer before raising Unhandled
       (no-op unless this is a reperform) */
        movq    0(%rbx), %r10  /* load performer stack from continuation */
        subq    $1, %r10       /* r10 := Ptr_val(r10) */
        movq    Caml_state(current_stack), %rsi
        SWITCH_OCAML_STACKS
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_exn_Unhandled, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_perform))

FUNCTION(G(caml_reperform))
CFI_STARTPROC
    /*  %rax: effect to reperform
        %rbx: continuation
        %rdi: last_fiber */
        movq    Caml_state(current_stack), %rsi  /* %rsi := old stack */
        movq    (Stack_handler-1)(%rdi), %r10
        movq    %rsi, Handler_parent(%r10)       /* Append to last_fiber */
        leaq    1(%rsi), %rdi  /* %rdi (last_fiber) := Val_ptr(old stack) */
     /* Need to update the oldest saved frame pointer here as the execution of
        the handler may have caused the current fiber stack to reallocate. */
        UPDATE_BASE_POINTER(%r10)
        jmp     LBL(do_perform)
CFI_ENDPROC
ENDFUNCTION(G(caml_reperform))

FUNCTION(G(caml_resume))
CFI_STARTPROC
    /* %rax -> fiber, %rbx -> fun, %rdi -> arg */
        leaq    -1(%rax), %r10  /* %r10 (new stack) = Ptr_val(%rax) */
        movq    %rdi, %rax      /* %rax := argument to function in %rbx */
    /*  check if stack null, then already used */
        testq   %r10, %r10
        jz      2f
    /* Find end of list of stacks and add current */
        movq    %r10, %rsi
1:      movq    Stack_handler(%rsi), %rcx
        movq    Handler_parent(%rcx), %rsi
        testq   %rsi, %rsi
        jnz     1b
        movq    Caml_state(current_stack), %rsi
        movq    %rsi, Handler_parent(%rcx)
     /* Need to update the oldest saved frame pointer here as the current fiber
        stack may have been reallocated or we may be resuming a computation
        that was not originally run here. */
        UPDATE_BASE_POINTER(%rcx)
        SWITCH_OCAML_STACKS
        jmp     *(%rbx)
2:      LEA_VAR(caml_exn_Continuation_already_taken, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_resume))

/* Run a function on a new stack,
   then invoke either the value or exception handler */
FUNCTION(G(caml_runstack))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
    /* %rax -> fiber, %rbx -> fun, %rdi -> arg */
        andq    $-2, %rax       /* %rax = Ptr_val(%rax) */
    /* save old stack pointer and exception handler */
        movq    Caml_state(current_stack), %rcx
        movq    Caml_state(exn_handler), %r10
        movq    %rsp, Stack_sp(%rcx)
        movq    %r10, Stack_exception(%rcx)
    /* Load new stack pointer and set parent */
        movq    Stack_handler(%rax), %r11
        movq    %rcx, Handler_parent(%r11)
        movq    %rax, Caml_state(current_stack)
        movq    Stack_sp(%rax), %r11
    /* Create an exception handler on the target stack
       after 16byte DWARF & gc_regs block (which is unused here) */
        subq    $32, %r11
        leaq    LBL(fiber_exn_handler)(%rip), %r10
        movq    %r10, 8(%r11)
    /* link the previous exn_handler so that copying stacks works */
        movq    Stack_exception(%rax), %r10
        movq    %r10, 0(%r11)
        movq    %r11, Caml_state(exn_handler)
    /* Switch to the new stack */
        movq    %r11, %rsp
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        .cfi_escape DW_CFA_def_cfa_expression, 3+3+2, \
          DW_OP_breg + DW_REG_rsp,                    \
            16 /* exn */ +                            \
            8 /* gc_regs slot (unused) */ +           \
            8 /* C_STACK_SP for DWARF (unused) */     \
            + Handler_parent, DW_OP_deref,            \
          DW_OP_plus_uconst, Stack_sp, DW_OP_deref,   \
          DW_OP_plus_uconst, RETADDR_ENTRY_SIZE
#endif
        movq    %rdi, %rax /* first argument */
        callq   *(%rbx) /* closure in %rbx (second argument) */
LBL(frame_runstack):
        leaq    32(%rsp), %r11 /* SP with exn handler popped */
        movq    Handler_value(%r11), %rbx
1:      movq    Caml_state(current_stack), C_ARG_1 /* arg to caml_free_stack */
    /* restore parent stack and exn_handler into Caml_state */
        movq    Handler_parent(%r11), %r10
        movq    Stack_exception(%r10), %r11
        movq    %r10, Caml_state(current_stack)
        movq    %r11, Caml_state(exn_handler)
    /* free old stack by switching directly to c_stack; is a no-alloc call */
        movq    Stack_sp(%r10), %r13 /* saved across C call */
        CFI_RESTORE_STATE
        CFI_REMEMBER_STATE
        CFI_DEF_CFA_REGISTER(DW_REG_r13)
        movq    %rax, %r12 /* save %rax across C call */
        movq    Caml_state(c_stack), %rsp
        C_call  (GCALL(caml_free_stack))
    /* switch directly to parent stack with correct return */
        movq    %r13, %rsp
        CFI_RESTORE_STATE
        movq    %r12, %rax
    /* Invoke handle_value (or handle_exn) */
        LEAVE_FUNCTION
        jmp     *(%rbx)
LBL(fiber_exn_handler):
        leaq    16(%rsp), %r11
        movq    Handler_exception(%r11), %rbx
        jmp     1b
CFI_ENDPROC
ENDFUNCTION(G(caml_runstack))

FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error_asm, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_bound_error))

FUNCTION(G(caml_assert_stack_invariants))
CFI_STARTPROC
/*      CHECK_STACK_ALIGNMENT */
        movq    Caml_state(current_stack), %r11
        movq    %rsp, %r10
        subq    %r11, %r10      /* %r10: number of bytes left on stack */
        /* can be two words over: the return addresses */
        cmp     $((Stack_threshold_words + Stack_ctx_words - 2)*8), %r10
        jge     1f
        int3
1:      ret
CFI_ENDPROC
ENDFUNCTION(G(caml_assert_stack_invariants))

        TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system.frametable)
        .align  EIGHT_ALIGN
G(caml_system.frametable):
        .quad   2           /* two descriptors */
        .quad   LBL(108)    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN
        .quad   LBL(frame_runstack) /* return address into fiber_val_handler */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst16,"aM",@progbits,16
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
