/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            */
/*                          Bill O'Farrell, IBM                           */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/m.h"

/* Special registers */

#define DOMAIN_STATE_PTR %r10
#define ALLOC_PTR %r11
#define TRAP_PTR %r13
#define TMP %r1
#define TMP2 %r12       /* callee-saved register, use with caution */
#define TMP3 %r0
/* Don't use TMP3 for indexed access in form of offset(TMP3) */

#define C_ARG_1 %r2
#define C_ARG_2 %r3
#define C_ARG_3 %r4
#define C_ARG_4 %r5
#define ADDITIONAL_ARG %r7

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOT
#define GCALL(r) r@PLT

#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) \
        .section .text.caml.##name,"ax",@progbits
#else
#define TEXT_SECTION(name) \
        .section .text
#endif

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot.code_begin)
        .globl caml_hot.code_begin
caml_hot.code_begin:

        TEXT_SECTION(caml_hot.code_end)
        .globl caml_hot.code_end
caml_hot.code_end:
#endif

#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        name:

#define ENDFUNCTION(name)

#define OBJECT(name) \
        .align 8; \
        .globl name; \
        .type name, @object; \
        name:

#define ENDOBJECT(name)

/* Stack space to be reserved by the caller of a C function */
#define RESERVED_STACK          160

/* special sleb128 constants, precalculated */
/* Cstack_sp + 160 = 8 + 160 = 168, encoded as sleb128 */
#define Cstack_sp_plus_160_sleb128_2byte 168, 1

/* struct c_stack_link + callee save regs = 24 + 8*8 = 88 */
#define start_program_sleb128_2byte 216, 0

/* exception handler + gc_regs slot + C_STACK_SP + Handler_parent
   = 16 + 8 + 8 + 24 = 56 */
#define caml_runstack_sleb128_1byte 56

#define RETADDR_ENTRY_SIZE   8 /* retaddr */

/* Function prologue and epilogue */

#define ENTER_FUNCTION \
        lay     %r15, -8(%r15); \
        CFI_ADJUST(8); \
        stg     %r14, 0(%r15)

#define LEAVE_FUNCTION \
        lg      %r14, 0(%r15); \
        CFI_RESTORE(14); \
        la      %r15, 8(%r15); \
        CFI_ADJUST(-8)

#define PREPARE_FOR_C_CALL      CFI_REMEMBER_STATE
#define CLEANUP_AFTER_C_CALL    CFI_RESTORE_STATE

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
#define Cstack_stack           (RESERVED_STACK + 0)
#define Cstack_sp              (RESERVED_STACK + 8)
#define Cstack_prev            (RESERVED_STACK + 16)
#define SIZEOF_C_STACK_LINK    (RESERVED_STACK + 24)

/******************************************************************************/
/* DWARF */
/******************************************************************************/

/* These s390x-specific register numbers are taken from
   Table 1.17 ("DWARF Register Number Mapping") of:

     ELF Application Binary Interface
     s390x Supplement
     Version 1.6
     https://github.com/IBM/s390x-abi/releases/download/v1.6/lzsabi_s390x.pdf
*/

#define DW_REG_r9                 9
#define DW_REG_r12                12
#define DW_REG_r15                15

#include "../runtime/caml/asm.h"

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

#define Caml_state(var) 8*domain_field_caml_##var(%r10)

/* Invoke the garbage collector. */

        TEXT_SECTION(caml_system__code_begin)
        .globl  caml_system__code_begin
caml_system__code_begin:

#if defined(__PIC__)
#define LEA_VAR(label,dst) \
        lgrl  dst, GREL(label)
#else
#define LEA_VAR(label,dst) \
        larl    dst, G(label)
#endif

/******************************************************************************/
/* Stack switching operations */
/******************************************************************************/

#ifdef ASM_CFI_SUPPORTED
#define SWITCH_OCAML_TO_C_CFI                                   \
        CFI_REMEMBER_STATE;                                     \
        CFI_OFFSET(14, 0); \
          /* %r15 points to the c_stack_link. */                \
        .cfi_escape DW_CFA_def_cfa_expression, 3,               \
          DW_OP_breg + DW_REG_r15, Cstack_sp, DW_OP_deref
#else
#define SWITCH_OCAML_TO_C_CFI
#endif

/* Switch from OCaml to C stack. Clobbers %r1, %r12. */
#define SWITCH_OCAML_TO_C                                  \
    /* Fill in Caml_state->current_stack->sp */            \
        lg      TMP,  Caml_state(current_stack);           \
        stg     %r15, Stack_sp(TMP);                       \
    /* Fill in Caml_state->c_stack */                      \
        lg      TMP2, Caml_state(c_stack);                 \
        stg     TMP,  Cstack_stack(TMP2);                  \
        stg     %r15, Cstack_sp(TMP2);                     \
    /* Switch to C stack */                                \
        lgr     %r15, TMP2;                                \
        SWITCH_OCAML_TO_C_CFI

/* Switch from C to OCaml stack. */
#define SWITCH_C_TO_OCAML                                           \
        lg     %r15, Cstack_sp(%r15);                               \
        CFI_RESTORE_STATE

#define SWITCH_OCAML_STACKS(old_stack, new_stack) \
    /* Save return address for old_stack */   \
        ENTER_FUNCTION;                                         \
    /* Save OCaml SP and exn_handler in the stack info */       \
        stg     %r15, Stack_sp(old_stack);                      \
        stg     TRAP_PTR, Stack_exception(old_stack);           \
    /* switch stacks */                                         \
        stg     new_stack, Caml_state(current_stack);           \
        lg      %r15,      Stack_sp(new_stack);                 \
    /* restore exn_handler for new stack */                     \
        lg      TRAP_PTR,  Stack_exception(new_stack);          \
    /* Restore return address for new_stack */                  \
        LEAVE_FUNCTION

/******************************************************************************/
/* Allocation */
/******************************************************************************/

/* Save all of the registers that may be in use to a free gc_regs bucket
   and store ALLOC_PTR and TRAP_PTR back to Caml_state
   At the end the saved registers are placed in Caml_state(gc_regs)
 */
#define SAVE_ALL_REGS                                  \
    /* First, save the young_ptr. */                   \
        stg     ALLOC_PTR, Caml_state(young_ptr);      \
        stg     TRAP_PTR,  Caml_state(exn_handler);    \
    /* Now, use ALLOC_PTR to point to the gc_regs bucket */  \
        lg      ALLOC_PTR, Caml_state(gc_regs_buckets);\
        lg      %r0,            0(ALLOC_PTR); /* next ptr */ \
        stg     %r0, Caml_state(gc_regs_buckets);      \
    /* Save allocatable registers */                   \
        stmg    %r2,%r9,    (2*8)(ALLOC_PTR);          \
        stg     %r12,        10*8(ALLOC_PTR);          \
        std     %f0,     (0+11)*8(ALLOC_PTR);          \
        std     %f1,     (1+11)*8(ALLOC_PTR);          \
        std     %f2,     (2+11)*8(ALLOC_PTR);          \
        std     %f3,     (3+11)*8(ALLOC_PTR);          \
        std     %f4,     (4+11)*8(ALLOC_PTR);          \
        std     %f5,     (5+11)*8(ALLOC_PTR);          \
        std     %f6,     (6+11)*8(ALLOC_PTR);          \
        std     %f7,     (7+11)*8(ALLOC_PTR);          \
        std     %f8,     (8+11)*8(ALLOC_PTR);          \
        std     %f9,     (9+11)*8(ALLOC_PTR);          \
        std     %f10,   (10+11)*8(ALLOC_PTR);          \
        std     %f11,   (11+11)*8(ALLOC_PTR);          \
        std     %f12,   (12+11)*8(ALLOC_PTR);          \
        std     %f13,   (13+11)*8(ALLOC_PTR);          \
        std     %f14,   (14+11)*8(ALLOC_PTR);          \
        std     %f15,   (15+11)*8(ALLOC_PTR);          \
        la      ALLOC_PTR, 16(ALLOC_PTR);              \
        stg     ALLOC_PTR, Caml_state(gc_regs);        \
        lg      ALLOC_PTR, Caml_state(young_ptr);

/* Undo SAVE_ALL_REGS. Expects gc_regs bucket in %r11 */
#define RESTORE_ALL_REGS                               \
        lg      ALLOC_PTR, Caml_state(gc_regs);        \
        lay     ALLOC_PTR, -16(ALLOC_PTR);             \
    /* Restore registers */                            \
        lmg     %r2,%r9,    (2*8)(ALLOC_PTR);          \
        lg      %r12,        10*8(ALLOC_PTR);          \
        ld      %f0,     (0+11)*8(ALLOC_PTR);          \
        ld      %f1,     (1+11)*8(ALLOC_PTR);          \
        ld      %f2,     (2+11)*8(ALLOC_PTR);          \
        ld      %f3,     (3+11)*8(ALLOC_PTR);          \
        ld      %f4,     (4+11)*8(ALLOC_PTR);          \
        ld      %f5,     (5+11)*8(ALLOC_PTR);          \
        ld      %f6,     (6+11)*8(ALLOC_PTR);          \
        ld      %f7,     (7+11)*8(ALLOC_PTR);          \
        ld      %f8,     (8+11)*8(ALLOC_PTR);          \
        ld      %f9,     (9+11)*8(ALLOC_PTR);          \
        ld      %f10,   (10+11)*8(ALLOC_PTR);          \
        ld      %f11,   (11+11)*8(ALLOC_PTR);          \
        ld      %f12,   (12+11)*8(ALLOC_PTR);          \
        ld      %f13,   (13+11)*8(ALLOC_PTR);          \
        ld      %f14,   (14+11)*8(ALLOC_PTR);          \
        ld      %f15,   (15+11)*8(ALLOC_PTR);          \
    /* Put gc_regs struct back in bucket linked list */\
        lg      %r0, Caml_state(gc_regs_buckets);      \
        stg     %r0,            0(ALLOC_PTR); /* next ptr */ \
        stg     ALLOC_PTR, Caml_state(gc_regs_buckets);\
    /* Reload new allocation pointer & exn handler */  \
        lg      ALLOC_PTR, Caml_state(young_ptr);      \
        lg      TRAP_PTR, Caml_state(exn_handler);

#if defined(WITH_THREAD_SANITIZER) /* { */

/* Setup a C call stack frame (which is the caller's duty), and save the
   current value of the return address to the stack.
   This is similar to ENTER_FUNCTION, but allocating more stack space. */
#define TSAN_SETUP_C_CALL                              \
        lay     %r15, -(RESERVED_STACK+8)(%r15);                  \
        CFI_ADJUST(RESERVED_STACK+8);                             \
        stg     %r14, (RESERVED_STACK+0)(%r15)

/* Restore the value of the return address from the stack and undo the C call
   stack frame. */
#define TSAN_CLEANUP_AFTER_C_CALL                      \
        lg      %r14, (RESERVED_STACK+0)(%r15);                   \
        CFI_RESTORE(14);                               \
        la      %r15, (RESERVED_STACK+8)(%r15);                   \
        CFI_ADJUST(-(RESERVED_STACK+8))

/* Invoke a C function, switching back and forth the OCaml and C stacks. */
#define TSAN_C_CALL(fun)                               \
        SWITCH_OCAML_TO_C;                             \
        TSAN_SETUP_C_CALL;                             \
        brasl %r14, GCALL(fun);                        \
        TSAN_CLEANUP_AFTER_C_CALL;                     \
        SWITCH_C_TO_OCAML

/* Invoke __tsan_func_entry(return address in the caller) */
#define TSAN_ENTER_FUNCTION                            \
        lgr     C_ARG_1, %r14; /* arg1: return address in caller */ \
        TSAN_C_CALL(__tsan_func_entry)

/* Invoke __tsan_func_exit(0) */
#define TSAN_EXIT_FUNCTION                             \
        lgfi    C_ARG_1, 0;                            \
        TSAN_C_CALL(__tsan_func_exit)

/* This is similar to SAVE_ALL_REGS, but only saving the caller-saved
   registers. */
#define TSAN_SAVE_CALLER_REGS                          \
    /* First, save the young_ptr. */                   \
        stg     ALLOC_PTR, Caml_state(young_ptr);      \
        stg     TRAP_PTR,  Caml_state(exn_handler);    \
    /* Now, use ALLOC_PTR to point to the gc_regs bucket */  \
        lg      ALLOC_PTR, Caml_state(gc_regs_buckets);\
        lg      %r0,            0(ALLOC_PTR); /* next ptr */ \
        stg     %r0, Caml_state(gc_regs_buckets);      \
    /* Save caller-saved registers */                  \
        stmg    %r2,%r5,    (2*8)(ALLOC_PTR);          \
        std     %f0,     (0+11)*8(ALLOC_PTR);          \
        std     %f1,     (1+11)*8(ALLOC_PTR);          \
        std     %f2,     (2+11)*8(ALLOC_PTR);          \
        std     %f3,     (3+11)*8(ALLOC_PTR);          \
        std     %f4,     (4+11)*8(ALLOC_PTR);          \
        std     %f5,     (5+11)*8(ALLOC_PTR);          \
        std     %f6,     (6+11)*8(ALLOC_PTR);          \
        std     %f7,     (7+11)*8(ALLOC_PTR);          \
        la      ALLOC_PTR, 16(ALLOC_PTR);              \
        stg     ALLOC_PTR, Caml_state(gc_regs);        \
        lg      ALLOC_PTR, Caml_state(young_ptr)

/* This is similar to RESTORE_ALL_REGS, but only restoring the caller-saved
   registers. */
#define TSAN_RESTORE_CALLER_REGS                       \
        lg      ALLOC_PTR, Caml_state(gc_regs);        \
        lay     ALLOC_PTR, -16(ALLOC_PTR);             \
    /* Restore registers */                            \
        lmg     %r2,%r5,    (2*8)(ALLOC_PTR);          \
        ld      %f0,     (0+11)*8(ALLOC_PTR);          \
        ld      %f1,     (1+11)*8(ALLOC_PTR);          \
        ld      %f2,     (2+11)*8(ALLOC_PTR);          \
        ld      %f3,     (3+11)*8(ALLOC_PTR);          \
        ld      %f4,     (4+11)*8(ALLOC_PTR);          \
        ld      %f5,     (5+11)*8(ALLOC_PTR);          \
        ld      %f6,     (6+11)*8(ALLOC_PTR);          \
        ld      %f7,     (7+11)*8(ALLOC_PTR);          \
    /* Put gc_regs struct back in bucket linked list */\
        lg      %r0, Caml_state(gc_regs_buckets);      \
        stg     %r0,            0(ALLOC_PTR); /* next ptr */ \
        stg     ALLOC_PTR, Caml_state(gc_regs_buckets);\
    /* Reload new allocation pointer & exn handler */  \
        lg      ALLOC_PTR, Caml_state(young_ptr);      \
        lg      TRAP_PTR, Caml_state(exn_handler)

#else /* } { */

#define TSAN_ENTER_FUNCTION
#define TSAN_EXIT_FUNCTION
#define TSAN_SAVE_CALLER_REGS
#define TSAN_RESTORE_CALLER_REGS

#endif /* } */

FUNCTION(G(caml_call_realloc_stack))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
        SAVE_ALL_REGS
        lgr    C_ARG_1, %r12 /* requested size */
        SWITCH_OCAML_TO_C
        PREPARE_FOR_C_CALL
#ifdef ASM_CFI_SUPPORTED
        .cfi_escape DW_CFA_def_cfa_expression, 4, DW_OP_breg + DW_REG_r15, \
          Cstack_sp_plus_160_sleb128_2byte, DW_OP_deref
#endif
        brasl %r14, GCALL(caml_try_realloc_stack)
        CLEANUP_AFTER_C_CALL
        SWITCH_C_TO_OCAML
        cgfi    %r2, 0
        je      LBL(120)
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        br      %r14
LBL(120):
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        LEA_VAR(caml_exn_Stack_overflow, %r2)
        brcl    15, GCALL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_call_realloc_stack))

FUNCTION(G(caml_call_gc))
CFI_STARTPROC
LBL(caml_call_gc):
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
        SAVE_ALL_REGS
        TSAN_ENTER_FUNCTION
        SWITCH_OCAML_TO_C
        PREPARE_FOR_C_CALL
#ifdef ASM_CFI_SUPPORTED
        .cfi_escape DW_CFA_def_cfa_expression, 4, DW_OP_breg + DW_REG_r15, \
          Cstack_sp_plus_160_sleb128_2byte, DW_OP_deref
#endif
        brasl %r14, GCALL(caml_garbage_collection)
        CLEANUP_AFTER_C_CALL
        SWITCH_C_TO_OCAML
        TSAN_EXIT_FUNCTION
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc))

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
        lay     ALLOC_PTR, -16(ALLOC_PTR)
        clg     ALLOC_PTR, Caml_state(young_limit)
        jl      LBL(caml_call_gc)
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1))

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
        lay     ALLOC_PTR, -24(ALLOC_PTR)
        clg     ALLOC_PTR, Caml_state(young_limit)
        jl      LBL(caml_call_gc)
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2))

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
        lay     ALLOC_PTR, -32(ALLOC_PTR)
        clg     ALLOC_PTR, Caml_state(young_limit)
        jl      LBL(caml_call_gc)
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3))

FUNCTION(G(caml_allocN))
CFI_STARTPROC
        clg     ALLOC_PTR, Caml_state(young_limit)
        jl      LBL(caml_call_gc)
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN))

/******************************************************************************/
/* Call a C function from OCaml */
/******************************************************************************/

#define RET_FROM_C_CALL                           \
        /* Test the least-significant byte of action_pending */ \
        cli     7+Caml_state(action_pending), 0;  \
        ber     %r14;                             \
        lghi    TMP, -1;                          \
        stg     TMP, Caml_state(young_limit);     \
        br      %r14

FUNCTION(G(caml_c_call))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
        TSAN_SAVE_CALLER_REGS
        TSAN_ENTER_FUNCTION
        TSAN_RESTORE_CALLER_REGS
LBL(caml_c_call):
    /* Arguments:
        C arguments         : %r2, %r3, %r4, %r5, %r6
        C function          : ADDITIONAL_ARG */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        stg     ALLOC_PTR, Caml_state(young_ptr)
        stg     TRAP_PTR, Caml_state(exn_handler)
    /* Call the function (address in ADDITIONAL_ARG) */
        PREPARE_FOR_C_CALL
#ifdef ASM_CFI_SUPPORTED
        .cfi_escape DW_CFA_def_cfa_expression, 4, DW_OP_breg + DW_REG_r15, \
          Cstack_sp_plus_160_sleb128_2byte, DW_OP_deref
#endif
        basr    %r14, ADDITIONAL_ARG
        CLEANUP_AFTER_C_CALL
    /* Reload new allocation pointer & exn handler */
        lg      ALLOC_PTR, Caml_state(young_ptr)
        lg      TRAP_PTR, Caml_state(exn_handler)
    /* Load ocaml stack and restore global variables */
        SWITCH_C_TO_OCAML
#if defined(WITH_THREAD_SANITIZER)
    /* Save return value registers. Since the called function could be
       anything, it may have returned its result (if any) either in %r2
       or %f0. */
        lay     %r15, -16(%r15)
        CFI_ADJUST(16)
        stg     %r2, 0(%r15)
        std     %f0, 8(%r15)
        TSAN_EXIT_FUNCTION
    /* Restore return value registers */
        ld      %f0, 8(%r15)
        lg      %r2, 0(%r15)
        la      %r15, 16(%r15)
        CFI_ADJUST(-16)
#endif
    /* Return to OCaml caller */
        LEAVE_FUNCTION
        RET_FROM_C_CALL
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call))

FUNCTION(G(caml_c_call_stack_args))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
    /* Arguments:
        C arguments         : %r2, %r3, %r4, %r5, %r6
        C function          : ADDITIONAL_ARG
        C stack args        : begin=%r9 end=%r8 */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        stg     ALLOC_PTR, Caml_state(young_ptr)
        stg     TRAP_PTR, Caml_state(exn_handler)
    /* Store sp to restore after call */
        lgr     %r12, %r15
#ifdef ASM_CFI_SUPPORTED
        .cfi_escape DW_CFA_def_cfa_expression, 3,           \
          /* %r12 points to the c_stack_link structure */   \
          DW_OP_breg + DW_REG_r12, Cstack_sp, DW_OP_deref
#endif
    /* Copy arguments from OCaml to C stack, always reserving
       the 160 bytes at the bottom of the C stack. */
LBL(105):
        lay     %r8, -8(%r8)
        clgr    %r8, %r9
        jl      LBL(106)
        lg      %r0, 0(%r8)
        lay     %r15, -8(%r15)
        stg     %r0, RESERVED_STACK(%r15)
        CFI_ADJUST(8)
        brcl    15, LBL(105)
LBL(106):
    /* Call the function (address in %r7) */
        PREPARE_FOR_C_CALL
        basr    %r14, ADDITIONAL_ARG
        CLEANUP_AFTER_C_CALL
    /* Restore stack */
        lgr     %r15, %r12
    /* Reload new allocation pointer & exn handler */
        lg      ALLOC_PTR, Caml_state(young_ptr)
        lg      TRAP_PTR, Caml_state(exn_handler)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return */
        LEAVE_FUNCTION
        RET_FROM_C_CALL
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_stack_args))

/******************************************************************************/
/* Start the OCaml program */
/******************************************************************************/

FUNCTION(G(caml_start_program))
CFI_STARTPROC
        CFI_SIGNAL_FRAME
#if defined(WITH_THREAD_SANITIZER)
        lay     %r15, -8(%r15)
        CFI_ADJUST(8)
        stg     C_ARG_1, 0(%r15)
    /* We can't use TSAN_ENTER_FUNCTION here, as it assumes to run on an
       OCaml stack, yet we are still on a C stack at this point. */
        lgr     C_ARG_1, %r14
        TSAN_SETUP_C_CALL
        brasl   %r14, GCALL(__tsan_func_entry)
        TSAN_CLEANUP_AFTER_C_CALL
        lg      C_ARG_1, 0(%r15)
        la      %r15, 8(%r15)
        CFI_ADJUST(-8)
#endif
    /* Load Caml_state into TMP (was passed as an argument from C) */
        lgr    TMP3, C_ARG_1
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, TMP)
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Allocate stack frame */
        lay     %r15, -RESERVED_STACK(%r15)
    /* Save all callee-save registers + return address */
    /* GPR 6..14 at sp + 0 ... sp + 64
       FPR 10..15 at sp + 72 ... sp + 128 */
        stmg    %r6,%r14, 0(%r15)
        CFI_OFFSET(14, 64 - RESERVED_STACK)
        std     %f8, 72(%r15)
        std     %f9, 80(%r15)
        std     %f10, 88(%r15)
        std     %f11, 96(%r15)
        std     %f12, 104(%r15)
        std     %f13, 112(%r15)
        std     %f14, 120(%r15)
        std     %f15, 128(%r15)
    /* Load domain state pointer from argument */
        lgr     DOMAIN_STATE_PTR, TMP3
    /* Reload allocation pointer */
        lg      ALLOC_PTR, Caml_state(young_ptr)
    /* Build struct c_stack_link on the C stack */
        lay     %r15, -SIZEOF_C_STACK_LINK(%r15)
        CFI_ADJUST(SIZEOF_C_STACK_LINK)
        lg      TMP3,  Caml_state(c_stack)
        lgfi    TMP2,  0
        stg     TMP2,  Cstack_stack(%r15)
        stg     TMP2,  Cstack_sp(%r15)
        stg     TMP3,  Cstack_prev(%r15)
        stg     %r15,  Caml_state(c_stack)
    /* Load the OCaml stack. */
        lg      %r8, Caml_state(current_stack)
        lg      %r8, Stack_sp(%r8)
    /* Store the stack pointer to allow DWARF unwind */
        lay     %r8, -16(%r8)
        stg     %r15, 0(%r8) /* C_STACK_SP */
    /* Store the gc_regs for callbacks during a GC */
        lg      %r9, Caml_state(gc_regs)
        stg     %r9, 8(%r8)
    /* Build a handler for exceptions raised in OCaml on the OCaml stack. */
        lay     %r8, -16(%r8)
    /* link in the previous exn_handler so that copying stacks works */
        lg      %r9, Caml_state(exn_handler)
        stg     %r9, 0(%r8)
        larl    %r9, LBL(trap_handler)
        stg     %r9, 8(%r8)
        stg     %r8, Caml_state(exn_handler)
        lgr     TRAP_PTR, %r8
    /* Switch stacks and call the OCaml code */
        lgr     %r15, %r8
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        CFI_OFFSET(14, 0)
        .cfi_escape DW_CFA_def_cfa_expression, 3 + 3,                 \
            /* %r15 points to the exn handler on the OCaml stack */   \
            /* %r15 + 16 contains the C_STACK_SP */                   \
          DW_OP_breg + DW_REG_r15, 16 /* exn handler */, DW_OP_deref, \
          DW_OP_plus_uconst, start_program_sleb128_2byte
#endif
        basr    %r14, TMP
LBL(caml_retaddr):
    /* pop exn handler */
        lg      %r8, 0(%r15)
        la      %r15, 16(%r15)
        stg     %r8, Caml_state(exn_handler)
LBL(return_result):  /* restore GC regs */
        lg      %r9, 8(%r15)
        la      %r15, 16(%r15)
        stg     %r9, Caml_state(gc_regs)
    /* Update alloc ptr */
        stg     ALLOC_PTR, Caml_state(young_ptr)
    /* Return to C stack. */
        lg      %r8, Caml_state(current_stack)
        stg     %r15, Stack_sp(%r8)
        lg      %r15, Caml_state(c_stack)
        CFI_RESTORE_STATE
    /* Pop the struct c_stack_link */
        lg      %r8, Cstack_prev(%r15)
        stg     %r8, Caml_state(c_stack)
        la      %r15, SIZEOF_C_STACK_LINK(%r15)
        CFI_ADJUST(SIZEOF_C_STACK_LINK)
#if defined(WITH_THREAD_SANITIZER)
    /* We can't use TSAN_EXIT_FUNCTION here, as it assumes to run on an
       OCaml stack, and we are back to a C stack at this point. */
        lay     %r15, -8(%r15)
        CFI_ADJUST(8)
        stg     C_ARG_1, 0(%r15)
        lgfi    C_ARG_1, 0
        TSAN_SETUP_C_CALL
        brasl   %r14, GCALL(__tsan_func_exit)
        TSAN_CLEANUP_AFTER_C_CALL
        lg      C_ARG_1, 0(%r15)
        la      %r15, 8(%r15)
        CFI_ADJUST(-8)
#endif
    /* Restore callee-save registers. */
        lmg     %r6,%r14, 0(%r15)
        CFI_RESTORE(14)
        ld      %f8, 72(%r15)
        ld      %f9, 80(%r15)
        ld      %f10, 88(%r15)
        ld      %f11, 96(%r15)
        ld      %f12, 104(%r15)
        ld      %f13, 112(%r15)
        ld      %f14, 120(%r15)
        ld      %f15, 128(%r15)
    /* Return */
        la     %r15, RESERVED_STACK(%r15)
    /* Return to caller. */
        br      %r14
LBL(trap_handler):
    /* Exception handler*/
        stg     TRAP_PTR, Caml_state(exn_handler)
    /* Mark the bucket as an exception result and return it */
        oill     %r2,  2
        j       LBL(return_result)
CFI_ENDPROC
ENDFUNCTION(G(caml_start_program))

/******************************************************************************/
/* Exceptions */
/******************************************************************************/

#define JUMP_TO_TRAP_PTR \
        lgr     %r15,     TRAP_PTR;                    \
        lg      TMP,       8(%r15);                    \
        lg      TRAP_PTR,  0(%r15);                    \
        la      %r15,     16(%r15);                    \
        br      TMP;

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
LBL(caml_raise_exn):
        clghsi  Caml_state(backtrace_active), 0
        jne     LBL(117)
LBL(116):
        JUMP_TO_TRAP_PTR
LBL(117):
        /* Zero backtrace_pos */
        lgfi    TMP, 0
        stg     TMP, Caml_state(backtrace_pos)
LBL(caml_reraise_exn_stash):
        lgr     %r9, %r2          /* Save exception bucket */
    /* Stash the backtrace */
                                  /* arg1: exception bucket, already in r2 */
        lgr     C_ARG_2, %r14     /* arg2: PC of raise */
        lgr     C_ARG_3, %r15     /* arg3: SP of raise */
        lgr     C_ARG_4, TRAP_PTR /* arg4: SP of handler */
    /* Switch to C stack */
        lg      %r15, Caml_state(c_stack)
        PREPARE_FOR_C_CALL
        CFI_ADJUST(RESERVED_STACK)
        brasl %r14, GCALL(caml_stash_backtrace)
        CLEANUP_AFTER_C_CALL
    /* Restore exception bucket and raise */
        lgr     %r2, %r9
        brcl    15, LBL(116)
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exn))

FUNCTION(G(caml_reraise_exn))
CFI_STARTPROC
        clghsi  Caml_state(backtrace_active), 0
        jne     LBL(caml_reraise_exn_stash)
        JUMP_TO_TRAP_PTR
CFI_ENDPROC
ENDFUNCTION(G(caml_reraise_exn))

#if defined(WITH_THREAD_SANITIZER)
/* When TSan support is enabled, this routine should be called just before
   raising an exception. It calls __tsan_func_exit for every OCaml frame about
   to be exited due to the exception.
   Takes no arguments, clobbers a0, a1, a2 and potentially all
   caller-saved registers of the C calling convention. */
FUNCTION(G(caml_tsan_exit_on_raise_asm))
CFI_STARTPROC
        lgr     C_ARG_1, %r14           /* arg1: pc of raise */
        lgr     C_ARG_2, %r15           /* arg2: sp of raise */
        lgr     C_ARG_3, TRAP_PTR       /* arg3: sp of handler */
        TSAN_C_CALL(caml_tsan_exit_on_raise)
        br      %r14
CFI_ENDPROC
ENDFUNCTION(G(caml_tsan_exit_on_raise_asm))
#endif

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
    /* Load the domain state ptr */
        lgr     DOMAIN_STATE_PTR, C_ARG_1
    /* Load the exception bucket */
        lgr     C_ARG_1,  C_ARG_2
    /* Reload trap ptr and alloc ptr */
        lg      TRAP_PTR, Caml_state(exn_handler)
        lg      ALLOC_PTR, Caml_state(young_ptr)
    /* Discard the C stack pointer and reset to ocaml stack */
        lg      TMP, Caml_state(current_stack)
        lg      %r15, Stack_sp(TMP)
#if defined(WITH_THREAD_SANITIZER)
        lgr     C_ARG_2, %r15
        lay     %r15, -8(%r15)
        CFI_ADJUST(8)
        stg     C_ARG_1, 0(%r15)        /* preserve exception bucket */
    /* Call __tsan_func_exit for every OCaml stack frame exited due to the
       exception */
        lg      C_ARG_1, 0(C_ARG_2)     /* arg1: pc of raise */
        /* This stack address adjustment is required to compensate the
           saving of r14 in SWITCH_OCAML_STACKS, which causes Stack_sp()
           to be 8 bytes lower than expected. */
        la      C_ARG_2, 8(C_ARG_2)     /* arg2: sp of raise */
        lgr     C_ARG_3, TRAP_PTR       /* arg3: sp of handler */
        TSAN_C_CALL(caml_tsan_exit_on_raise)
        lg      C_ARG_1, 0(%r15)
        la      %r15, 8(%r15)
        CFI_ADJUST(-8)
#endif
    /* Restore frame and link on return to OCaml */
        LEAVE_FUNCTION
        brcl    15, LBL(caml_raise_exn)
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exception))

/******************************************************************************/
/* Callback from C to OCaml */
/******************************************************************************/

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2, r3, r4 and r14 before C call */
        lay     %r15, -(RESERVED_STACK+32)(%r15)
        CFI_ADJUST(RESERVED_STACK+32)
        stg     C_ARG_1, (RESERVED_STACK+0)(%r15)
        stg     C_ARG_2, (RESERVED_STACK+8)(%r15)
        stg     C_ARG_3, (RESERVED_STACK+16)(%r15)
        stg     %r14, (RESERVED_STACK+24)(%r15)
        lgr     C_ARG_1, %r14
        brasl   %r14, GCALL(__tsan_func_entry)
        lg      %r14, (RESERVED_STACK+24)(%r15)
        lg      C_ARG_3, (RESERVED_STACK+16)(%r15)
        lg      C_ARG_2, (RESERVED_STACK+8)(%r15)
        lg      C_ARG_1, (RESERVED_STACK+0)(%r15)
        la      %r15, (RESERVED_STACK+32)(%r15)
        CFI_ADJUST(-(RESERVED_STACK+32))
#endif
    /* Initial shuffling of arguments */
    /* (%r2 = Caml_state, %r3 = closure, 0(%r4) = first arg) */
        lgr     TMP3, C_ARG_1        /* Caml_state */
        lg      C_ARG_1, 0(C_ARG_3)  /* %r2 - first arg */
                                     /* %r3 - closure environment */
        lg      TMP,     0(C_ARG_2)  /* code pointer */
        brcl    15, LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback_asm))

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2, r3, r4 and r14 before C call */
        lay     %r15, -(RESERVED_STACK+32)(%r15)
        CFI_ADJUST(RESERVED_STACK+32)
        stg     C_ARG_1, (RESERVED_STACK+0)(%r15)
        stg     C_ARG_2, (RESERVED_STACK+8)(%r15)
        stg     C_ARG_3, (RESERVED_STACK+16)(%r15)
        stg     %r14, (RESERVED_STACK+24)(%r15)
        lgr     C_ARG_1, %r14
        brasl   %r14, GCALL(__tsan_func_entry)
        lg      %r14, (RESERVED_STACK+24)(%r15)
        lg      C_ARG_3, (RESERVED_STACK+16)(%r15)
        lg      C_ARG_2, (RESERVED_STACK+8)(%r15)
        lg      C_ARG_1, (RESERVED_STACK+0)(%r15)
        la      %r15, (RESERVED_STACK+32)(%r15)
        CFI_ADJUST(-(RESERVED_STACK+32))
#endif
    /* Initial shuffling of arguments */
    /* (%r2 = Caml_state, %r3 = closure, 0(%r4) = arg1, 8(%r4) = arg2) */
        lgr     TMP3, C_ARG_1       /* Caml_state */
        lgr     TMP, C_ARG_2
        lg      C_ARG_1, 0(C_ARG_3) /* first argument */
        lg      C_ARG_2, 8(C_ARG_3) /* second argument */
        lgr     C_ARG_3, TMP        /* closure */
        LEA_VAR(caml_apply2, TMP)   /* code pointer */
        brcl    15, LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback2_asm))

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2, r3, r4 and r14 before C call */
        lay     %r15, -(RESERVED_STACK+32)(%r15)
        CFI_ADJUST(RESERVED_STACK+32)
        stg     C_ARG_1, (RESERVED_STACK+0)(%r15)
        stg     C_ARG_2, (RESERVED_STACK+8)(%r15)
        stg     C_ARG_3, (RESERVED_STACK+16)(%r15)
        stg     %r14, (RESERVED_STACK+24)(%r15)
        lgr     C_ARG_1, %r14
        brasl   %r14, GCALL(__tsan_func_entry)
        lg      %r14, (RESERVED_STACK+24)(%r15)
        lg      C_ARG_3, (RESERVED_STACK+16)(%r15)
        lg      C_ARG_2, (RESERVED_STACK+8)(%r15)
        lg      C_ARG_1, (RESERVED_STACK+0)(%r15)
        la      %r15, (RESERVED_STACK+32)(%r15)
        CFI_ADJUST(-(RESERVED_STACK+32))
#endif
    /* Initial shuffling of arguments */
    /* (%r2 = Caml_state, %r3 = closure, 0(%r4) = arg1, 8(%r4) = arg2,
        16(%r4) = arg3) */
        lgr     TMP3,       C_ARG_1     /* Caml_state */
        lgr     C_ARG_4,    C_ARG_2     /* closure */
        lg      C_ARG_1,  0(C_ARG_3)    /* first argument */
        lg      C_ARG_2,  8(C_ARG_3)    /* second argument */
        lg      C_ARG_3, 16(C_ARG_3)    /* third argument */
        LEA_VAR(caml_apply3, TMP)       /* code pointer */
        brcl    15, LBL(caml_start_program)
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
    /*  %r2: effect to perform
        %r3: freshly allocated continuation */
        lg      %r4, Caml_state(current_stack) /* %r4 := old stack */
        lay     %r5, 1(%r4) /* %r5 (last_fiber) := Val_ptr(old stack) */
        stg     %r5, 0(%r3) /* Initialise continuation */
LBL(do_perform):
    /*  %r2: effect to perform
        %r3: continuation
        %r4: old_stack
        %r5: last_fiber */
#if defined(WITH_THREAD_SANITIZER)
    /* Signal to TSan all stack frames exited by the perform. */
        TSAN_SAVE_CALLER_REGS
        lgr     C_ARG_1, %r14   /* arg1: pc of perform */
        lgr     C_ARG_2, %r15   /* arg2: sp of perform */
        TSAN_C_CALL(caml_tsan_exit_on_perform)
        TSAN_RESTORE_CALLER_REGS
#endif
        stg     %r5, 8(%r3) /* Set the last fiber field in the continuation */
        lg      %r9, Stack_handler(%r4)  /* %r9 := old stack -> handler */
        lg      %r8, Handler_parent(%r9)
        clgfi   %r8, 0   /* %r8 := parent stack; is parent NULL? */
        je      LBL(112)
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2-r5. r8 and r9 are callee-saved
       and do not need to be preserved here. */
        lay     %r15, -32(%r15)
        CFI_ADJUST(32)
        stg     C_ARG_1, 0(%r15)
        stg     C_ARG_2, 8(%r15)
        stg     C_ARG_3, 16(%r15)
        stg     C_ARG_4, 24(%r15)
    /* Match the TSan-enter made from caml_runstack */
        TSAN_EXIT_FUNCTION
        lg      C_ARG_4, 24(%r15)
        lg      C_ARG_3, 16(%r15)
        lg      C_ARG_2, 8(%r15)
        lg      C_ARG_1, 0(%r15)
        la      %r15, 32(%r15)
        CFI_ADJUST(-32)
#endif
        SWITCH_OCAML_STACKS(%r4, %r8)
    /* We have to null the Handler_parent after the switch because the
       Handler_parent is needed to unwind the stack for backtraces */
        lgfi    %r0, 0
        stg     %r0, Handler_parent(%r9) /* Set parent of performer to NULL */
        lgr     %r4, %r5                 /* %r4 = last_fiber */
        lg      %r5, Handler_effect(%r9) /* %r5 := effect handler */
        brcl    15, GCALL(caml_apply3)
LBL(112):
    /* Switch back to original performer before raising Unhandled
       (no-op unless this is a reperform) */
        lg      %r8, 0(%r3)         /* load performer stack from continuation */
        lay     %r8, -1(%r8)        /* r8 := Ptr_val(r8) */
        lg      %r9, Caml_state(current_stack)
        SWITCH_OCAML_STACKS(%r9, %r8)
#if defined(WITH_THREAD_SANITIZER)
    /* We must let the TSan runtime know that we switched back to the
       original performer stack. For that, we perform the necessary calls
       to __tsan_func_entry via caml_tsan_entry_on_resume.
       Note that, from TSan's point of view, we just exited all stack frames,
       including those of the main fiber. This is ok, because we will re-enter
       them immediately via caml_tsan_entry_on_resume below. */
        TSAN_SAVE_CALLER_REGS
        lgr     C_ARG_1, %r14   /* arg1: pc of perform */
        lgr     C_ARG_2, %r15   /* arg2: sp of perform */
        lgr     C_ARG_3, %r8    /* arg3: performer stack */
        TSAN_C_CALL(caml_tsan_entry_on_resume)
        TSAN_RESTORE_CALLER_REGS
#endif
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_raise_unhandled_effect, ADDITIONAL_ARG)
        brcl    15, GCALL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_perform))

FUNCTION(G(caml_reperform))
CFI_STARTPROC
    /*  %r2: effect to reperform
        %r3: continuation
        %r4: last_fiber */
        lg      TMP, (Stack_handler-1)(%r4)
        lg      %r4, Caml_state(current_stack)  /* %r4 := old stack */
        stg     %r4, Handler_parent(TMP)        /* Append to last_fiber */
        lay     %r5, 1(%r4)  /* %r5 (last_fiber) := Val_ptr(old stack) */
        brcl    15, LBL(do_perform)
CFI_ENDPROC
ENDFUNCTION(G(caml_reperform))

FUNCTION(G(caml_resume))
CFI_STARTPROC
    /* %r2 -> new fiber, %r3 -> fun, %r4 -> arg, %r5 -> last_fiber */
        lay     %r2, -1(%r2)  /* %r2 (new stack) = Ptr_val(%r2) */
        lg      %r6,  0(%r3)  /* code pointer */
    /*  check if stack null, then already used */
        cgfi    %r2, 0
        jz      LBL(caml_resume_1)
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2-r5 */
        lay     %r15, -32(%r15)
        CFI_ADJUST(32)
        stg     C_ARG_1, 0(%r15)
        stg     C_ARG_2, 8(%r15)
        stg     C_ARG_3, 16(%r15)
        stg     C_ARG_4, 24(%r15)
    /* Necessary to include the caller of caml_resume in the TSan backtrace */
        TSAN_ENTER_FUNCTION
        lg      C_ARG_4, 24(%r15)
        lg      C_ARG_3, 16(%r15)
        lg      C_ARG_2, 8(%r15)
        lg      C_ARG_1, 0(%r15)
        la      %r15, 32(%r15)
        CFI_ADJUST(-32)
        TSAN_SAVE_CALLER_REGS
    /* Signal to TSan all stack frames exited by the perform. */
        lgr     C_ARG_3, %r2            /* arg3: fiber */
        lg      C_ARG_2, Stack_sp(%r2)
        lg      C_ARG_1, 0(C_ARG_2)     /* arg1: pc of perform */
        /* This stack address adjustment is required to compensate the
           saving of r14 in SWITCH_OCAML_STACKS, which causes Stack_sp()
           to be 8 bytes lower than expected. */
        la      C_ARG_2, 8(C_ARG_2)     /* arg2: sp of raise */
        TSAN_C_CALL(caml_tsan_entry_on_resume)
        TSAN_RESTORE_CALLER_REGS
    /* Reload lost value of %r6 */
        lg      %r6,  0(%r3)  /* code pointer */
#endif
    /* Add current stack to the end */
        lg      %r8, (Stack_handler-1)(%r5)
        lg      %r9, Caml_state(current_stack)
        stg     %r9, Handler_parent(%r8)
     /* Need to update the oldest saved frame pointer here as the current fiber
        stack may have been reallocated or we may be resuming a computation
        that was not originally run here. */
        SWITCH_OCAML_STACKS(%r9, %r2)
        lgr     %r2, %r4
        br      %r6
LBL(caml_resume_1):
        LEA_VAR(caml_raise_continuation_already_resumed, ADDITIONAL_ARG)
        brcl    15, GCALL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_resume))

/* Run a function on a new stack,
   then invoke either the value or exception handler */
FUNCTION(G(caml_runstack))
CFI_STARTPROC
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r2-r4 */
        lay     %r15, -24(%r15)
        CFI_ADJUST(24)
        stg     C_ARG_1, 0(%r15)
        stg     C_ARG_2, 8(%r15)
        stg     C_ARG_3, 16(%r15)
    /* Necessary to include the caller of caml_runstack in the TSan backtrace */
        TSAN_ENTER_FUNCTION
        lg      C_ARG_3, 16(%r15)
        lg      C_ARG_2, 8(%r15)
        lg      C_ARG_1, 0(%r15)
        la      %r15, 24(%r15)
        CFI_ADJUST(-24)
#endif
        CFI_SIGNAL_FRAME
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
    /* %r2 -> fiber, %r3 -> fun, %r4 -> arg */
        lay     %r2, -1(%r2)  /* %r2 (new stack) = Ptr_val(%r2) */
        lg      %r5,  0(%r3)  /* code pointer */
    /* save old stack pointer and exception handler */
        lg      %r8, Caml_state(current_stack) /* %r8 = old stack */
        stg     %r15, Stack_sp(%r8)
        stg     TRAP_PTR, Stack_exception(%r8)
    /* Load new stack pointer and set parent */
        lg      TMP, Stack_handler(%r2)
        stg     %r8, Handler_parent(TMP)
        stg     %r2, Caml_state(current_stack)
        lg      %r9, Stack_sp(%r2) /* %r9 = sp of new stack */
    /* Create an exception handler on the target stack
       after 16byte DWARF & gc_regs block (which is unused here) */
        lay     %r9, -32(%r9)
        larl    TMP, LBL(fiber_exn_handler)
        stg     TMP, 8(%r9)
    /* link the previous exn_handler so that copying stacks works */
        lg      TMP, Stack_exception(%r2)
        stg     TMP, 0(%r9)
        lgr     TRAP_PTR, %r9
    /* Switch to the new stack */
        lgr     %r15, %r9
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        .cfi_escape DW_CFA_def_cfa_expression, 3+3+2, \
          DW_OP_breg + DW_REG_r15,                    \
          caml_runstack_sleb128_1byte,                \
          DW_OP_deref,                                \
          DW_OP_plus_uconst, Stack_sp, DW_OP_deref,   \
          DW_OP_plus_uconst, RETADDR_ENTRY_SIZE
#endif
    /* Call the function on the new stack */
        lgr     %r2, %r4 /* first argument */
        basr    %r14, %r5 /* closure in %r3 (second argument) */
LBL(frame_runstack):
        lay     %r8, 32(%r15) /*%r8 = stack_handler */
        lg      %r7, Handler_value(%r8) /* saved across C call */
LBL(caml_runstack_1):
        lgr     %r12, %r2 /* save return across C call */
        lg      %r2, Caml_state(current_stack)
    /* restore parent stack and exn_handler into Caml_state */
        lg      TMP, Handler_parent(%r8)
        stg     TMP, Caml_state(current_stack)
        lg      TRAP_PTR, Stack_exception(TMP)
        stg     TRAP_PTR, Caml_state(exn_handler)
    /* free old stack by switching directly to c_stack; is a no-alloc call */
        lg      %r9, Stack_sp(TMP) /* saved across C call */
        CFI_RESTORE_STATE
        CFI_REMEMBER_STATE
        CFI_DEF_CFA_REGISTER(DW_REG_r9)
        lg      %r15, Caml_state(c_stack)
        PREPARE_FOR_C_CALL
        CFI_ADJUST(RESERVED_STACK)
        brasl %r14, GCALL(caml_free_stack)
        CLEANUP_AFTER_C_CALL
    /* switch directly to parent stack */
        lgr     %r15, %r9
        CFI_RESTORE_STATE
#if defined(WITH_THREAD_SANITIZER)
    /* Signal to TSan that we exit caml_runstack. Theoretically, no registers
       need to be saved here, but TSAN_EXIT_FUNCTION uses SWITCH_OCAML_TO_C
       which clobbers %r12 (TMP2), so we need to preserve it here. */
        lay     %r15, -8(%r15)
        CFI_ADJUST(8)
        stg     %r12, 0(%r15)
        TSAN_EXIT_FUNCTION
        lg      %r12, 0(%r15)
        la      %r15, 8(%r15)
        CFI_ADJUST(-8)
#endif
        lgr     %r2,  %r12
        lgr     %r3,  %r7
        lg      TMP, 0(%r3) /* code pointer */
    /* Invoke handle_value (or handle_exn) */
        LEAVE_FUNCTION
        br      TMP
LBL(fiber_exn_handler):
        lay     %r8, 16(%r15)
        lg      %r7, Handler_exception(%r8)
        brcl    15, LBL(caml_runstack_1)
CFI_ENDPROC
ENDFUNCTION(G(caml_runstack))

FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        ENTER_FUNCTION
        CFI_OFFSET(14, -168)
    /* No registers require saving before C call to TSan */
        TSAN_ENTER_FUNCTION
        LEA_VAR(caml_array_bound_error_asm, ADDITIONAL_ARG)
    /* Note the following jumps in the middle of caml_c_call, since stack
       has already been adjusted. */
        brcl    15, LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_bound_error))

        TEXT_SECTION(caml_system__code_end)
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
OBJECT(caml_system.frametable)
        .quad   2               /* two descriptors */
        .quad   LBL(caml_retaddr)  /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
        .align  8
        .quad   LBL(frame_runstack) /* return address into fiber_val_handler */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
        .align  8
ENDOBJECT(caml_system.frametable)

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
