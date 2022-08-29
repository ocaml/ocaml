/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/m.h"

        .abiversion 2

#if _CALL_ELF != 2
#error "This POWER port requires the ELFv2 ABI"
#endif

/* Special registers */
#define SP 1
#define TMP 11
#define TMP2 12
#define START_PRG_ARG 12
#define START_PRG_DOMAIN_STATE_PTR 7
#define STACK_ARG_BYTES 24
#define C_CALL_FUN 25
#define C_CALL_TOC 26
#define C_CALL_RET_ADDR 27
#define C_CALL_TMP 28
#define TRAP_PTR 29
#define DOMAIN_STATE_PTR 30
#define ALLOC_PTR 31

/* Stack layout */
#define RESERVED_STACK 32
#define LR_SAVE 16
#define TOC_SAVE_PARENT 8
#define TOC_SAVE 24
#define TRAP_SIZE 16
#define TRAP_HANDLER_OFFSET 8
#define TRAP_PREVIOUS_OFFSET 0
#define CALLBACK_LINK_SIZE 32

/* struct stack_info */
#define Stack_sp(reg)           0(reg)
#define Stack_sp_offset         0
#define Stack_exception(reg)    8(reg)
#define Stack_handler(reg)      16(reg)

/* struct c_stack_link */
#define Cstack_stack(reg)       32(reg)
#define Cstack_sp(reg)          40(reg)
#define Cstack_sp_offset        40
#define Cstack_prev(reg)        48(reg)

/* struct stack_handler */
#define Handler_value(reg)      0(reg)
#define Handler_exception(reg)  8(reg)
#define Handler_effect(reg)     16(reg)
#define Handler_parent(reg)     24(reg)
#define Handler_parent_offset   24

/* Function definitions */

.macro TEXT_SECTION name
#if defined(FUNCTION_SECTIONS)
        .section ".text.caml.\name","ax",@progbits
#else
        .section ".text"
#endif
.endm

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION caml_hot.code_begin
        .globl caml_hot.code_begin
caml_hot.code_begin:

        TEXT_SECTION caml_hot.code_end
        .globl caml_hot.code_end
caml_hot.code_end:
#endif

.macro FUNCTION name
        TEXT_SECTION \name
        .globl \name
        .type \name, @function
\name:
0:      addis   2, 12, .TOC.- 0b@ha
        addi    2, 2,  .TOC.- 0b@l
        .localentry \name, . - \name
.endm

.macro ENDFUNCTION name
       .size \name, . - \name
.endm

/* Function prologue and epilogue */

.macro ENTER_FUNCTION
   /* Save return address in caller's frame. */
        mflr    0
        std     0, LR_SAVE(SP)
.endm

.macro LEAVE_FUNCTION
   /* Restore return address. */
        ld      0, LR_SAVE(SP)
        mtlr    0
.endm

/* Accessing global variables.  */

#define LSYMB(glob) .L##glob

#define Addrglobal(reg,glob) \
        ld      reg, LSYMB(glob)@toc(2)

/* Accessing local code labels.  */

#define LLABEL(lbl) .LL##lbl

#define Addrlabel(reg,lbl) \
        ld      reg, LLABEL(lbl)@toc(2)

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) 8*domain_field_caml_##var(DOMAIN_STATE_PTR)

/* Call or tail-call function F defined in another compilation unit */

#define Far_call(fn) bl fn; nop

#define Far_tailcall(fn) Addrglobal(12, fn); mtctr 12; bctr

/* Switch from OCaml stack to C stack */

.macro SWITCH_OCAML_TO_C
   /* Fill in Caml_state->current_stack->sp */
       ld      TMP, Caml_state(current_stack)
       std     SP, Stack_sp(TMP)
   /* Fill in Caml_state->c_stack */
       ld      TMP2, Caml_state(c_stack)
       std     TMP, Cstack_stack(TMP2)
       std     SP, Cstack_sp(TMP2)
   /* Switch to C stack */
       mr      SP, TMP2
.endm

/* Switch from C stack to OCaml stack */

.macro SWITCH_C_TO_OCAML
       ld      SP, Cstack_sp(SP)
.endm

/* Save ALLOC_PTR and TRAP_PTR to domain state, and save
   the registers used by the code generator to a free gc_regs bucket.
   Address of bucket is then written to Caml_state(gc_regs) */

.macro SAVE_ALL_REGS
    /* Save allocation pointer and exception pointer */
       std      ALLOC_PTR, Caml_state(young_ptr)
       std      TRAP_PTR, Caml_state(exn_handler)
    /* Point TMP to the gc_regs bucket and skip to next bucket */
       ld       TMP, Caml_state(gc_regs_buckets)
       ld       0, 0(TMP)
       std      0, Caml_state(gc_regs_buckets)
    /* Save all allocatable integer registers */
       std      3,  0x000(TMP)
       std      4,  0x008(TMP)
       std      5,  0x010(TMP)
       std      6,  0x018(TMP)
       std      7,  0x020(TMP)
       std      8,  0x028(TMP)
       std      9,  0x030(TMP)
       std      10, 0x038(TMP)
       std      14, 0x040(TMP)
       std      15, 0x048(TMP)
       std      16, 0x050(TMP)
       std      17, 0x058(TMP)
       std      18, 0x060(TMP)
       std      19, 0x068(TMP)
       std      20, 0x070(TMP)
       std      21, 0x078(TMP)
       std      22, 0x080(TMP)
       std      23, 0x088(TMP)
       std      24, 0x090(TMP)
       std      25, 0x098(TMP)
       std      26, 0x0A0(TMP)
       std      27, 0x0A8(TMP)
       std      28, 0x0B0(TMP)
    /* Save caller-save floating-point registers */
    /* (callee-saves are preserved by C functions) */
       stfd     0,  0x0B8(TMP)
       stfd     1,  0x0C0(TMP)
       stfd     2,  0x0C8(TMP)
       stfd     3,  0x0D0(TMP)
       stfd     4,  0x0D8(TMP)
       stfd     5,  0x0E0(TMP)
       stfd     6,  0x0E8(TMP)
       stfd     7,  0x0F0(TMP)
       stfd     8,  0x0F8(TMP)
       stfd     9,  0x100(TMP)
       stfd     10, 0x108(TMP)
       stfd     11, 0x110(TMP)
       stfd     12, 0x118(TMP)
       stfd     13, 0x120(TMP)
    /* Save bucket to gc_regs */
       std      TMP, Caml_state(gc_regs)
.endm

/* Undo SAVE_ALL_REGS: load all the registers saved to Caml_state(gc_regs)
   and refresh ALLOC_PTR and TRAP_PTR from Caml_state */
.macro RESTORE_ALL_REGS
       ld       TMP, Caml_state(gc_regs)
    /* Restore all allocatable integer registers */
       ld       3,  0x000(TMP)
       ld       4,  0x008(TMP)
       ld       5,  0x010(TMP)
       ld       6,  0x018(TMP)
       ld       7,  0x020(TMP)
       ld       8,  0x028(TMP)
       ld       9,  0x030(TMP)
       ld       10, 0x038(TMP)
       ld       14, 0x040(TMP)
       ld       15, 0x048(TMP)
       ld       16, 0x050(TMP)
       ld       17, 0x058(TMP)
       ld       18, 0x060(TMP)
       ld       19, 0x068(TMP)
       ld       20, 0x070(TMP)
       ld       21, 0x078(TMP)
       ld       22, 0x080(TMP)
       ld       23, 0x088(TMP)
       ld       24, 0x090(TMP)
       ld       25, 0x098(TMP)
       ld       26, 0x0A0(TMP)
       ld       27, 0x0A8(TMP)
       ld       28, 0x0B0(TMP)
    /* Restore caller-save floating-point registers
       (callee-saves are preserved by C functions) */
       lfd      0,  0x0B8(TMP)
       lfd      1,  0x0C0(TMP)
       lfd      2,  0x0C8(TMP)
       lfd      3,  0x0D0(TMP)
       lfd      4,  0x0D8(TMP)
       lfd      5,  0x0E0(TMP)
       lfd      6,  0x0E8(TMP)
       lfd      7,  0x0F0(TMP)
       lfd      8,  0x0F8(TMP)
       lfd      9,  0x100(TMP)
       lfd      10, 0x108(TMP)
       lfd      11, 0x110(TMP)
       lfd      12, 0x118(TMP)
       lfd      13, 0x120(TMP)
    /* Put gc_regs struct back in bucket linked list */
       ld       TMP2, Caml_state(gc_regs_buckets)
       std      TMP2, 0(TMP)  /* next ptr */
       std      TMP, Caml_state(gc_regs_buckets)
    /* Reload new allocation pointer and exception pointer */
       ld       ALLOC_PTR, Caml_state(young_ptr)
       ld       TRAP_PTR, Caml_state(exn_handler)
.endm

#if defined(WITH_THREAD_SANITIZER) /* { */

.macro TSAN_SETUP_C_CALL size
        ENTER_FUNCTION
   /* Setup new frame for a function call and possibly some register saves. */
        addi    SP, SP, -(RESERVED_STACK + \size)
        std     2, TOC_SAVE(SP)
.endm

.macro TSAN_CLEANUP_AFTER_C_CALL size
   /* Undo call frame. */
        addi    SP, SP, (RESERVED_STACK + \size)
        LEAVE_FUNCTION
.endm

.macro TSAN_ENTER_FUNCTION
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 0
        mr      3, 0            /* arg1: return address in caller */
        Far_call(__tsan_func_entry)
        TSAN_CLEANUP_AFTER_C_CALL 0
        SWITCH_C_TO_OCAML
.endm

/* This is similar to SAVE_ALL_REGS, but only saving the caller-saved
   registers. */
.macro TSAN_SAVE_CALLER_REGS
    /* Save allocation pointer and exception pointer */
       std      ALLOC_PTR, Caml_state(young_ptr)
       std      TRAP_PTR, Caml_state(exn_handler)
    /* Point TMP to the gc_regs bucket and skip to next bucket */
       ld       TMP, Caml_state(gc_regs_buckets)
       ld       0, 0(TMP)
       std      0, Caml_state(gc_regs_buckets)
    /* Save caller-save integer registers */
       std      3,  0x000(TMP)
       std      4,  0x008(TMP)
       std      5,  0x010(TMP)
       std      6,  0x018(TMP)
       std      7,  0x020(TMP)
       std      8,  0x028(TMP)
       std      9,  0x030(TMP)
       std      10, 0x038(TMP)
    /* Save caller-save floating-point registers */
       stfd     0,  0x0B8(TMP)
       stfd     1,  0x0C0(TMP)
       stfd     2,  0x0C8(TMP)
       stfd     3,  0x0D0(TMP)
       stfd     4,  0x0D8(TMP)
       stfd     5,  0x0E0(TMP)
       stfd     6,  0x0E8(TMP)
       stfd     7,  0x0F0(TMP)
       stfd     8,  0x0F8(TMP)
       stfd     9,  0x100(TMP)
       stfd     10, 0x108(TMP)
       stfd     11, 0x110(TMP)
       stfd     12, 0x118(TMP)
       stfd     13, 0x120(TMP)
    /* Save bucket to gc_regs */
       std      TMP, Caml_state(gc_regs)
.endm

/* This is similar to RESTORE_ALL_REGS, but only restoring the caller-saved
   registers. */
.macro TSAN_RESTORE_CALLER_REGS
       ld       TMP, Caml_state(gc_regs)
    /* Restore caller-save integer registers */
       ld       3,  0x000(TMP)
       ld       4,  0x008(TMP)
       ld       5,  0x010(TMP)
       ld       6,  0x018(TMP)
       ld       7,  0x020(TMP)
       ld       8,  0x028(TMP)
       ld       9,  0x030(TMP)
       ld       10, 0x038(TMP)
    /* Restore caller-save floating-point registers */
       lfd      0,  0x0B8(TMP)
       lfd      1,  0x0C0(TMP)
       lfd      2,  0x0C8(TMP)
       lfd      3,  0x0D0(TMP)
       lfd      4,  0x0D8(TMP)
       lfd      5,  0x0E0(TMP)
       lfd      6,  0x0E8(TMP)
       lfd      7,  0x0F0(TMP)
       lfd      8,  0x0F8(TMP)
       lfd      9,  0x100(TMP)
       lfd      10, 0x108(TMP)
       lfd      11, 0x110(TMP)
       lfd      12, 0x118(TMP)
       lfd      13, 0x120(TMP)
    /* Put gc_regs struct back in bucket linked list */
       ld       TMP2, Caml_state(gc_regs_buckets)
       std      TMP2, 0(TMP)  /* next ptr */
       std      TMP, Caml_state(gc_regs_buckets)
    /* Reload new allocation pointer and exception pointer */
       ld       ALLOC_PTR, Caml_state(young_ptr)
       ld       TRAP_PTR, Caml_state(exn_handler)
.endm

#else /* } { */

.macro TSAN_ENTER_FUNCTION
.endm

.macro TSAN_SAVE_CALLER_REGS
.endm

.macro TSAN_RESTORE_CALLER_REGS
.endm

#endif /* } WITH_THREAD_SANITIZER */

        TEXT_SECTION caml_system__code_begin
        .globl  caml_system__code_begin
caml_system__code_begin:

/* Reallocate the stack when it is too small. */
/* Desired size is passed in register r27. */

FUNCTION caml_call_realloc_stack
        ENTER_FUNCTION
   /* Save all registers, as well as ALLOC_PTR and TRAP_PTR */
        SAVE_ALL_REGS  /* r27 is preserved */
   /* Recover desired size, to be passed in r3 */
        mr      3, 27
   /* Switch stacks and call caml_try_realloc_stack */
        SWITCH_OCAML_TO_C
        Far_call(caml_try_realloc_stack)
        SWITCH_C_TO_OCAML
        cmpdi   3, 0
   /* Restore all registers, and also return address */
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
   /* Check status */
        beq     1f
   /* Reallocation successful: return to caller */
        blr
   /* Reallocation failed: raise the Stack_overflow exception */
1:      Addrglobal(3, caml_exn_Stack_overflow)
        b       .Lcaml_raise_exn
ENDFUNCTION caml_call_realloc_stack

/* Invoke the garbage collector. */

FUNCTION caml_call_gc
        ENTER_FUNCTION
   /* Save all registers, as well as ALLOC_PTR and TRAP_PTR */
        SAVE_ALL_REGS
        TSAN_ENTER_FUNCTION
   /* Switch stacks and call caml_garbage_collection */
        SWITCH_OCAML_TO_C
        Far_call(caml_garbage_collection)
#if defined(WITH_THREAD_SANITIZER)
        TSAN_SETUP_C_CALL 0
        li      3, 0
        Far_call(__tsan_func_exit)
        TSAN_CLEANUP_AFTER_C_CALL 0
#endif
        SWITCH_C_TO_OCAML
   /* Restore registers and return to caller */
        RESTORE_ALL_REGS
        LEAVE_FUNCTION
        ld      2, TOC_SAVE(SP)
        blr
ENDFUNCTION caml_call_gc

/* Call a C function from OCaml.  Function to call is in C_CALL_FUN */

.macro RET_FROM_C_CALL
        ld      TMP, Caml_state(action_pending)
        cmpdi   TMP, 0
        beqlr+  0
        li      TMP, -1
        std     TMP, Caml_state(young_limit)
        blr
.endm

FUNCTION caml_c_call
        TSAN_SAVE_CALLER_REGS
        TSAN_ENTER_FUNCTION
        TSAN_RESTORE_CALLER_REGS
.Lcaml_c_call:
   /* Save return address in caller's frame AND in a callee-save register */
        mflr    C_CALL_RET_ADDR
        std     C_CALL_RET_ADDR, LR_SAVE(SP)
   /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
   /* Make the exception handler and alloc ptr available to the C code */
        std     ALLOC_PTR, Caml_state(young_ptr)
        std     TRAP_PTR, Caml_state(exn_handler)
    /* Call C function (address in C_CALL_FUN) */
        mtctr   C_CALL_FUN
        mr      12, C_CALL_FUN
        mr      C_CALL_TOC, 2   /* save current TOC in a callee-save register */
        bctrl
        mr      2, C_CALL_TOC   /* restore current TOC */
    /* Restore return address (in register C_CALL_RET_ADDR, preserved by C) */
        mtlr    C_CALL_RET_ADDR
    /* Reload new allocation pointer and exception pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
        ld      TRAP_PTR, Caml_state(exn_handler)
#if defined(WITH_THREAD_SANITIZER)
        TSAN_SETUP_C_CALL 16
    /* Save return value registers. Since the called function could be anything,
       it may have returned its result (if any) either in r3 or f1. */
        std     3,  (RESERVED_STACK + 0)(SP)
        stfd    1,  (RESERVED_STACK + 8)(SP)
        li      3, 0
        Far_call(__tsan_func_exit)
        lfd     1,  (RESERVED_STACK + 8)(SP)
        ld      3,  (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 16
#endif
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return to caller */
        RET_FROM_C_CALL
ENDFUNCTION caml_c_call

FUNCTION caml_c_call_stack_args
   /* Extra arguments to be passed on stack:
      STACK_ARG_BYTES at offsets 32 to 32 + STACK_ARG_BYTES from SP */
        mr      C_CALL_TMP, SP
   /* Save return address in caller's frame AND in a callee-save register */
        mflr    C_CALL_RET_ADDR
        std     C_CALL_RET_ADDR, LR_SAVE(SP)
   /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
   /* Make the exception handler and alloc ptr available to the C code */
        std     ALLOC_PTR, Caml_state(young_ptr)
        std     TRAP_PTR, Caml_state(exn_handler)
   /* Reserve STACK_ARG_BYTES bytes on the C stack */
        subfc   SP, STACK_ARG_BYTES, SP
   /* Copy from OCaml SP + [32...32+STACK_ARG_BYTES)
           to C SP + [32...32+STACK_ARG_BYTES) */
        addi    TMP, STACK_ARG_BYTES, 32 - 8
1:      ldx     0, C_CALL_TMP, TMP
        stdx    0, SP, TMP
        addi    TMP, TMP, -8
        cmpdi   TMP, 32
        bge     1b
    /* Call C function (address in C_CALL_FUN) */
        mtctr   C_CALL_FUN
        mr      12, C_CALL_FUN
        mr      C_CALL_TOC, 2   /* save current TOC in a callee-save register */
        bctrl
        mr      2, C_CALL_TOC   /* restore current TOC */
    /* Pop the extra stack space used */
        add     SP, SP, STACK_ARG_BYTES
    /* Restore return address (in register C_CALL_RET_ADDR, preserved by C) */
        mtlr    C_CALL_RET_ADDR
    /* Reload new allocation pointer and exception pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
        ld      TRAP_PTR, Caml_state(exn_handler)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return to caller */
        RET_FROM_C_CALL
ENDFUNCTION caml_c_call_stack_args

/* Raise an exception from OCaml */

FUNCTION caml_raise_exn
.Lcaml_raise_exn:
        ld      0, Caml_state(backtrace_active)
        cmpdi   0, 0
        bne     .L111
.L110:
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(TRAP_PTR)
        addi    SP, TRAP_PTR, TRAP_SIZE-RESERVED_STACK
        mtctr   0
        ld      TRAP_PTR, (RESERVED_STACK-TRAP_SIZE+TRAP_PREVIOUS_OFFSET)(SP)
    /* Branch to handler */
        bctr
.L111:
    /* Zero backtrace_pos */
        li      0, 0
        std     0, Caml_state(backtrace_pos)
.L112:
        mr      27, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        mflr    4               /* arg2: PC of raise */
        mr      5, SP           /* arg3: SP of raise */
        mr      6, TRAP_PTR     /* arg4: SP of handler */
    /* Switch to C stack and call caml_stash_backtrace */
        ld      SP, Caml_state(c_stack)
        Far_call(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mr      3, 27
        b       .L110
ENDFUNCTION caml_raise_exn

FUNCTION caml_reraise_exn
        ld      0, Caml_state(backtrace_active)
        cmpdi   0, 0
        bne     .L112
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(TRAP_PTR)
        addi    SP, TRAP_PTR, TRAP_SIZE-RESERVED_STACK
        mtctr   0
        ld      TRAP_PTR, (RESERVED_STACK-TRAP_SIZE+TRAP_PREVIOUS_OFFSET)(SP)
    /* Branch to handler */
        bctr
ENDFUNCTION caml_reraise_exn

#if defined(WITH_THREAD_SANITIZER)
/* When TSan support is enabled, this routine should be called just before
   raising an exception. It calls __tsan_func_exit for every OCaml frame about
   to be exited due to the exception.
   Takes no arguments, clobbers r3, r4, r5 and potentially all
   caller-saved registers of the C calling convention. */
FUNCTION caml_tsan_exit_on_raise_asm
        mflr    3               /* arg1: PC of raise */
        mr      4, SP           /* arg2: SP of raise */
        mr      5, TRAP_PTR     /* arg3: SP of handler */
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 0
        Far_call(caml_tsan_exit_on_raise)
        TSAN_CLEANUP_AFTER_C_CALL 0
        SWITCH_C_TO_OCAML
        blr
ENDFUNCTION caml_tsan_exit_on_raise_asm
#endif

/* Raise an exception from C */

FUNCTION caml_raise_exception
    /* Reinitialize domain state pointer */
        mr      DOMAIN_STATE_PTR, 3
    /* Move exn bucket where caml_raise_exn expects it */
        mr      3, 4
    /* Reload allocation pointer and exception pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
        ld      TRAP_PTR, Caml_state(exn_handler)
    /* Discard the C stack pointer and switch to OCaml stack */
        ld      TMP, Caml_state(current_stack)
        ld      SP, Stack_sp(TMP)
    /* Reload return address from caller's frame (for the backtrace) */
        LEAVE_FUNCTION
#if defined(WITH_THREAD_SANITIZER)
    /* Call __tsan_func_exit for every OCaml stack frame exited due to the
       exception */
        mr      4, SP           /* arg2: SP of raise */
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 16
        std     3, (RESERVED_STACK + 0)(SP) /* preserve exception bucket */
        mr      3, 0            /* arg1: PC of raise */
        mr      5, TRAP_PTR     /* arg3: SP of handler */
        Far_call(caml_tsan_exit_on_raise)
        ld      3,  (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 16
        SWITCH_C_TO_OCAML
#endif
    /* Raise the exception */
        b       .Lcaml_raise_exn
ENDFUNCTION caml_raise_exception

/* Start the OCaml program */

FUNCTION caml_start_program
#if defined(WITH_THREAD_SANITIZER)
  /* We can't use the TSAN_ENTER_FUNCTION macro, as it assumes to run on an
     OCaml stack, and we are still on a C stack at this point. Moreover, we
     need to save r3 on the stack. */
        TSAN_SETUP_C_CALL 16
        std     3, (RESERVED_STACK + 0)(SP)
        mr      3, 0            /* arg1: return address in caller */
        Far_call(__tsan_func_entry)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 16
#endif
  /* Domain state pointer is the first arg to caml_start_program. Move it */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
  /* Code to call is caml_program */
        Addrglobal(START_PRG_ARG, caml_program)

/* Code shared between caml_start_program and caml_callback */
/* Domain state pointer is in START_PRG_DOMAIN_STATE_PTR */
/* Code to call is in START_PRG_ARG */
.L102:
    /* Stack frame contains:
       - 18 callee-save FPRs
       - 18 callee-save GPRs
       - a struct c_stack_link
       - the standard reserved space */
#define STACKSIZE (18 * 8 + 18 * 8 + CALLBACK_LINK_SIZE + RESERVED_STACK)
    /* Allocate and link stack frame */
        stdu    SP, -STACKSIZE(SP)
    /* Save return address and TOC pointer */
        mflr    0
        std     0, (STACKSIZE + LR_SAVE)(SP)
        std     2, (STACKSIZE + TOC_SAVE_PARENT)(SP)
    /* Save all callee-save registers */
        addi    TMP, SP, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        stdu    14, 8(TMP)
        stdu    15, 8(TMP)
        stdu    16, 8(TMP)
        stdu    17, 8(TMP)
        stdu    18, 8(TMP)
        stdu    19, 8(TMP)
        stdu    20, 8(TMP)
        stdu    21, 8(TMP)
        stdu    22, 8(TMP)
        stdu    23, 8(TMP)
        stdu    24, 8(TMP)
        stdu    25, 8(TMP)
        stdu    26, 8(TMP)
        stdu    27, 8(TMP)
        stdu    28, 8(TMP)
        stdu    29, 8(TMP)
        stdu    30, 8(TMP)
        stdu    31, 8(TMP)
        stfdu   14, 8(TMP)
        stfdu   15, 8(TMP)
        stfdu   16, 8(TMP)
        stfdu   17, 8(TMP)
        stfdu   18, 8(TMP)
        stfdu   19, 8(TMP)
        stfdu   20, 8(TMP)
        stfdu   21, 8(TMP)
        stfdu   22, 8(TMP)
        stfdu   23, 8(TMP)
        stfdu   24, 8(TMP)
        stfdu   25, 8(TMP)
        stfdu   26, 8(TMP)
        stfdu   27, 8(TMP)
        stfdu   28, 8(TMP)
        stfdu   29, 8(TMP)
        stfdu   30, 8(TMP)
        stfdu   31, 8(TMP)
    /* Load domain state pointer from argument */
        mr      DOMAIN_STATE_PTR, START_PRG_DOMAIN_STATE_PTR
    /* Reload allocation pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Set up a struct c_stack_link on the C stack */
        ld      0, Caml_state(c_stack)
        std     0, Cstack_prev(SP)
        std     SP, Caml_state(c_stack)
    /* Load the OCaml stack */
        ld      TMP, Caml_state(current_stack)
        ld      TMP, Stack_sp(TMP)
    /* Store the gc_regs for callbacks during a GC */
        ld      0, Caml_state(gc_regs)
        stdu    0, -8(TMP)
    /* Store the stack pointer to allow DWARF unwind (one day) */
        stdu    SP, -8(TMP)
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        addi    TMP, TMP, -TRAP_SIZE
        ld      0, Caml_state(exn_handler)
        std     0, TRAP_PREVIOUS_OFFSET(TMP)
        Addrlabel(0,trap_handler)
        std     0, TRAP_HANDLER_OFFSET(TMP)
        mr      TRAP_PTR, TMP
    /* Switch stacks, reserving 32 bytes at the bottom of the OCaml stack */
        addi    SP, TMP, -RESERVED_STACK
    /* Call the OCaml code (address in START_PRG_ARG) */
        mtctr   START_PRG_ARG
.Lcaml_retaddr:
        bctrl
    /* Pop the reserved 32 bytes */
        addi    SP, SP, RESERVED_STACK
    /* Pop the trap frame, restoring caml_exn_handler */
        ld      0, TRAP_PREVIOUS_OFFSET(SP)
        std     0, Caml_state(exn_handler)
        addi    SP, SP, TRAP_SIZE
    /* Update allocation pointer */
.L106:
        std     ALLOC_PTR, Caml_state(young_ptr)
    /* Restore GC regs */
        ld      0, 8(SP)
        std     0, Caml_state(gc_regs)
        addi    SP, SP, 16
    /* Return to C stack */
        ld      TMP, Caml_state(current_stack)
        std     SP, Stack_sp(TMP)
        ld      SP, Caml_state(c_stack)
    /* Pop the struct c_stack_link */
        ld      0, Cstack_prev(SP)
        std     0, Caml_state(c_stack)
    /* Restore callee-save registers */
        addi    TMP, SP, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        ldu     14, 8(TMP)
        ldu     15, 8(TMP)
        ldu     16, 8(TMP)
        ldu     17, 8(TMP)
        ldu     18, 8(TMP)
        ldu     19, 8(TMP)
        ldu     20, 8(TMP)
        ldu     21, 8(TMP)
        ldu     22, 8(TMP)
        ldu     23, 8(TMP)
        ldu     24, 8(TMP)
        ldu     25, 8(TMP)
        ldu     26, 8(TMP)
        ldu     27, 8(TMP)
        ldu     28, 8(TMP)
        ldu     29, 8(TMP)
        ldu     30, 8(TMP)
        ldu     31, 8(TMP)
        lfdu    14, 8(TMP)
        lfdu    15, 8(TMP)
        lfdu    16, 8(TMP)
        lfdu    17, 8(TMP)
        lfdu    18, 8(TMP)
        lfdu    19, 8(TMP)
        lfdu    20, 8(TMP)
        lfdu    21, 8(TMP)
        lfdu    22, 8(TMP)
        lfdu    23, 8(TMP)
        lfdu    24, 8(TMP)
        lfdu    25, 8(TMP)
        lfdu    26, 8(TMP)
        lfdu    27, 8(TMP)
        lfdu    28, 8(TMP)
        lfdu    29, 8(TMP)
        lfdu    30, 8(TMP)
        lfdu    31, 8(TMP)
    /* Reload return address and TOC pointer */
        ld      0, (STACKSIZE + LR_SAVE)(SP)
        ld      2, (STACKSIZE + TOC_SAVE_PARENT)(SP)
        mtlr    0
    /* Return */
        addi    SP, SP, STACKSIZE
#if defined(WITH_THREAD_SANITIZER)
        TSAN_SETUP_C_CALL 16
        std     3, (RESERVED_STACK + 0)(SP)
        li      3, 0
        Far_call(__tsan_func_exit)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 16
#endif
        blr

    /* The trap handler: */
.Ltrap_handler:
    /* Update caml_exn_handler */
        std     TRAP_PTR, Caml_state(exn_handler)
    /* Pop the reserved 32 bytes */
        addi    SP, SP, RESERVED_STACK
    /* Encode exception bucket as an exception result and return it */
        ori     3, 3, 2
        b       .L106
#undef STACKSIZE
ENDFUNCTION caml_start_program

/* Callback from C to OCaml */

FUNCTION caml_callback_asm
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r3, r4, r5 before C call */
        TSAN_SETUP_C_CALL 32
        std     3, (RESERVED_STACK + 0)(SP)
        std     4, (RESERVED_STACK + 8)(SP)
        std     5, (RESERVED_STACK + 16)(SP)
        mr      3, 0 /* return address */
        Far_call(__tsan_func_entry)
        ld      5, (RESERVED_STACK + 16)(SP)
        ld      4, (RESERVED_STACK + 8)(SP)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 32
#endif
    /* Initial shuffling of arguments */
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        ld      3, 0(5)             /* r3 = Argument */
                                    /* r4 = Closure */
        ld      START_PRG_ARG, 0(4) /* Code pointer */
        b       .L102
ENDFUNCTION caml_callback_asm

FUNCTION caml_callback2_asm
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r3, r4, r5 before C call */
        TSAN_SETUP_C_CALL 32
        std     3, (RESERVED_STACK + 0)(SP)
        std     4, (RESERVED_STACK + 8)(SP)
        std     5, (RESERVED_STACK + 16)(SP)
        mr      3, 0 /* return address */
        Far_call(__tsan_func_entry)
        ld      5, (RESERVED_STACK + 16)(SP)
        ld      4, (RESERVED_STACK + 8)(SP)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 32
#endif
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg,
       8(r5) = second arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      0, 4
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        mr      5, 0                /* r5 = Closure */
        Addrglobal(START_PRG_ARG, caml_apply2)
        b       .L102
ENDFUNCTION caml_callback2_asm

FUNCTION caml_callback3_asm
#if defined(WITH_THREAD_SANITIZER)
    /* Save non-callee-saved registers r3, r4, r5 before C call */
        TSAN_SETUP_C_CALL 32
        std     3, (RESERVED_STACK + 0)(SP)
        std     4, (RESERVED_STACK + 8)(SP)
        std     5, (RESERVED_STACK + 16)(SP)
        mr      3, 0 /* return address */
        Far_call(__tsan_func_entry)
        ld      5, (RESERVED_STACK + 16)(SP)
        ld      4, (RESERVED_STACK + 8)(SP)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 32
#endif
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg, 8(r5) = second arg,
       2*8(r5) = third arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      6, 4                /* r6 = Closure */
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        ld      5, 2*8(5)        /* r5 = Third argument */
        Addrglobal(START_PRG_ARG, caml_apply3)
        b       .L102
ENDFUNCTION caml_callback3_asm

/* Fibers */

/* Switch between OCaml stacks. Clobbers register 0 and switches TRAP_PTR.
   Preserves old_stack and new_stack registers */
.macro SWITCH_OCAML_STACKS old_stack, new_stack
    /* Save return address for old_stack */
        ENTER_FUNCTION
    /* Save OCaml SP and exn_handler in the stack info */
        std     SP, Stack_sp(\old_stack)
        std     TRAP_PTR, Stack_exception(\old_stack)
    /* switch stacks */
        std     \new_stack, Caml_state(current_stack)
        ld      SP, Stack_sp(\new_stack)
    /* restore exn_handler for new stack */
        ld      TRAP_PTR, Stack_exception(\new_stack)
    /* Restore return address for new_stack */
        LEAVE_FUNCTION
.endm

/*
 * A continuation is a one word object that points to a fiber. A fiber [f] will
 * point to its parent at Handler_parent(Stack_handler(f)). In the following,
 * the [last_fiber] refers to the last fiber in the linked-list formed by the
 * parent pointer.
 */

FUNCTION caml_perform
  /* r3: effect to perform
     r4: freshly allocated continuation */
        ld      5, Caml_state(current_stack) /* r5 := old stack */
        addi    6, 5, 1  /* r6 := Val_ptr(old stack) */
        std     6, 0(4)  /* Initialize continuation */
.Ldo_perform:
  /* r3: effect to perform
     r4: continuation
     r5: old_stack
     r6: last_fiber */
#if defined(WITH_THREAD_SANITIZER)
  /* Signal to TSan all stack frames exited by the perform. */
        TSAN_SAVE_CALLER_REGS
        mflr    3        /* arg1: PC of perform */
        mr      4, SP    /* arg2: SP of perform */
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 0
        Far_call(caml_tsan_exit_on_perform)
        TSAN_CLEANUP_AFTER_C_CALL 0
        SWITCH_C_TO_OCAML
        TSAN_RESTORE_CALLER_REGS
#endif
        std     6, 8(4) /* Set the last fiber field in the continuation */
        ld      7, Stack_handler(5)  /* r7 := old stack -> handler */
        ld      8, Handler_parent(7) /* r8 := parent stack */
        cmpdi   8, 0
        beq     1f
#if defined(WITH_THREAD_SANITIZER)
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 48
    /* Save non-callee-saved registers r3 to r8 */
        std     3, (RESERVED_STACK + 0)(SP)
        std     4, (RESERVED_STACK + 8)(SP)
        std     5, (RESERVED_STACK + 16)(SP)
        std     6, (RESERVED_STACK + 24)(SP)
        std     7, (RESERVED_STACK + 32)(SP)
        std     8, (RESERVED_STACK + 40)(SP)
    /* Match the TSan-enter made from caml_runstack */
        li      3, 0
        Far_call(__tsan_func_exit)
        ld      8, (RESERVED_STACK + 40)(SP)
        ld      7, (RESERVED_STACK + 32)(SP)
        ld      6, (RESERVED_STACK + 24)(SP)
        ld      5, (RESERVED_STACK + 16)(SP)
        ld      4, (RESERVED_STACK + 8)(SP)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 48
        SWITCH_C_TO_OCAML
#endif
        SWITCH_OCAML_STACKS 5, 8
  /* We have to null the Handler_parent after the switch because
     the Handler_parent is needed to unwind the stack for backtraces */
        li      0, 0
        std     0, Handler_parent(7) /* Set parent of performer to NULL */
        ld      0, Handler_effect(7)
                                     /* 1st argument: effect to perform */
                                     /* 2nd argument: continuation */
        mr      5, 6                 /* 3rd argument: last_fiber */
        mr      6, 0                 /* 4th argument: effect handler */
        Far_tailcall(caml_apply3)
1:
  /* Switch back to original performer before raising Effect.Unhandled
     (no-op unless this is a reperform) */
        ld      10, 0(4) /* r10 := performer stack (loaded from continuation) */
        addi    10, 10, -1 /* r10 := Ptr_val(r10) */
        ld      9, Caml_state(current_stack)
        SWITCH_OCAML_STACKS 9, 10
  /* No parent stack. Raise Effect.Unhandled. */
#if defined(WITH_THREAD_SANITIZER)
        /* We must let the TSan runtime know that we switched back to the
           original performer stack. For that, we perform the necessary calls
           to __tsan_func_entry via caml_tsan_entry_on_resume.
           Note that from TSan's point of view, we just exited all stack
           frames, including those of the main fiber. This is ok, because we
           re-enter them immediately via caml_tsan_entry_on_resume below. */
        TSAN_SAVE_CALLER_REGS
        mflr    3       /* arg1: PC of perform */
        mr      4, SP   /* arg2: SP of perform */
        mr      5, 10   /* arg3: performer stack */
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 0
        Far_call(caml_tsan_entry_on_resume)
        /* invoke __tsan_func_entry without shuffling stacks */
        ld      3, LR_SAVE(SP)
        Far_call(__tsan_func_entry)
        TSAN_CLEANUP_AFTER_C_CALL 0
        SWITCH_C_TO_OCAML
        TSAN_RESTORE_CALLER_REGS
#endif
        Addrglobal(C_CALL_FUN, caml_raise_unhandled_effect)
        b       .Lcaml_c_call
ENDFUNCTION caml_perform

FUNCTION caml_reperform
  /* r3: effect to perform
     r4: continuation
     r5: last_fiber */
        addi    5, 5, -1
        ld      TMP, Stack_handler(5)
        ld      5, Caml_state(current_stack) /* r5 := old stack */
        std     5, Handler_parent(TMP) /* Append to last_fiber */
        addi    6, 5, 1 /* r6 (last_fiber) := Val_ptr(old stack) */
        b       .Ldo_perform
ENDFUNCTION caml_reperform

FUNCTION caml_resume
  /* r3: new fiber
     r4: fun
     r5: arg
     r6: last_fiber */
        addi    3, 3, -1 /* r3 = Ptr_val(r3) */
        ld      12, 0(4) /* r12 = code pointer */
        mtctr   12       /* CTR = code pointer */
    /* Check if stack is null, then already used */
        cmpdi   3, 0
        beq     1f
#if defined(WITH_THREAD_SANITIZER)
        TSAN_SAVE_CALLER_REGS
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 16
        std     3, (RESERVED_STACK + 0)(SP)
    /* Necessary to include the caller of caml_resume in the TSan backtrace */
        mr      3, 0            /* arg1: return address in caller */
        Far_call(__tsan_func_entry)
    /* Signal to TSan all stack frames exited by the perform. */
        ld      3, (RESERVED_STACK + 0)(SP)
        mr      5, 3            /* arg3: fiber */
        ld      4, Stack_sp(3)  /* arg2: SP at perform */
        ld      3, LR_SAVE(4)   /* arg1: PC of perform */
        Far_call(caml_tsan_entry_on_resume)
        TSAN_CLEANUP_AFTER_C_CALL 16
        SWITCH_C_TO_OCAML
        TSAN_RESTORE_CALLER_REGS
    /* Restore r12 and ctr */
        ld      12, 0(4) /* r12 = code pointer */
        mtctr   12       /* CTR = code pointer */
#endif
    /* Add current stack to the end */
        addi    6, 6, -1 /* r6 = Ptr_val(r6) */
        ld      7, Stack_handler(6)
        ld      8, Caml_state(current_stack)
        std     8, Handler_parent(7)
    /* Switch stacks and run code */
        SWITCH_OCAML_STACKS 8, 3
        mr      3, 5
        bctr
1:      TSAN_ENTER_FUNCTION /* needed since we skip caml_c_call entry */
        Addrglobal(C_CALL_FUN, caml_raise_continuation_already_resumed)
        b       .Lcaml_c_call
ENDFUNCTION caml_resume

/* Run a function on a new stack, then either
   return the value or invoke exception handler */
FUNCTION caml_runstack
#if defined(WITH_THREAD_SANITIZER)
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 32
    /* Save non-callee-saved registers r3, r4 and r5 */
        std     3, (RESERVED_STACK + 0)(SP)
        std     4, (RESERVED_STACK + 8)(SP)
        std     5, (RESERVED_STACK + 16)(SP)
    /* Necessary to include the caller of caml_runstack in the TSan backtrace */
        mr      3, 0            /* arg1: return address in caller */
        Far_call(__tsan_func_entry)
        ld      5, (RESERVED_STACK + 16)(SP)
        ld      4, (RESERVED_STACK + 8)(SP)
        ld      3, (RESERVED_STACK + 0)(SP)
        TSAN_CLEANUP_AFTER_C_CALL 32
        SWITCH_C_TO_OCAML
#endif
  /* r3: fiber
     r4: fun
     r5: arg */
    /* save return address and TOC on old stack */
        ENTER_FUNCTION
        std     2, TOC_SAVE_PARENT(SP)
        addi    3, 3, -1   /* r3 := Ptr_val(r3) */
        ld      12, 0(4)   /* r12 := code pointer */
        mtctr   12         /* CTR := code pointer */
    /*  save old stack pointer and exception handler */
        ld      8, Caml_state(current_stack) /* r8 := old stack */
        std     SP, Stack_sp(8)
        std     TRAP_PTR, Stack_exception(8)
    /* Load new stack pointer and set parent */
        ld      TMP, Stack_handler(3)
        std     8, Handler_parent(TMP)
        std     3, Caml_state(current_stack)
        ld      9, Stack_sp(3) /* r9 := sp of new stack */
    /* Reserve 16-byte DWARF and gc_regs block, which is unused here. */
        addi    9, 9, -16
    /* Create an exception handler on the target stack
       after 16byte DWARF & gc_regs block (which is unused here) */
        addi    9, 9, -TRAP_SIZE
        Addrlabel(TMP, fiber_exn_handler)
        std     TMP, TRAP_HANDLER_OFFSET(9)
    /* link the previous exn_handler so that copying stacks works */
        ld      TMP, Stack_exception(3)
        std     TMP, TRAP_PREVIOUS_OFFSET(9)
        mr      TRAP_PTR, 9
    /* Switch to the new stack, after reserving 32 bytes at bottom */
        addi    SP, 9, -RESERVED_STACK
    /* Call the function on the new stack */
        mr      3, 5   /* argument */
.Lframe_runstack:
        bctrl
    /* Clean up on return */
        addi    8, SP, (RESERVED_STACK + TRAP_SIZE + 16)
                                       /* r8 := stack_handler */
        ld      25, Handler_value(8) /* saved across C call */
1:
        mr      26, 3                /* save return value across C call */
        ld      3, Caml_state(current_stack) /* arg to caml_free_stack */
    /* restore parent stack and exn_handler into Caml_state */
        ld      TMP, Handler_parent(8)
        std     TMP, Caml_state(current_stack)
        ld      TRAP_PTR, Stack_exception(TMP)
        std     TRAP_PTR, Caml_state(exn_handler)
    /* restore saved TOC */
        ld      27, Stack_sp(TMP)    /* saved across C calls */
        ld      2, TOC_SAVE_PARENT(27)
    /* free old stack by switching directly to c_stack;
       this is a no-alloc call */
        ld      SP, Caml_state(c_stack)
        Far_call(caml_free_stack)
    /* switch directly to parent stack */
        mr      SP, 27 /* OCaml stack */
#if defined(WITH_THREAD_SANITIZER)
    /* Signal to TSan that we exit caml_runstack (no registers to save here) */
        li      3, 0
        SWITCH_OCAML_TO_C
        TSAN_SETUP_C_CALL 0
        Far_call(__tsan_func_exit)
        TSAN_CLEANUP_AFTER_C_CALL 0
        SWITCH_C_TO_OCAML
#endif
    /* pick correct return */
        mr      3, 26  /* return value */
        mr      4, 25  /* handler value */
        ld      12, 0(4) /* code pointer */
        mtctr   12       /* code pointer */
    /* Invoke handle_value (or handle_exn) */
        LEAVE_FUNCTION
        bctr
.Lfiber_exn_handler:
        addi    8, SP, (RESERVED_STACK + 16) /* r8 := stack_handler */
        ld      25, Handler_exception(8)
        b       1b
ENDFUNCTION caml_runstack

FUNCTION caml_ml_array_bound_error
        TSAN_ENTER_FUNCTION /* needed since we skip caml_c_call entry */
        Addrglobal(C_CALL_FUN, caml_array_bound_error_asm)
        b       .Lcaml_c_call
ENDFUNCTION caml_ml_array_bound_error

        TEXT_SECTION caml_system__code_end
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .globl  caml_system.frametable
        .type   caml_system.frametable, @object
caml_system.frametable:
        .quad   2               /* two descriptors */
        .quad   .Lcaml_retaddr + 4  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .quad   .Lframe_runstack + 4 /* return address into fiber handler */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots here */
        .align 3
        .size   caml_system.frametable, .-caml_system.frametable

/* TOC entries */

        .section ".toc", "aw"

#define TOCENTRY(glob) LSYMB(glob): .quad glob
#define TOCENTRYLABEL(lbl) LLABEL(lbl): .quad .L##lbl

TOCENTRY(caml_apply2)
TOCENTRY(caml_apply3)
TOCENTRY(caml_program)
TOCENTRY(caml_exn_Stack_overflow)
TOCENTRY(caml_raise_unhandled_effect)
TOCENTRY(caml_raise_continuation_already_resumed)
TOCENTRY(caml_array_bound_error_asm)
TOCENTRYLABEL(fiber_exn_handler)
TOCENTRYLABEL(trap_handler)

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
