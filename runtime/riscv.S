/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>                 */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, RISC-V processor, 64-bit mode */
/* Must be preprocessed by cpp */

#include "caml/m.h"

#define DOMAIN_STATE_PTR s11
#define TRAP_PTR s1
#define ALLOC_PTR s10
#define ADDITIONAL_ARG t2
#define STACK_ARG_BEGIN s3
#define STACK_ARG_END s4
#define TMP t0
#define TMP2 t1

#define C_ARG_1 a0
#define C_ARG_2 a1
#define C_ARG_3 a2
#define C_ARG_4 a3

/* Support for CFI directives */

#if defined(ASM_CFI_SUPPORTED)
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_REGISTER(r1,r2) .cfi_register r1,r2
#define CFI_OFFSET(r,n) .cfi_offset r,n
#define CFI_DEF_CFA_REGISTER(r) .cfi_def_cfa_register r
#define CFI_REMEMBER_STATE .cfi_remember_state
#define CFI_RESTORE_STATE .cfi_restore_state
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_REGISTER(r1,r2)
#define CFI_OFFSET(r,n)
#define CFI_DEF_CFA_REGISTER(r)
#define CFI_REMEMBER_STATE
#define CFI_RESTORE_STATE
#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) (8*domain_field_caml_##var)(DOMAIN_STATE_PTR)

/* Globals and labels */
#define L(lbl) .L##lbl

#define FUNCTION(name) \
        .align 2; \
        .globl name; \
        .type  name, @function; \
name:; \
        CFI_STARTPROC

#define END_FUNCTION(name) \
        CFI_ENDPROC; \
        .size name, .-name

#if defined(__PIC__)
        .option pic
#define PLT(r) r@plt
#else
        .option nopic
#define PLT(r) r
#endif

#define OBJECT(name) \
        .data; \
        .align  3; \
        .globl  name; \
        .type   name, @object; \
name:
#define END_OBJECT(name) \
        .size   name, .-name

/* Stack switching operations */

/* struct stack_info */
#define Stack_sp(reg)           0(reg)
#define Stack_exception(reg)    8(reg)
#define Stack_handler(reg)      16(reg)
#define Stack_handler_from_cont(reg) 15(reg)

/* struct c_stack_link */
#define Cstack_stack(reg)       0(reg)
#define Cstack_sp(reg)          8(reg)
#define Cstack_prev(reg)        16(reg)

/* struct stack_handler */
#define Handler_value(reg)      0(reg)
#define Handler_exception(reg)  8(reg)
#define Handler_effect(reg)     16(reg)
#define Handler_parent(reg)     24(reg)

/* Switch from OCaml to C stack. */
.macro SWITCH_OCAML_TO_C
    /* Fill in Caml_state->current_stack->sp */
        ld      TMP, Caml_state(current_stack)
        sd      sp, Stack_sp(TMP)
    /* Fill in Caml_state->c_stack */
        ld      TMP2, Caml_state(c_stack)
        sd      TMP, Cstack_stack(TMP2)
        sd      sp, Cstack_sp(TMP2)
    /* Switch to C stack */
        mv      sp, TMP2
        CFI_REMEMBER_STATE
.endm

/* Switch from C to OCaml stack. */
.macro SWITCH_C_TO_OCAML
        ld      sp, Cstack_sp(sp)
        CFI_RESTORE_STATE
.endm

/* Save all of the registers that may be in use to a free gc_regs bucket
   and store ALLOC_PTR and TRAP_PTR back to Caml_state
   At the end the saved registers are placed in Caml_state(gc_regs)
 */
.macro SAVE_ALL_REGS
    /* First, save the young_ptr & exn_handler */
        sd      ALLOC_PTR, Caml_state(young_ptr)
        sd      TRAP_PTR, Caml_state(exn_handler)
    /* Now, use TMP to point to the gc_regs bucket */
        ld      TMP, Caml_state(gc_regs_buckets)
        ld      TMP2, 0(TMP) /* next ptr */
        sd      TMP2, Caml_state(gc_regs_buckets)
    /* Save allocatable integer registers */
        sd      a0, (2*8)(TMP)
        sd      a1, (3*8)(TMP)
        sd      a2, (4*8)(TMP)
        sd      a3, (5*8)(TMP)
        sd      a4, (6*8)(TMP)
        sd      a5, (7*8)(TMP)
        sd      a6, (8*8)(TMP)
        sd      a7, (9*8)(TMP)
        sd      s2, (10*8)(TMP)
        sd      s3, (11*8)(TMP)
        sd      s4, (12*8)(TMP)
        sd      s5, (13*8)(TMP)
        sd      s6, (14*8)(TMP)
        sd      s7, (15*8)(TMP)
        sd      s8, (16*8)(TMP)
        sd      s9, (17*8)(TMP)
        sd      t2, (18*8)(TMP)
        sd      t3, (19*8)(TMP)
        sd      t4, (20*8)(TMP)
        sd      t5, (21*8)(TMP)
        sd      t6, (22*8)(TMP)
        sd      s0, (23*8)(TMP)
    /* Save caller-save floating-point registers
       (callee-saves are preserved by C functions) */
        fsd     ft0, (24*8)(TMP)
        fsd     ft1, (25*8)(TMP)
        fsd     ft2, (26*8)(TMP)
        fsd     ft3, (27*8)(TMP)
        fsd     ft4, (28*8)(TMP)
        fsd     ft5, (29*8)(TMP)
        fsd     ft6, (30*8)(TMP)
        fsd     ft7, (31*8)(TMP)
        fsd     fa0, (32*8)(TMP)
        fsd     fa1, (33*8)(TMP)
        fsd     fa2, (34*8)(TMP)
        fsd     fa3, (35*8)(TMP)
        fsd     fa4, (36*8)(TMP)
        fsd     fa5, (37*8)(TMP)
        fsd     fa6, (38*8)(TMP)
        fsd     fa7, (39*8)(TMP)
        fsd     ft8, (40*8)(TMP)
        fsd     ft9, (41*8)(TMP)
        fsd     ft10, (42*8)(TMP)
        fsd     ft11, (43*8)(TMP)
        addi    TMP, TMP, 16
        sd      TMP, Caml_state(gc_regs)
.endm

/* Undo SAVE_ALL_REGS by loading the registers saved in Caml_state(gc_regs)
   and refreshing ALLOC_PTR & TRAP_PTR from Caml_state */
.macro RESTORE_ALL_REGS
    /* Restore a0, a1, freeing up the next ptr slot */
        ld      TMP, Caml_state(gc_regs)
        addi    TMP, TMP, -16
    /* Restore registers */
        ld      a0, (2*8)(TMP)
        ld      a1, (3*8)(TMP)
        ld      a2, (4*8)(TMP)
        ld      a3, (5*8)(TMP)
        ld      a4, (6*8)(TMP)
        ld      a5, (7*8)(TMP)
        ld      a6, (8*8)(TMP)
        ld      a7, (9*8)(TMP)
        ld      s2, (10*8)(TMP)
        ld      s3, (11*8)(TMP)
        ld      s4, (12*8)(TMP)
        ld      s5, (13*8)(TMP)
        ld      s6, (14*8)(TMP)
        ld      s7, (15*8)(TMP)
        ld      s8, (16*8)(TMP)
        ld      s9, (17*8)(TMP)
        ld      t2, (18*8)(TMP)
        ld      t3, (19*8)(TMP)
        ld      t4, (20*8)(TMP)
        ld      t5, (21*8)(TMP)
        ld      t6, (22*8)(TMP)
        ld      s0, (23*8)(TMP)
        fld     ft0, (24*8)(TMP)
        fld     ft1, (25*8)(TMP)
        fld     ft2, (26*8)(TMP)
        fld     ft3, (27*8)(TMP)
        fld     ft4, (28*8)(TMP)
        fld     ft5, (29*8)(TMP)
        fld     ft6, (30*8)(TMP)
        fld     ft7, (31*8)(TMP)
        fld     fa0, (32*8)(TMP)
        fld     fa1, (33*8)(TMP)
        fld     fa2, (34*8)(TMP)
        fld     fa3, (35*8)(TMP)
        fld     fa4, (36*8)(TMP)
        fld     fa5, (37*8)(TMP)
        fld     fa6, (38*8)(TMP)
        fld     fa7, (39*8)(TMP)
        fld     ft8, (40*8)(TMP)
        fld     ft9, (41*8)(TMP)
        fld     ft10, (42*8)(TMP)
        fld     ft11, (43*8)(TMP)
    /* Put gc_regs struct back in bucket linked list */
        ld      TMP2, Caml_state(gc_regs_buckets)
        sd      TMP2, 0(TMP) /* next ptr */
        sd      TMP, Caml_state(gc_regs_buckets)
    /* Reload new allocation pointer & exn handler */
        ld      ALLOC_PTR, Caml_state(young_ptr)
        ld      TRAP_PTR, Caml_state(exn_handler)
.endm

        .section        .text
/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

FUNCTION(caml_call_realloc_stack)
    /* Save return address */
        CFI_OFFSET(ra, -8)
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
    /* Save all registers (including ALLOC_PTR & TRAP_PTR) */
        SAVE_ALL_REGS
        ld      C_ARG_1, 16(sp) /* argument */
        SWITCH_OCAML_TO_C
        call    PLT(caml_try_realloc_stack)
        SWITCH_C_TO_OCAML
        beqz    a0, 1f
        RESTORE_ALL_REGS
    /* Free stack space and return to caller */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        ret
1:      RESTORE_ALL_REGS
    /* Raise the Stack_overflow exception */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        addi    sp, sp, 16 /* pop argument */
        la      a0, caml_exn_Stack_overflow
        j       caml_raise_exn
END_FUNCTION(caml_call_realloc_stack)

FUNCTION(caml_call_gc)
L(caml_call_gc):
    /* Save return address */
        CFI_OFFSET(ra, -8)
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
    /* Store all registers (including ALLOC_PTR & TRAP_PTR) */
        SAVE_ALL_REGS
        SWITCH_OCAML_TO_C
    /* Call the garbage collector */
        call    PLT(caml_garbage_collection)
        SWITCH_C_TO_OCAML
        RESTORE_ALL_REGS
    /* Free stack space and return to caller */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        ret
END_FUNCTION(caml_call_gc)

FUNCTION(caml_alloc1)
        ld      TMP, Caml_state(young_limit)
        addi    ALLOC_PTR, ALLOC_PTR, -16
        bltu    ALLOC_PTR, TMP, L(caml_call_gc)
        ret
END_FUNCTION(caml_alloc1)

FUNCTION(caml_alloc2)
        ld      TMP, Caml_state(young_limit)
        addi    ALLOC_PTR, ALLOC_PTR, -24
        bltu    ALLOC_PTR, TMP, L(caml_call_gc)
        ret
END_FUNCTION(caml_alloc2)

FUNCTION(caml_alloc3)
        ld      TMP, Caml_state(young_limit)
        addi    ALLOC_PTR, ALLOC_PTR, -32
        bltu    ALLOC_PTR, TMP, L(caml_call_gc)
        ret
END_FUNCTION(caml_alloc3)

FUNCTION(caml_allocN)
        ld      TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, ADDITIONAL_ARG
        bltu    ALLOC_PTR, TMP, L(caml_call_gc)
        ret
END_FUNCTION(caml_allocN)

/* Call a C function from OCaml */
/* Function to call is in ADDITIONAL_ARG */

FUNCTION(caml_c_call)
        CFI_OFFSET(ra, -8)
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
    /* Switch form OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        sd      ALLOC_PTR, Caml_state(young_ptr)
        sd      TRAP_PTR, Caml_state(exn_handler)
    /* Call the function */
        jalr    ADDITIONAL_ARG
    /* Reload alloc ptr */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Load ocaml stack */
        SWITCH_C_TO_OCAML
    /* Return */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        ret
END_FUNCTION(caml_c_call)

FUNCTION(caml_c_call_stack_args)
    /* Arguments:
        C arguments  : a0 to a7, fa0 to fa7
        C function   : ADDITIONAL_ARG
        C stack args : begin=STACK_ARG_BEGIN
                       end=STACK_ARG_END */
        CFI_OFFSET(ra, -8)
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        sd      ALLOC_PTR, Caml_state(young_ptr)
        sd      TRAP_PTR, Caml_state(exn_handler)
    /* Store sp to restore after call */
        mv      s2, sp
    /* Copy arguments from OCaml to C stack
       NB: STACK_ARG_{BEGIN,END} are 16-byte aligned */
1:      addi    STACK_ARG_END, STACK_ARG_END, -16
        bltu    STACK_ARG_END, STACK_ARG_BEGIN, 2f
        ld      TMP, 0(STACK_ARG_END)
        ld      TMP2, 8(STACK_ARG_END)
        addi    sp, sp, -16
        sd      TMP, 0(sp)
        sd      TMP2, 8(sp)
        CFI_ADJUST(16)
        j       1b
2:  /* Call the function */
        jalr    ADDITIONAL_ARG
    /* Restore stack */
        mv      sp, s2
    /* Reload alloc ptr */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        ret
END_FUNCTION(caml_c_call_stack_args)

/* Start the OCaml program */

FUNCTION(caml_start_program)
    /* domain state is passed as arg from C */
        mv      TMP, C_ARG_1
        la      TMP2, caml_program

/* Code shared with caml_callback* */
/* Address of domain state is in TMP */
/* Address of OCaml code to call is in TMP2 */
/* Arguments to the OCaml code are in a0...a7 */

L(jump_to_caml):
    /* Set up stack frame and save callee-save registers */
        CFI_OFFSET(ra, -200)
        addi    sp, sp, -208
        sd      ra, 8(sp)
        CFI_ADJUST(208)
        sd      s0, (2*8)(sp)
        sd      s1, (3*8)(sp)
        sd      s2, (4*8)(sp)
        sd      s3, (5*8)(sp)
        sd      s4, (6*8)(sp)
        sd      s5, (7*8)(sp)
        sd      s6, (8*8)(sp)
        sd      s7, (9*8)(sp)
        sd      s8, (10*8)(sp)
        sd      s9, (11*8)(sp)
        sd      s10, (12*8)(sp)
        sd      s11, (13*8)(sp)
        fsd     fs0, (14*8)(sp)
        fsd     fs1, (15*8)(sp)
        fsd     fs2, (16*8)(sp)
        fsd     fs3, (17*8)(sp)
        fsd     fs4, (18*8)(sp)
        fsd     fs5, (19*8)(sp)
        fsd     fs6, (20*8)(sp)
        fsd     fs7, (21*8)(sp)
        fsd     fs8, (22*8)(sp)
        fsd     fs9, (23*8)(sp)
        fsd     fs10, (24*8)(sp)
        fsd     fs11, (25*8)(sp)
    /* Load domain state pointer from argument */
        mv      DOMAIN_STATE_PTR, TMP
    /* Reload allocation pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Build (16-byte aligned) struct c_stack_link on the C stack */
        ld      t2, Caml_state(c_stack)
        addi    sp, sp, -32
        sd      t2, Cstack_prev(sp)
        sd      x0, Cstack_stack(sp)
        sd      x0, Cstack_sp(sp)
        CFI_ADJUST(32)
        sd      sp, Caml_state(c_stack)
    /* Load the OCaml stack */
        ld      t2, Caml_state(current_stack)
        ld      t2, Stack_sp(t2)
    /* Store the gc_regs for callbacks during a GC */
        ld      t3, Caml_state(gc_regs)
        addi    t2, t2, -8
        sd      t3, 0(t2)
    /* Store the stack pointer to allow DWARF unwind */
        addi    t2, t2, -8
        sd      sp, 0(t2) /* C_stack_sp */
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        ld      t3, Caml_state(exn_handler)
        la      t4, L(trap_handler)
        addi    t2, t2, -16
        sd      t3, 0(t2)
        sd      t4, 8(t2)
        mv      TRAP_PTR, t2
    /* Switch stacks and call the OCaml code */
        mv      sp, t2
        CFI_REMEMBER_STATE
    /* Call the OCaml code */
        jalr    TMP2
L(caml_retaddr):
    /* Pop the trap frame, restoring Caml_state->exn_handler */
        ld      t2, 0(sp)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
        sd      t2, Caml_state(exn_handler)
L(return_result):
    /* Restore GC regs */
        ld      t2, 0(sp)
        ld      t3, 8(sp)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
        sd      t3, Caml_state(gc_regs)
    /* Update allocation pointer */
        sd      ALLOC_PTR, Caml_state(young_ptr)
    /* Return to C stack */
        ld      t2, Caml_state(current_stack)
        sd      sp, Stack_sp(t2)
        ld      t3, Caml_state(c_stack)
        mv      sp, t3
        CFI_RESTORE_STATE
    /* Pop the struct c_stack_link */
        ld      t2, Cstack_prev(sp)
        addi    sp, sp, 32
        CFI_ADJUST(-32)
        sd      t2, Caml_state(c_stack)
    /* Reload callee-save register and return address */
        ld      s0, (2*8)(sp)
        ld      s1, (3*8)(sp)
        ld      s2, (4*8)(sp)
        ld      s3, (5*8)(sp)
        ld      s4, (6*8)(sp)
        ld      s5, (7*8)(sp)
        ld      s6, (8*8)(sp)
        ld      s7, (9*8)(sp)
        ld      s8, (10*8)(sp)
        ld      s9, (11*8)(sp)
        ld      s10, (12*8)(sp)
        ld      s11, (13*8)(sp)
        fld     fs0, (14*8)(sp)
        fld     fs1, (15*8)(sp)
        fld     fs2, (16*8)(sp)
        fld     fs3, (17*8)(sp)
        fld     fs4, (18*8)(sp)
        fld     fs5, (19*8)(sp)
        fld     fs6, (20*8)(sp)
        fld     fs7, (21*8)(sp)
        fld     fs8, (22*8)(sp)
        fld     fs9, (23*8)(sp)
        fld     fs10, (24*8)(sp)
        fld     fs11, (25*8)(sp)
        ld      ra, 8(sp)
        addi    sp, sp, 208
        CFI_ADJUST(-208)
    /* Return to C caller */
        ret
END_FUNCTION(caml_start_program)

/* The trap handler */

        .align  2
L(trap_handler):
        CFI_STARTPROC
    /* Save exception pointer */
        sd      TRAP_PTR, Caml_state(exn_handler)
    /* Encode exception pointer */
        ori     a0, a0, 2
    /* Return it */
        j       L(return_result)
        CFI_ENDPROC

/* Exceptions */

.macro JUMP_TO_TRAP_PTR
    /* Cut stack at current trap handler */
        mv      sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ld      TMP, 8(sp)
        ld      TRAP_PTR, 0(sp)
        addi    sp, sp, 16
        jr      TMP
.endm

/* Raise an exception from OCaml */
FUNCTION(caml_raise_exn)
    /* Test if backtrace is active */
        ld      TMP, Caml_state(backtrace_active)
        bnez    TMP, 2f
1:
        JUMP_TO_TRAP_PTR
2:  /* Zero backtrace_pos */
        sd      x0, Caml_state(backtrace_pos)
L(caml_reraise_exn_stash):
    /* Preserve exception bucket in callee-save register s2 */
        mv      s2, a0
    /* Stash the backtrace */
                                  /* arg1: exn bucket, already in a0 */
        mv      a1, ra            /* arg2: pc of raise */
        mv      a2, sp            /* arg3: sp of raise */
        mv      a3, TRAP_PTR      /* arg4: sp of handler */
    /* Switch to C stack */
        ld      TMP, Caml_state(c_stack)
        mv      sp, TMP
        call    PLT(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mv      a0, s2
        j       1b
END_FUNCTION(caml_raise_exn)

FUNCTION(caml_reraise_exn)
        ld      TMP, Caml_state(backtrace_active)
        bnez    TMP, L(caml_reraise_exn_stash)
        JUMP_TO_TRAP_PTR
END_FUNCTION(caml_reraise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
    /* Load the domain state ptr */
        mv      DOMAIN_STATE_PTR, C_ARG_1
    /* Load the exception bucket */
        mv      a0, C_ARG_2
    /* Reload trap ptr and alloc ptr */
        ld      TRAP_PTR, Caml_state(exn_handler)
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Discard the C stack pointer and reset to ocaml stack */
        ld      TMP, Caml_state(current_stack)
        ld      TMP, Stack_sp(TMP)
        mv      sp, TMP
    /* Restore frame and link on return to OCaml */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        j       caml_raise_exn
END_FUNCTION(caml_raise_exception)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
    /* Initial shuffling of arguments */
    /* (a0 = Caml_state, a1 = closure, 0(a2) = first arg) */
        mv      TMP, a0
        ld      a0, 0(a2)           /* a0 = first arg */
                                    /* a1 = closure environment */
        ld      TMP2, 0(a1)          /* code pointer */
        j       L(jump_to_caml)
END_FUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
    /* Initial shuffling of arguments */
    /* (a0 = Caml_state, a1 = closure, 0(a2) = arg1, 8(a2) = arg2) */
        mv      TMP, a0
        mv      TMP2, a1
        ld      a0, 0(a2) /* a0 = first arg */
        ld      a1, 8(a2) /* a1 = second arg */
        mv      a2, TMP2   /* a2 = closure environment */
        la      TMP2, caml_apply2
        j       L(jump_to_caml)
END_FUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
    /* Initial shuffling of arguments */
    /* (a0 = Caml_state, a1 = closure, 0(a2) = arg1, 8(a2) = arg2,
        16(a2) = arg3) */
        mv      TMP, a0
        mv      a3, a1       /* a3 = closure environment */
        ld      a0, 0(a2)    /* a0 = first arg */
        ld      a1, 8(a2)    /* a1 = second arg */
        ld      a2, 16(a2)   /* a2 = third arg */
        la      TMP2, caml_apply3
        j       L(jump_to_caml)
END_FUNCTION(caml_callback3_asm)

/* Fibers */

/* Switch between OCaml stacks. Clobbers TMP and switches TRAP_PTR
   Preserves old_stack and new_stack registers */
.macro SWITCH_OCAML_STACKS old_stack, new_stack
    /* Save frame pointer and return address for old_stack */
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
    /* Save OCaml SP and exn_handler in the stack info */
        sd      sp, Stack_sp(\old_stack)
        sd      TRAP_PTR, Stack_exception(\old_stack)
    /* switch stacks */
        sd      \new_stack, Caml_state(current_stack)
        ld      TMP, Stack_sp(\new_stack)
        mv      sp, TMP
    /* restore exn_handler for new stack */
        ld      TRAP_PTR, Stack_exception(\new_stack)
    /* Restore frame pointer and return address for new_stack */
        ld      ra, 8(sp)
        addi    sp, sp, 16
.endm

/*
 * A continuation is a one word object that points to a fiber. A fiber [f] will
 * point to its parent at Handler_parent(Stack_handler(f)). In the following,
 * the [last_fiber] refers to the last fiber in the linked-list formed by the
 * parent pointer.
 */

FUNCTION(caml_perform)
    /*  a0: effect to perform
        a1: freshly allocated continuation */
        ld      a2, Caml_state(current_stack) /* a2 := old stack */
        addi    a3, a2, 1 /* a3 := Val_ptr(old stack) */
        sd      a3, 0(a1) /* Iniitalize continuation */
L(do_perform):
    /*  a0: effect to perform
        a1: continuation
        a2: old_stack
        a3: last_fiber */

        ld      t3, Stack_handler(a2)  /* t3 := old stack -> handler */
        ld      t4, Handler_parent(t3) /* t4 := parent stack */
        beqz    t4, 1f
        SWITCH_OCAML_STACKS a2, t4
    /*  we have to null the Handler_parent after the switch because
        the Handler_parent is needed to unwind the stack for backtraces */
        sd      x0, Handler_parent(t3) /* Set parent of performer to NULL */
        ld      TMP, Handler_effect(t3)
        mv      a2, a3                 /* a2 := last_fiber */
        mv      a3, TMP                /* a3 := effect handler */
        tail    PLT(caml_apply3)
1:
    /*  switch back to original performer before raising Unhandled
        (no-op unless this is a reperform) */
        ld      t4, 0(a1) /* load performer stack from continuation */
        addi    t4, t4, -1 /* t4 := Ptr_val(t4) */
        ld      t3, Caml_state(current_stack)
        SWITCH_OCAML_STACKS t3, t4
    /*  No parent stack. Raise Unhandled. */
        la      a0, caml_exn_Unhandled
        j       caml_raise_exn
END_FUNCTION(caml_perform)

FUNCTION(caml_reperform)
    /*  a0: effect to perform
        a1: continuation
        a2: last_fiber */
        ld      TMP, Stack_handler_from_cont(a2)
        ld      a2, Caml_state(current_stack) /* a2 := old stack */
        sd      a2, Handler_parent(TMP) /* Append to last_fiber */
        addi    a3, a2, 1 /* a3 (last_fiber) := Val_ptr(old stack) */
        j       L(do_perform)
END_FUNCTION(caml_reperform)

FUNCTION(caml_resume)
    /*  a0: new fiber
        a1: fun
        a2: arg */
        addi    a0, a0, -1 /* a0 = Ptr_val(a0) */
        ld      a3, 0(a1)  /* code pointer */
    /* Check if stack null, then already used */
        beqz    a0, 2f
    /* Find end of list of stacks (put in t2) */
        mv      TMP, a0
1:      ld      t2, Stack_handler(TMP)
        ld      TMP, Handler_parent(t2)
        bnez    TMP, 1b
    /* Add current stack to the end */
        ld      t3, Caml_state(current_stack)
        sd      t3, Handler_parent(t2)
        SWITCH_OCAML_STACKS t3, a0
        mv      a0, a2
        jr      a3
2:      la      a0, caml_exn_Continuation_already_taken
        j       caml_raise_exn
END_FUNCTION(caml_resume)

/* Run a function on a new stack, then either
   return the value or invoke exception handler */
FUNCTION(caml_runstack)
    /*  a0: fiber
        a1: fun
        a2: arg */
        CFI_OFFSET(ra, -8)
        addi    sp, sp, -16
        sd      ra, 8(sp)
        CFI_ADJUST(16)
        addi    a0, a0, -1  /* a0 := Ptr_val(a0) */
        ld      a3, 0(a1)   /* code pointer */
    /*  save old stack pointer and exception handler */
        ld      t2, Caml_state(current_stack) /* t2 := old stack */
        sd      sp, Stack_sp(t2)
        sd      TRAP_PTR, Stack_exception(t2)
    /* Load new stack pointer and set parent */
        ld      TMP, Stack_handler(a0)
        sd      t2, Handler_parent(TMP)
        sd      a0, Caml_state(current_stack)
        ld      t3, Stack_sp(a0) /* t3 := sp of new stack */
    /* Create an exception handler on the target stack
       after 16byte DWARF & gc_regs block (which is unused here) */
        addi    t3, t3, -32
        la      TMP, L(fiber_exn_handler)
        sd      TMP, 8(t3)
    /* link the previous exn_handler so that copying stacks works */
        ld      TMP, Stack_exception(a0)
        sd      TMP, 0(t3)
        mv      TRAP_PTR, t3
    /* Switch to the new stack */
        mv      sp, t3
        CFI_REMEMBER_STATE
    /* Call the function on the new stack */
        mv      a0, a2
        jalr    a3
L(frame_runstack):
        addi    t2, sp, 32 /* t2 := stack_handler */
        ld      s2, Handler_value(t2) /* saved across C call */
1:
        mv      s3, a0     /* save return across C call */
        ld      a0, Caml_state(current_stack) /* arg to caml_free_stack */
    /* restore parent stack and exn_handler into Caml_state */
        ld      TMP, Handler_parent(t2)
        sd      TMP, Caml_state(current_stack)
        ld      TRAP_PTR, Stack_exception(TMP)
        sd      TRAP_PTR, Caml_state(exn_handler)
    /* free old stack by switching directly to c_stack;
       is a no-alloc call */
        ld      s4, Stack_sp(TMP) /* saved across C call */
        CFI_RESTORE_STATE
        CFI_REMEMBER_STATE
        ld      TMP, Caml_state(c_stack)
        mv      sp, TMP
        call    PLT(caml_free_stack)
    /* switch directly to parent stack with correct return */
        mv      a0, s3
        mv      a1, s2
        mv      sp, s4
        CFI_RESTORE_STATE
        ld      TMP, 0(s2)  /* code pointer */
    /* Invoke handle_value (or handle_exn) */
        ld      ra, 8(sp)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
        jr      TMP
L(fiber_exn_handler):
        addi    t2, sp, 16  /* t2 := stack_handler */
        ld      s2, Handler_exception(t2)
        j       1b
END_FUNCTION(caml_runstack)

FUNCTION(caml_ml_array_bound_error)
    /* Load address of [caml_array_bound_error_asm] in ADDITIONAL_ARG */
        la      ADDITIONAL_ARG, caml_array_bound_error_asm
    /* Call that function */
        j       caml_c_call
END_FUNCTION(caml_ml_array_bound_error)

        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

OBJECT(caml_system.frametable)
        .quad   2                 /* two descriptors */
        .quad   L(caml_retaddr)   /* return address into callback */
        .short  -1                /* negative frame size => use callback link */
        .short  0                 /* no roots */
        .align  3
        .quad   L(frame_runstack) /* return address into fiber handler */
        .short  -1                /* negative frame size => use callback link */
        .short  0                 /* no roots */
        .align  3
END_OBJECT(caml_system.frametable)
