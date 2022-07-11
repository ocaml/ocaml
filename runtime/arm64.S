/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*             Tom Kelly, OCaml Labs Consultancy, UK                      */
/*                                                                        */
/*   Copyright 2013 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*   Copyright 2022 OCaml Labs Consultancy Ltd.                           */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, ARM processor, 64-bit mode */
/* Must be preprocessed by cpp */

#include "caml/m.h"

/* Special registers */

#define DOMAIN_STATE_PTR x28
#define TRAP_PTR x26
#define ALLOC_PTR x27
#define ADDITIONAL_ARG x8
#define STACK_ARG_BEGIN x20
#define STACK_ARG_END x21
#define TMP x16
#define TMP2 x17

#define C_ARG_1 x0
#define C_ARG_2 x1
#define C_ARG_3 x2
#define C_ARG_4 x3

/* Support for CFI directives */

#if defined(ASM_CFI_SUPPORTED)
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_REGISTER(r1,r2) .cfi_register r1,r2
#define CFI_OFFSET(r,n) .cfi_offset r,n
#define CFI_DEF_CFA_REGISTER(r) .cfi_def_cfa_register r
#define CFI_SIGNAL_FRAME .cfi_signal_frame
#define CFI_REMEMBER_STATE .cfi_remember_state
#define CFI_RESTORE_STATE .cfi_restore_state
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_REGISTER(r1,r2)
#define CFI_OFFSET(r,n)
#define CFI_DEF_CFA_REGISTER(r)
#define CFI_SIGNAL_FRAME
#define CFI_REMEMBER_STATE
#define CFI_RESTORE_STATE
#endif

/* DWARF

   These constants are taken from:

     DWARF Debugging Information Format, Version 3
     http://dwarfstd.org/doc/Dwarf3.pdf

   with the ARM64 specific register numbers coming from
   Table 4 ("Mapping from DWARF register numbers to Arm
   64-bit architecture registers") of:

     https://developer.arm.com/documentation/ihi0057/latest

 */

#define DW_CFA_def_cfa_expression 0x0f
#define DW_REG_x21                21
#define DW_REG_sp                 31
#define DW_OP_breg                0x70
#define DW_OP_deref               0x06
#define DW_OP_plus_uconst         0x23


        .set    domain_curr_field, 0
#if defined(SYS_macosx)
#define DOMAIN_STATE(c_type, name) DOMAIN_STATE c_type, name
        .macro DOMAIN_STATE c_type, name
        .equ    domain_field_caml_\name, domain_curr_field
        .set    domain_curr_field, domain_curr_field + 1
        .endm
#else
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#endif
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) [DOMAIN_STATE_PTR, 8*domain_field_caml_##var]

/* Globals and labels */
#if defined(SYS_macosx)
#define G(sym) _##sym
#define L(lbl) L##lbl
#else
#define G(sym) sym
#define L(lbl) .L##lbl
#endif

#if defined(SYS_macosx)

#define ADDRGLOBAL(reg,symb) ADDRGLOBAL reg, symb
        .macro ADDRGLOBAL reg, symb
        adrp        TMP2, G(\symb)@GOTPAGE
        ldr         \reg, [TMP2, G(\symb)@GOTPAGEOFF]
        .endm
#elif defined(__PIC__)
#define ADDRGLOBAL(reg,symb) \
        adrp    TMP2, :got:G(symb); \
        ldr     reg, [TMP2, #:got_lo12:G(symb)]
#else

#define ADDRGLOBAL(reg,symb) \
        adrp    reg, G(symb); \
        add     reg, reg, #:lo12:G(symb)

#endif

#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot.code_begin)
        .globl  G(caml_hot.code_begin)
G(caml_hot.code_begin):

        TEXT_SECTION(caml_hot.code_end)
        .globl  G(caml_hot.code_end)
G(caml_hot.code_end):
#endif

#if defined(SYS_macosx)

#define FUNCTION(name) FUNCTION name
        .macro FUNCTION name
        TEXT_SECTION(G(\name))
        .align 2
        .globl G(\name)
G(\name):
        .endm
#define END_FUNCTION(name)

#define OBJECT(name) OBJECT name
        .macro OBJECT name
        .data
        .align  3
        .globl  G(\name)
G(\name):
        .endm
#define END_OBJECT(name)

#else

#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .align  2; \
        .globl  G(name); \
        .type   G(name), %function; \
G(name):
#define END_FUNCTION(name) \
        .size   G(name), .-G(name)

#define OBJECT(name) \
        .data; \
        .align  3; \
        .globl  G(name); \
        .type   G(name), %object; \
G(name):
#define END_OBJECT(name) \
        .size   G(name), .-G(name)
#endif

/* Stack switching operations */

/* struct stack_info */
#define Stack_sp(reg)           [reg]
#define Stack_sp_offset         0
#define Stack_exception(reg)    [reg, #8]
#define Stack_handler(reg)      [reg, #16]
#define Stack_handler_from_cont(reg) [reg, #15]

/* struct c_stack_link */
#define Cstack_stack(reg)       [reg]
#define Cstack_sp(reg)          [reg, #8]
#define Cstack_sp_offset        8
#define Cstack_prev(reg)        [reg, #16]

/* struct stack_handler */
#define Handler_value(reg)      [reg]
#define Handler_exception(reg)  [reg, #8]
#define Handler_effect(reg)     [reg, #16]
#define Handler_parent(reg)     [reg, #24]
#define Handler_parent_offset   24

/* Switch from OCaml to C stack. */
.macro SWITCH_OCAML_TO_C
    /* Fill in Caml_state->current_stack->sp */
        ldr     TMP, Caml_state(current_stack)
        mov     TMP2, sp
        str     TMP2, Stack_sp(TMP)
    /* Fill in Caml_state->c_stack */
        ldr     TMP2, Caml_state(c_stack)
        str     TMP, Cstack_stack(TMP2)
        mov     TMP, sp
        str     TMP, Cstack_sp(TMP2)
    /* Switch to C stack */
        mov     sp, TMP2
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
    /* sp  points to the c_stack_link. */
        .cfi_escape DW_CFA_def_cfa_expression, 5,                 \
           DW_OP_breg + DW_REG_sp, Cstack_sp_offset, DW_OP_deref, \
           DW_OP_plus_uconst, 16 /* fp + retaddr */
#endif
.endm

/* Switch from C to OCaml stack. */
.macro SWITCH_C_TO_OCAML
        ldr     TMP, Cstack_sp(sp)
        mov     sp, TMP
        CFI_RESTORE_STATE
.endm

/* Save all of the registers that may be in use to a free gc_regs bucket
   and store ALLOC_PTR and TRAP_PTR back to Caml_state
   At the end the saved registers are placed in Caml_state(gc_regs)
 */
.macro SAVE_ALL_REGS
    /* First, save the young_ptr & exn_handler */
        str     ALLOC_PTR, Caml_state(young_ptr)
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Now, use TMP to point to the gc_regs bucket */
        ldr     TMP, Caml_state(gc_regs_buckets)
        ldr     TMP2, [TMP, 0] /* next ptr */
        str     TMP2, Caml_state(gc_regs_buckets)
    /* Save allocatable registers */
        stp     x0, x1, [TMP, 16]
        stp     x2, x3, [TMP, 32]
        stp     x4, x5, [TMP, 48]
        stp     x6, x7, [TMP, 64]
        stp     x8, x9, [TMP, 80]
        stp     x10, x11, [TMP, 96]
        stp     x12, x13, [TMP, 112]
        stp     x14, x15, [TMP, 128]
        stp     x19, x20, [TMP, 144]
        stp     x21, x22, [TMP, 160]
        stp     x23, x24, [TMP, 176]
        str     x25, [TMP, 192]
    /* Save caller-save floating-point registers
       (callee-saves are preserved by C functions) */
        stp     d0, d1, [TMP, 208]
        stp     d2, d3, [TMP, 224]
        stp     d4, d5, [TMP, 240]
        stp     d6, d7, [TMP, 256]
        stp     d16, d17, [TMP, 272]
        stp     d18, d19, [TMP, 288]
        stp     d20, d21, [TMP, 304]
        stp     d22, d23, [TMP, 320]
        stp     d24, d25, [TMP, 336]
        stp     d26, d27, [TMP, 352]
        stp     d28, d29, [TMP, 368]
        stp     d30, d31, [TMP, 384]
        add     TMP, TMP, #16
        str     TMP, Caml_state(gc_regs)
.endm

/* Undo SAVE_ALL_REGS by loading the registers saved in Caml_state(gc_regs)
   and refreshing ALLOC_PTR & TRAP_PTR from Caml_state */
.macro RESTORE_ALL_REGS
    /* Restore x0, x1, freeing up the next ptr slot */
        ldr     TMP, Caml_state(gc_regs)
        sub     TMP, TMP, #16
    /* Restore registers */
        ldp     x0, x1, [TMP, 16]
        ldp     x2, x3, [TMP, 32]
        ldp     x4, x5, [TMP, 48]
        ldp     x6, x7, [TMP, 64]
        ldp     x8, x9, [TMP, 80]
        ldp     x10, x11, [TMP, 96]
        ldp     x12, x13, [TMP, 112]
        ldp     x14, x15, [TMP, 128]
        ldp     x19, x20, [TMP, 144]
        ldp     x21, x22, [TMP, 160]
        ldp     x23, x24, [TMP, 176]
        ldr     x25, [TMP, 192]
        ldp     d0, d1, [TMP, 208]
        ldp     d2, d3, [TMP, 224]
        ldp     d4, d5, [TMP, 240]
        ldp     d6, d7, [TMP, 256]
        ldp     d16, d17, [TMP, 272]
        ldp     d18, d19, [TMP, 288]
        ldp     d20, d21, [TMP, 304]
        ldp     d22, d23, [TMP, 320]
        ldp     d24, d25, [TMP, 336]
        ldp     d26, d27, [TMP, 352]
        ldp     d28, d29, [TMP, 368]
        ldp     d30, d31, [TMP, 384]
    /* Put gc_regs struct back in bucket linked list */
        ldr     TMP2, Caml_state(gc_regs_buckets)
        str     TMP2, [TMP, 0]  /* next ptr */
        str     TMP, Caml_state(gc_regs_buckets)
    /* Reload new allocation pointer & exn handler */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
        ldr     TRAP_PTR, Caml_state(exn_handler)
.endm

/* Allocation functions and GC interface */
        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

FUNCTION(caml_call_realloc_stack)
        CFI_STARTPROC
    /* Save return address and frame pointer */
        CFI_OFFSET(29, -16)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
    /* Save all registers (including ALLOC_PTR & TRAP_PTR) */
        SAVE_ALL_REGS
        ldr     C_ARG_1, [sp, 16] /* argument */
        SWITCH_OCAML_TO_C
        bl      G(caml_try_realloc_stack)
        SWITCH_C_TO_OCAML
        cbz     x0, 1f
        RESTORE_ALL_REGS
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp], 16
        ret
1:      RESTORE_ALL_REGS
    /* Raise the Stack_overflow exception */
        ldp     x29, x30, [sp], 16
        add     sp, sp, 16 /* pop argument */
        ADDRGLOBAL(x0, caml_exn_Stack_overflow)
        b       G(caml_raise_exn)
        CFI_ENDPROC
        END_FUNCTION(caml_call_realloc_stack)

FUNCTION(caml_call_gc)
        CFI_STARTPROC
L(caml_call_gc):
    /* Save return address and frame pointer */
        CFI_OFFSET(29, -16)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
    /* Store all registers (including ALLOC_PTR & TRAP_PTR) */
        SAVE_ALL_REGS
        SWITCH_OCAML_TO_C
    /* Call the garbage collector */
        bl      G(caml_garbage_collection)
        SWITCH_C_TO_OCAML
        RESTORE_ALL_REGS
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp], 16
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_call_gc)

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc1)

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc2)

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc3)

FUNCTION(caml_allocN)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, ADDITIONAL_ARG
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_allocN)

/* Call a C function from OCaml */
/* Function to call is in ADDITIONAL_ARG */

FUNCTION(caml_c_call)
        CFI_STARTPROC
        CFI_OFFSET(29, -16)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        str     ALLOC_PTR, Caml_state(young_ptr)
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Call the function */
        blr     ADDITIONAL_ARG
    /* Reload alloc ptr  */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Load ocaml stack */
        SWITCH_C_TO_OCAML
    /* Return */
        ldp     x29, x30, [sp], 16
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_c_call)

FUNCTION(caml_c_call_stack_args)
        CFI_STARTPROC
    /* Arguments:
        C arguments  : x0 to x7, d0 to d7
        C function   : ADDITIONAL_ARG
        C stack args : begin=STACK_ARG_BEGIN
                       end=STACK_ARG_END */
        CFI_OFFSET(29, -16)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
    /* Make the exception handler alloc ptr available to the C code */
        str     ALLOC_PTR, Caml_state(young_ptr)
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Store sp to restore after call */
        mov     x19, sp
    /* Copy arguments from OCaml to C stack
       NB: STACK_ARG_{BEGIN,END} are 16-byte aligned */
1:      sub     STACK_ARG_END, STACK_ARG_END, 16
        cmp     STACK_ARG_END, STACK_ARG_BEGIN
        b.lo    2f
        ldp     TMP, TMP2, [STACK_ARG_END]
        stp     TMP, TMP2, [sp, -16]!; CFI_ADJUST(16)
        b       1b
2:  /* Call the function */
        blr     ADDITIONAL_ARG
    /* Restore stack */
        mov     sp, x19
    /* Reload alloc ptr */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return */
        ldp     x29, x30, [sp], 16
        ret
CFI_ENDPROC

/* Start the OCaml program */

FUNCTION(caml_start_program)
        CFI_STARTPROC
    /* domain state is passed as arg from C */
        mov     TMP, C_ARG_1
        ADDRGLOBAL(TMP2, caml_program)

/* Code shared with caml_callback* */
/* Address of domain state is in TMP */
/* Address of OCaml code to call is in TMP2 */
/* Arguments to the OCaml code are in x0...x7 */

L(jump_to_caml):
    /* Set up stack frame and save callee-save registers */
        CFI_OFFSET(29, -160)
        CFI_OFFSET(30, -152)
        stp     x29, x30, [sp, -160]!
        CFI_ADJUST(160)
        add     x29, sp, #0
        stp     x19, x20, [sp, 16]
        stp     x21, x22, [sp, 32]
        stp     x23, x24, [sp, 48]
        stp     x25, x26, [sp, 64]
        stp     x27, x28, [sp, 80]
        stp     d8, d9, [sp, 96]
        stp     d10, d11, [sp, 112]
        stp     d12, d13, [sp, 128]
        stp     d14, d15, [sp, 144]
    /* Load domain state pointer from argument */
        mov     DOMAIN_STATE_PTR, TMP
    /* Reload allocation pointer */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Build (16-byte aligned) struct c_stack_link on the C stack */
        ldr     x8, Caml_state(c_stack)
        stp     x8, xzr, [sp, -16]! /* C_stack_prev, pad */
        CFI_ADJUST(16)
        stp     xzr, xzr, [sp, -16]! /* C_stack_stack, C_stack_sp */
        CFI_ADJUST(16)
        mov     x8, sp
        str     x8, Caml_state(c_stack)
    /* Load the OCaml stack */
        ldr     x8, Caml_state(current_stack)
        ldr     x8, Stack_sp(x8)
    /* Store the gc_regs for callbacks during a GC */
        ldr     x9, Caml_state(gc_regs)
        str     x9, [x8, -8]!
    /* Store the stack pointer to allow DWARF unwind */
        mov     x9, sp
        str     x9, [x8, -8]! /* C_stack_sp */
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        ldr     x9, Caml_state(exn_handler)
        adr     x10, L(trap_handler)
        stp     x9, x10, [x8, -16]!
        mov     TRAP_PTR, x8
    /* Switch stacks and call the OCaml code */
        mov     sp, x8
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        .cfi_escape DW_CFA_def_cfa_expression, 3 + 2 + 2,             \
            /* sp points to the exn handler on the OCaml stack */     \
            /* sp + 16 contains the C_STACK_SP */                     \
          DW_OP_breg + DW_REG_sp, 16 /* exn handler */, DW_OP_deref,  \
            /* 32   struct c_stack_link + pad */                      \
            /* 18*8 callee save regs */                               \
            /* 16   fp + ret addr */                                  \
            /* need to split to get under 127 limit */                \
          DW_OP_plus_uconst, 96, DW_OP_plus_uconst, 96
#endif
    /* Call the OCaml code */
        blr     TMP2
L(caml_retaddr):
    /* Pop the trap frame, restoring Caml_state->exn_handler */
        ldr     x8, [sp], 16
        CFI_ADJUST(-16)
        str     x8, Caml_state(exn_handler)
L(return_result):
    /* restore GC regs */
        ldp     x8, x9, [sp], 16
        CFI_ADJUST(-16)
        str     x9, Caml_state(gc_regs)
    /* Update allocation pointer */
        str     ALLOC_PTR, Caml_state(young_ptr)
    /* Return to C stack */
        ldr     x8, Caml_state(current_stack)
        mov     x9, sp
        str     x9, Stack_sp(x8)
        ldr     x9, Caml_state(c_stack)
        mov     sp, x9
        CFI_RESTORE_STATE
    /* Pop the struct c_stack_link */
        ldr     x8, Cstack_prev(sp)
        add     sp, sp, 32
        CFI_ADJUST(-32)
        str     x8, Caml_state(c_stack)
    /* Reload callee-save registers and return address */
        ldp     x19, x20, [sp, 16]
        ldp     x21, x22, [sp, 32]
        ldp     x23, x24, [sp, 48]
        ldp     x25, x26, [sp, 64]
        ldp     x27, x28, [sp, 80]
        ldp     d8, d9, [sp, 96]
        ldp     d10, d11, [sp, 112]
        ldp     d12, d13, [sp, 128]
        ldp     d14, d15, [sp, 144]
        ldp     x29, x30, [sp], 160
        CFI_ADJUST(-160)
    /* Return to C caller */
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_start_program)

/* The trap handler */

        .align  2
L(trap_handler):
        CFI_STARTPROC
    /* Save exception pointer */
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Encode exception bucket as an exception result */
        orr     x0, x0, #2
    /* Return it */
        b       L(return_result)
        CFI_ENDPROC

/* Exceptions */

.macro JUMP_TO_TRAP_PTR
    /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
.endm

/* Raise an exception from OCaml */
FUNCTION(caml_raise_exn)
        CFI_STARTPROC
    /* Test if backtrace is active */
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, 2f
1:
        JUMP_TO_TRAP_PTR
2:  /* Zero backtrace_pos */
        str     xzr, Caml_state(backtrace_pos)
L(caml_reraise_exn_stash):
    /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                               /* arg1: exn bucket, already in x0 */
        mov     x1, x30        /* arg2: pc of raise */
        mov     x2, sp         /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
    /* Switch to C stack */
        ldr     TMP, Caml_state(c_stack)
        mov     sp, TMP
        bl      G(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        END_FUNCTION(caml_raise_exn)

FUNCTION(caml_reraise_exn)
        CFI_STARTPROC
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, L(caml_reraise_exn_stash)
        JUMP_TO_TRAP_PTR
        CFI_ENDPROC
        END_FUNCTION(caml_reraise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
    /* Load the domain state ptr */
        mov     DOMAIN_STATE_PTR, C_ARG_1
    /* Load the exception bucket */
        mov     x0, C_ARG_2
    /* Reload trap ptr and alloc ptr */
        ldr     TRAP_PTR, Caml_state(exn_handler)
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Discard the C stack pointer and reset to ocaml stack */
        ldr     TMP, Caml_state(current_stack)
        ldr     TMP, Stack_sp(TMP)
        mov     sp, TMP
    /* Restore frame and link on return to OCaml */
        ldp     x29, x30, [sp], 16
        b       G(caml_raise_exn)
        CFI_ENDPROC
        END_FUNCTION(caml_raise_exception)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = first arg) */
        mov     TMP, x0
        ldr     x0, [x2]        /* x0 = first arg */
                                /* x1 = closure environment */
        ldr     TMP2, [x1]       /* code pointer */
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2) */
        mov     TMP, x0
        mov     TMP2, x1
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        mov     x2, TMP2         /* x2 = closure environment */
        ADDRGLOBAL(TMP2, caml_apply2)
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2,
        [x2,16] = arg3) */
        mov     TMP, x0
        mov     x3, x1          /* x3 = closure environment */
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        ldr     x2, [x2, 16]    /* x2 = third arg */
        ADDRGLOBAL(TMP2, caml_apply3)
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback3_asm)

/* Fibers */

/* Switch between OCaml stacks. Clobbers TMP and switches TRAP_PTR
   Preserves old_stack and new_stack registers */
.macro SWITCH_OCAML_STACKS old_stack, new_stack
    /* Save frame pointer and return address for old_stack */
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
    /* Save OCaml SP and exn_handler in the stack info */
        mov     TMP, sp
        str     TMP, Stack_sp(\old_stack)
        str     TRAP_PTR, Stack_exception(\old_stack)
    /* switch stacks */
        str     \new_stack, Caml_state(current_stack)
        ldr     TMP, Stack_sp(\new_stack)
        mov     sp, TMP
    /* restore exn_handler for new stack */
        ldr     TRAP_PTR, Stack_exception(\new_stack)
    /* Restore frame pointer and return address for new_stack */
        ldp     x29, x30, [sp], 16
.endm


/*
 * A continuation is a one word object that points to a fiber. A fiber [f] will
 * point to its parent at Handler_parent(Stack_handler(f)). In the following,
 * the [last_fiber] refers to the last fiber in the linked-list formed by the
 * parent pointer.
 */

FUNCTION(caml_perform)
        CFI_STARTPROC
    /*  x0: effect to perform
        x1: freshly allocated continuation */
        ldr     x2, Caml_state(current_stack) /* x2 := old stack */
        add     x3, x2, 1 /* x3 := Val_ptr(old stack) */
        str     x3, [x1] /* Iniitalize continuation */
L(do_perform):
    /*  x0: effect to perform
        x1: continuation
        x2: old_stack
        x3: last_fiber */

        ldr     x9, Stack_handler(x2)  /* x9 := old stack -> handler */
        ldr     x10, Handler_parent(x9) /* x10 := parent stack */
        cbz     x10, 1f
        SWITCH_OCAML_STACKS x2, x10
    /*  we have to null the Handler_parent after the switch because
        the Handler_parent is needed to unwind the stack for backtraces */
        str     xzr, Handler_parent(x9) /* Set parent of performer to NULL */
        ldr     TMP, Handler_effect(x9)
        mov     x2, x3                 /* x2 := last_fiber */
        mov     x3, TMP                /* x3 := effect handler */
        b       G(caml_apply3)
1:
    /*  switch back to original performer before raising Unhandled_effect
        (no-op unless this is a reperform) */
        ldr     x10, [x1] /* load performer stack from continuation */
        sub     x10, x10, 1 /* x10 := Ptr_val(x10) */
        ldr     x9, Caml_state(current_stack)
        SWITCH_OCAML_STACKS x9, x10
    /*  No parent stack. Raise Unhandled_effect. */
        ADDRGLOBAL(ADDITIONAL_ARG, caml_raise_unhandled_effect)
        b       G(caml_c_call)
        CFI_ENDPROC
        END_FUNCTION(caml_perform)

FUNCTION(caml_reperform)
        CFI_STARTPROC
    /*  x0: effect to perform
        x1: continuation
        x2: last_fiber */
        ldr     TMP, Stack_handler_from_cont(x2)
        ldr     x2, Caml_state(current_stack) /* x2 := old stack */
        str     x2, Handler_parent(TMP) /* Append to last_fiber */
        add     x3, x2, 1 /* x3 (last_fiber) := Val_ptr(old stack) */
        b       L(do_perform)
        CFI_ENDPROC
        END_FUNCTION(caml_reperform)

FUNCTION(caml_resume)
CFI_STARTPROC
    /*  x0: new fiber
        x1: fun
        x2: arg */
        sub     x0, x0, 1 /* x0 = Ptr_val(x0) */
        ldr     x3, [x1]  /* code pointer */
    /* Check if stack null, then already used */
        cbz     x0, 2f
    /* Find end of list of stacks (put in x8) */
        mov     TMP, x0
1:      ldr     x8, Stack_handler(TMP)
        ldr     TMP, Handler_parent(x8)
        cbnz    TMP, 1b
    /* Add current stack to the end */
        ldr     x9, Caml_state(current_stack)
        str     x9, Handler_parent(x8)
        SWITCH_OCAML_STACKS x9, x0
        mov     x0, x2
        br      x3
2:      ADDRGLOBAL(ADDITIONAL_ARG, caml_raise_continuation_already_taken)
        b       G(caml_c_call)
        CFI_ENDPROC
        END_FUNCTION(caml_resume)

/* Run a function on a new stack, then either
   return the value or invoke exception handler */
FUNCTION(caml_runstack)
CFI_STARTPROC
    /*  x0: fiber
        x1: fun
        x2: arg */
        CFI_OFFSET(29, -16)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp, -16]!
        CFI_ADJUST(16)
        add     x29, sp, #0
        sub     x0, x0, 1  /* x0 := Ptr_val(x0) */
        ldr     x3, [x1]   /* code pointer */
    /*  save old stack pointer and exception handler */
        ldr     x8, Caml_state(current_stack) /* x8 := old stack */
        mov     TMP, sp
        str     TMP, Stack_sp(x8)
        str     TRAP_PTR, Stack_exception(x8)
    /* Load new stack pointer and set parent */
        ldr     TMP, Stack_handler(x0)
        str     x8, Handler_parent(TMP)
        str     x0, Caml_state(current_stack)
        ldr     x9, Stack_sp(x0) /* x9 := sp of new stack */
    /* Create an exception handler on the target stack
       after 16byte DWARF & gc_regs block (which is unused here) */
        sub     x9, x9, 32
        adr     TMP, L(fiber_exn_handler)
        str     TMP, [x9, 8]
    /* link the previous exn_handler so that copying stacks works */
        ldr     TMP, Stack_exception(x0)
        str     TMP, [x9]
        mov     TRAP_PTR, x9
    /* Switch to the new stack */
        mov     sp, x9
#ifdef ASM_CFI_SUPPORTED
        CFI_REMEMBER_STATE
        .cfi_escape DW_CFA_def_cfa_expression, 3+3+2,       \
          DW_OP_breg + DW_REG_sp,                           \
            16 /* exn */ +                                  \
            8 /* gc_regs slot (unused) */ +                 \
            8 /* C_STACK_SP for DWARF (unused) */           \
            + Handler_parent_offset, DW_OP_deref,           \
          DW_OP_plus_uconst, Stack_sp_offset, DW_OP_deref,  \
          DW_OP_plus_uconst, 16 /* fp + ret addr */
#endif
    /* Call the function on the new stack */
        mov     x0, x2
        blr     x3
L(frame_runstack):
        add     x8, sp, 32 /* x8 := stack_handler */
        ldr     x19, Handler_value(x8) /* saved across C call */
1:
        mov     x20, x0     /* save return across C call */
        ldr     x0, Caml_state(current_stack) /* arg to caml_free_stack */
    /* restore parent stack and exn_handler into Caml_state */
        ldr     TMP, Handler_parent(x8)
        str     TMP, Caml_state(current_stack)
        ldr     TRAP_PTR, Stack_exception(TMP)
        str     TRAP_PTR, Caml_state(exn_handler)
    /* free old stack by switching directly to c_stack;
       is a no-alloc call */
        ldr     x21, Stack_sp(TMP) /* saved across C call */
        CFI_RESTORE_STATE
        CFI_REMEMBER_STATE
        CFI_DEF_CFA_REGISTER(DW_REG_x21)
        ldr     TMP, Caml_state(c_stack)
        mov     sp, TMP
        bl      G(caml_free_stack)
    /* switch directly to parent stack with correct return */
        mov     x0, x20
        mov     x1, x19
        mov     sp, x21
        CFI_RESTORE_STATE
        ldr     TMP, [x19]  /* code pointer */
    /* Invoke handle_value (or handle_exn) */
        ldp     x29, x30, [sp], 16
        CFI_ADJUST(-16)
        br      TMP
L(fiber_exn_handler):
        add     x8, sp, 16  /* x8 := stack_handler */
        ldr     x19, Handler_exception(x8)
        b       1b
        CFI_ENDPROC
        END_FUNCTION(caml_runstack)

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
    /* Load address of [caml_array_bound_error_asm] in ADDITIONAL_ARG */
        ADDRGLOBAL(ADDITIONAL_ARG, caml_array_bound_error_asm)
    /* Call that function */
        b       G(caml_c_call)
        CFI_ENDPROC
        END_FUNCTION(caml_ml_array_bound_error)

         TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

/* GC roots for callback */

OBJECT(caml_system.frametable)
        .quad   2               /* two descriptors */
        .quad   L(caml_retaddr) /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .quad   L(frame_runstack) /* return address into fiber handler */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots here */
        .align 3
        END_OBJECT(caml_system.frametable)

#if !defined(SYS_macosx)
/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
#endif
