/**************************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*     en Automatique.                                                    */
/*                                                                     */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, ARM processor, 64-bit mode */
/* Must be preprocessed by cpp */

/* Special registers */

#define TRAP_OFF x26
#define ALLOC_PTR x27
#define ALLOC_LIMIT x28
#define ARG x15
#define TMP x16
#define TMP2 x17

/* Support for CFI directives */

#if defined(ASM_CFI_SUPPORTED)
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#endif

#define FUNCTION(name) \
        .globl name; \
        .align 2; \
        name:

/* Support for profiling with gprof */

#define PROFILE

/******************************************************************************/
/* Access to the current domain state block. */
/******************************************************************************/

#define CAML_CONFIG_H_NO_TYPEDEFS
#include "../byterun/caml/config.h"

#define GET_DOMAIN_STATE(reg) \
        and     reg, ALLOC_PTR, (0xffffffffffffffff << Minor_heap_align_bits)

/* CR mshinwell: We should optimize the case where there are multiple
   loads/stores in one go to the domain state block. */

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../byterun/caml/domain_state.tbl"
#undef DOMAIN_STATE

/* Load from the current domain state block.  Clobbers only the destination. */
#define LOAD_TL_VAR(var,dstreg) \
        GET_DOMAIN_STATE(dstreg) ; \
        ldr     dstreg, [dstreg, (8 * domain_field_##var)]

/* Store to the current domain state block.  Clobbers TMP. */
#define STORE_TL_VAR(srcreg, var) \
        GET_DOMAIN_STATE(TMP)   ; \
        str     srcreg, [TMP, (8 * domain_field_##var)]

/* Push pair of values from current domain state block to the stack. Clobbers
   TMP. */
#define PUSH_TL_PAIR(var1,var2) \
        LOAD_TL_VAR(var2, TMP); \
        str     TMP, [sp, -16]!; \
        CFI_ADJUST(16); \
        LOAD_TL_VAR(var1, TMP); \
        str     TMP, [sp, 8]

/* Pop pair of values from the stack and store in the current domain state
   block. Clobbers TMP and TMP2. */
#define POP_TL_PAIR(var1,var2) \
        ldr     TMP2, [sp], 16; \
        CFI_ADJUST(-16); \
        STORE_TL_VAR(TMP2, var2); \
        ldr     TMP2,[sp, -8]; \
        STORE_TL_VAR(TMP2, var1)


/******************************************************************************/
/* Macros to load and store global variables.  Destroy TMP2 */
/******************************************************************************/

#if defined(__PIC__)

#define ADDRGLOBAL(reg,symb) \
        adrp    TMP2, :got:symb; \
        ldr     reg, [TMP2, #:got_lo12:symb]

#else

#define ADDRGLOBAL(reg,symb) \
        adrp    reg, symb; \
        add     reg, reg, #:lo12:symb

#endif

/******************************************************************************/
/* Stack switching operations */
/******************************************************************************/

/* Switch from OCaml to C stack. Clobbers REG & TRAP_OFF. */
#define SWITCH_OCAML_TO_C_NO_CTXT(REG) \
    /* Save OCaml SP in the stack slot */ \
        LOAD_TL_VAR(caml_stack_high, TRAP_OFF); \
        sub     TRAP_OFF, sp, TRAP_OFF; \
        asr     TRAP_OFF, TRAP_OFF, #3; \
        LOAD_TL_VAR(caml_current_stack, REG); \
        str     TRAP_OFF, [REG]; \
    /* Switch to system stack */ \
        LOAD_TL_VAR(caml_system_sp, REG); \
        mov     sp, REG

/* Switch from OCaml to C stack. Also builds a context at
 * the bottom of the OCaml stack. Clobbers REG & TRAP_OFF. */
#define SWITCH_OCAML_TO_C(REG) \
    /* Build caml_context at the bottom of the stack */ \
        str     TRAP_OFF, [sp, -32]!; /* save trap_off */ \
        CFI_ADJUST(24); \
        str     x30, [sp, 24];        /* return address */ \
        str     xzr, [sp, 8];         /* gc_regs_slot <- 0 */ \
        SWITCH_OCAML_TO_C_NO_CTXT(REG)

/* Switch from C to OCaml stack.  Clobbers REG. */
#define SWITCH_C_TO_OCAML_NO_CTXT(REG) \
    /* Switch to OCaml stack */ \
        LOAD_TL_VAR(caml_stack_high, REG); \
        mov     sp, REG; \
        LOAD_TL_VAR(caml_current_stack, REG); \
    /* REG is Stack_sp(caml_current_stack) */ \
        ldr     REG, [REG]; \
        add     sp, sp, REG, LSL 3

/* Switch from C to OCaml stack. Also pops the context
 * from the bottom of the OCaml stack. Clobbers REG. */
#define SWITCH_C_TO_OCAML(REG) \
        SWITCH_C_TO_OCAML_NO_CTXT(REG); \
    /* Pop the caml_context from the bottom of stack updating TRAP_OFF and return address. */ \
        ldr     x30, [sp, 24]; \
        ldr     TRAP_OFF, [sp], 32

/* Restore exception handler for OCaml code. Cuts stack, and loads target address in TMP. */
#define RESTORE_EXN_HANDLER_OCAML \
        LOAD_TL_VAR(caml_stack_high, TMP); \
        sub     sp, TMP, TRAP_OFF; \
        ldr     TMP, [sp, 8]; \
        ldr     TRAP_OFF, [sp], 16

/* Load [caml_system_stack_high - TRAP_OFF] into sp. Cuts the stack, and loads the target address in TMP. */
#define RESTORE_EXN_HANDLER_SYS \
        LOAD_TL_VAR(caml_system_stack_high, TMP); \
        sub     sp, TMP, TRAP_OFF; \
        ldr     TMP, [sp, 8]; \
        ldr     TRAP_OFF, [sp], 16

/* Switch between OCaml stacks.
 * arguments : target stack (x0) */
#define SWITCH_OCAML_STACKS \
      /* Switch from OCaml to C */ \
        SWITCH_OCAML_TO_C(TMP); \
        ADDRGLOBAL(ARG,caml_switch_stack); \
        blr     ARG; \
        SWITCH_C_TO_OCAML(TMP)

/* Stack-local variables */

#define Stack_sp(REG)               [REG]
#define Stack_dirty(REG)            [REG, 8]
#define Stack_handle_value(REG)     [REG, 16]
#define Stack_handle_exception(REG) [REG, 24]
#define Stack_handle_effect(REG)    [REG, 32]
#define Stack_parent(REG)           [REG, 40]

/******************************************************************************/
/* Allocation functions and GC interface */
/******************************************************************************/

        .globl  caml_system__code_begin
caml_system__code_begin:

#define SAVE_ALL_REGS_AND_CALL(TARGET_FUN)  \
    /* Set up stack space, saving return address and frame pointer */ \
    /* (2 regs RA/GP, 24 allocatable int regs, 24 caller-save float regs) * 8 */ \
        stp     x29, x30, [sp, -400]!; \
        CFI_ADJUST(400); \
        add     x29, sp, 0 ; \
    /* Save allocatable integer registers on the stack, in the order given in proc.ml */ \
        stp     x0, x1, [sp, 16]; \
        stp     x2, x3, [sp, 32]; \
        stp     x4, x5, [sp, 48]; \
        stp     x6, x7, [sp, 64]; \
        stp     x8, x9, [sp, 80]; \
        stp     x10, x11, [sp, 96]; \
        stp     x12, x13, [sp, 112]; \
        stp     x14, x15, [sp, 128]; \
        stp     x19, x20, [sp, 144]; \
        stp     x21, x22, [sp, 160]; \
        stp     x23, x24, [sp, 176]; \
        str     x25, [sp, 192]; \
     /* Save caller-save floating-point registers on the stack. */ \
     /* (callee-saves are preserved by caml_garbage_collection) */ \
        stp     d0, d1, [sp, 208]; \
        stp     d2, d3, [sp, 224]; \
        stp     d4, d5, [sp, 240]; \
        stp     d6, d7, [sp, 256]; \
        stp     d16, d17, [sp, 272]; \
        stp     d18, d19, [sp, 288]; \
        stp     d20, d21, [sp, 304]; \
        stp     d22, d23, [sp, 320]; \
        stp     d24, d25, [sp, 336]; \
        stp     d26, d27, [sp, 352]; \
        stp     d28, d29, [sp, 368]; \
        stp     d30, d31, [sp, 384]; \
    /* Save current allocation pointer */ \
        STORE_TL_VAR(ALLOC_PTR, caml_young_ptr); \
    /* Update in OCaml stack's context, the pointer to gc_regs. */ \
    /* Store pointer to saved integer registers in caml_gc_regs */ \
        add     x0, sp, 16 ; \
        bl      caml_update_gc_regs_slot; \
    /* Args are relevant only for caml_realloc_stack */ \
        mov     x0, 0; \
        mov     x1, 0; \
        mov     x2, 0; \
        bl      TARGET_FUN; \
    /* Restore registers */ \
        ldp     x0, x1, [sp, 16]; \
        ldp     x2, x3, [sp, 32]; \
        ldp     x4, x5, [sp, 48]; \
        ldp     x6, x7, [sp, 64]; \
        ldp     x8, x9, [sp, 80]; \
        ldp     x10, x11, [sp, 96]; \
        ldp     x12, x13, [sp, 112]; \
        ldp     x14, x15, [sp, 128]; \
        ldp     x19, x20, [sp, 144]; \
        ldp     x21, x22, [sp, 160]; \
        ldp     x23, x24, [sp, 176]; \
        ldr     x25, [sp, 192]; \
        ldp     d0, d1, [sp, 208]; \
        ldp     d2, d3, [sp, 224]; \
        ldp     d4, d5, [sp, 240]; \
        ldp     d6, d7, [sp, 256]; \
        ldp     d16, d17, [sp, 272]; \
        ldp     d18, d19, [sp, 288]; \
        ldp     d20, d21, [sp, 304]; \
        ldp     d22, d23, [sp, 320]; \
        ldp     d24, d25, [sp, 336]; \
        ldp     d26, d27, [sp, 352]; \
        ldp     d28, d29, [sp, 368]; \
        ldp     d30, d31, [sp, 384]; \
    /* Reload new allocation pointer and allocation limit */ \
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR); \
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT); \
    /* Free stack space and return to caller */ \
        ldp     x29, x30, [sp], 400

FUNCTION(caml_call_gc)
CFI_STARTPROC
        PROFILE
        SWITCH_OCAML_TO_C(TMP)
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
        SWITCH_C_TO_OCAML(TMP)
        ret
CFI_ENDPROC
        .type   caml_call_gc, %function
        .size   caml_call_gc, .-caml_call_gc

FUNCTION(caml_call_realloc_stack)
CFI_STARTPROC
        PROFILE
        SWITCH_OCAML_TO_C(TMP)
        SAVE_ALL_REGS_AND_CALL(caml_realloc_stack)
        SWITCH_C_TO_OCAML(TMP)
        ret
CFI_ENDPROC
        .type   caml_call_realloc_stack, %function
        .size   caml_call_realloc_stack, .-caml_call_realloc_stack


/* Arguments: x0 = base; x1 = offset. */
FUNCTION(caml_call_read_barrier)
CFI_STARTPROC
        PROFILE
        SWITCH_OCAML_TO_C(TMP)
=======
    /* Record return address */
        STOREGLOBAL(x30, caml_last_return_address)
    /* Record lowest stack address */
        mov     TMP, sp
        STOREGLOBAL(TMP, caml_bottom_of_stack)
.Lcaml_call_gc:
>>>>>>> b4.04.2
    /* Set up stack space, saving return address and frame pointer */
    /* (2 regs RA/GP, 24 allocatable int regs, 24 caller-save float regs) * 8 */
        stp     x29, x30, [sp, -400]!
        CFI_ADJUST(400)
        add     x29, sp, 0
    /* Save allocatable integer registers on the stack, in the order given in proc.ml */
        stp     x0, x1, [sp, 16]
        stp     x2, x3, [sp, 32]
        stp     x4, x5, [sp, 48]
        stp     x6, x7, [sp, 64]
        stp     x8, x9, [sp, 80]
        stp     x10, x11, [sp, 96]
        stp     x12, x13, [sp, 112]
        stp     x14, x15, [sp, 128]
        stp     x19, x20, [sp, 144]
        stp     x21, x22, [sp, 160]
        stp     x23, x24, [sp, 176]
        str     x25, [sp, 192]
     /* Save caller-save floating-point registers on the stack. */
     /* (callee-saves are preserved by caml_garbage_collection) */
        stp     d0, d1, [sp, 208]
        stp     d2, d3, [sp, 224]
        stp     d4, d5, [sp, 240]
        stp     d6, d7, [sp, 256]
        stp     d16, d17, [sp, 272]
        stp     d18, d19, [sp, 288]
        stp     d20, d21, [sp, 304]
        stp     d22, d23, [sp, 320]
        stp     d24, d25, [sp, 336]
        stp     d26, d27, [sp, 352]
        stp     d28, d29, [sp, 368]
        stp     d30, d31, [sp, 384]
    /* Save current allocation pointer */
        STORE_TL_VAR(ALLOC_PTR, caml_young_ptr)
    /* Update in OCaml stack's context, the pointer to gc_regs. */
    /* Store pointer to saved integer registers in caml_gc_regs */
        add     x0, sp, 16
        bl      caml_update_gc_regs_slot
    /* Args are relevant only for caml_realloc_stack */
        ldp     x0, x1, [sp, 16]       /* Load C arguments saved in stack */
        bl      caml_read_barrier
    /* Result is in x0. Restore other registers. */
        ldp     x2, x3, [sp, 32]
        ldp     x4, x5, [sp, 48]
        ldp     x6, x7, [sp, 64]
        ldp     x8, x9, [sp, 80]
        ldp     x10, x11, [sp, 96]
        ldp     x12, x13, [sp, 112]
        ldp     x14, x15, [sp, 128]
        ldp     x19, x20, [sp, 144]
        ldp     x21, x22, [sp, 160]
        ldp     x23, x24, [sp, 176]
        ldr     x25, [sp, 192]
        ldp     d0, d1, [sp, 208]
        ldp     d2, d3, [sp, 224]
        ldp     d4, d5, [sp, 240]
        ldp     d6, d7, [sp, 256]
        ldp     d16, d17, [sp, 272]
        ldp     d18, d19, [sp, 288]
        ldp     d20, d21, [sp, 304]
        ldp     d22, d23, [sp, 320]
        ldp     d24, d25, [sp, 336]
        ldp     d26, d27, [sp, 352]
        ldp     d28, d29, [sp, 368]
        ldp     d30, d31, [sp, 384]
    /* Reload new allocation pointer and allocation limit */
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR)
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT)
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp], 400
        SWITCH_C_TO_OCAML(TMP)
        ret
CFI_ENDPROC
        .type   caml_call_read_barrier, %function
        .size   caml_call_read_barrier, .-caml_call_read_barrier

/* TODO KC: define MK_CAML_ALLOC(SZ) macro for generating caml_allocX functions. */

FUNCTION(caml_alloc1)
CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, 16
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      SWITCH_OCAML_TO_C(TMP)
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
        SWITCH_C_TO_OCAML(TMP)
    /* Try again */
        b       1b
CFI_ENDPROC
        .type   caml_alloc1, %function
        .size   caml_alloc1, .-caml_alloc1

FUNCTION(caml_alloc2)
CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, 24
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      SWITCH_OCAML_TO_C(TMP)
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
        SWITCH_C_TO_OCAML(TMP)
    /* Try again */
        b       1b
CFI_ENDPROC
        .type   caml_alloc2, %function
        .size   caml_alloc2, .-caml_alloc2

FUNCTION(caml_alloc3)
CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, 32
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      SWITCH_OCAML_TO_C(TMP)
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
        SWITCH_C_TO_OCAML(TMP)
    /* Try again */
        b       1b
CFI_ENDPROC
        .type   caml_alloc3, %function
        .size   caml_alloc3, .-caml_alloc3

FUNCTION(caml_allocN)
CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, ARG
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      SWITCH_OCAML_TO_C(TMP)
    /* Call GC.  This preserves ARG */
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
        SWITCH_C_TO_OCAML(TMP)
    /* Try again */
        b       1b
CFI_ENDPROC
        .type   caml_allocN, %function
        .size   caml_allocN, .-caml_allocN

/******************************************************************************/
/* Call a C function from OCaml */
/* Function to call is in ARG */
/******************************************************************************/

FUNCTION(caml_c_call)
CFI_STARTPROC
    /* Arguments:
        C arguments : x0 to x7, d0 to d7
        C function  : ARG */
        PROFILE
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C(TMP)
    /* Make alloc ptr available to the C code */
        STORE_TL_VAR(ALLOC_PTR, caml_young_ptr)
    /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR)
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML(TMP)
    /* Return */
        ret
CFI_ENDPROC
        .type   caml_c_call, %function
        .size   caml_c_call, .-caml_c_call

FUNCTION(caml_c_call_stack_args)
CFI_STARTPROC
    /* Arguments:
        C arguments  : x0 to x7, d0 to d7
        C function   : ARG
        C stack args : begin=x19 end=x20 */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C(TMP)
    /* Make the exception handler alloc ptr available to the C code */
        STORE_TL_VAR(ALLOC_PTR, caml_young_ptr)
    /* Copy arguments from OCaml to C stack */
        sub     TMP, x20, x19
        mov     TMP2, sp
        sub     sp, sp, TMP
1:      sub     x20, x20, 8
        cmp     x20, x19
        b.lo    2f
        ldr     TMP, [x20]
        str     TMP, [TMP2, -8]!; CFI_ADJUST(8)
        b       1b
2:  /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR)
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML(TMP)
    /* Return */
        ret
CFI_ENDPROC
        .type   caml_c_call_stack_args, %function
        .size   caml_c_call_stack_args, .-caml_c_call_stack_args

/******************************************************************************/
/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */
/******************************************************************************/

#define PUSH_CALLEE_SAVE_REGS \
        stp     x29, x30, [sp, -160]!; \
        CFI_ADJUST(160); \
        add     x29, sp, 0 ; \
        stp     x19, x20, [sp, 16]; \
        stp     x21, x22, [sp, 32]; \
        stp     x23, x24, [sp, 48]; \
        stp     x25, x26, [sp, 64]; \
        stp     x27, x28, [sp, 80]; \
        stp     d8, d9, [sp, 96]; \
        stp     d10, d11, [sp, 112]; \
        stp     d12, d13, [sp, 128]; \
        stp     d14, d15, [sp, 144]

#define POP_CALLEE_SAVE_REGS \
        ldp     x19, x20, [sp, 16]; \
        ldp     x21, x22, [sp, 32]; \
        ldp     x23, x24, [sp, 48]; \
        ldp     x25, x26, [sp, 64]; \
        ldp     x27, x28, [sp, 80]; \
        ldp     d8, d9, [sp, 96]; \
        ldp     d10, d11, [sp, 112]; \
        ldp     d12, d13, [sp, 128]; \
        ldp     d14, d15, [sp, 144]; \
        ldp     x29, x30, [sp], 160; \
        CFI_ADJUST(-160)


/******************************************************************************/
/* Start the OCaml program */
/******************************************************************************/

FUNCTION(caml_start_program)
CFI_STARTPROC
        PROFILE
        PUSH_CALLEE_SAVE_REGS
    /* Load caml_young_ptr into ALLOC_PTR (was passed as an argument from C) */
        mov     ALLOC_PTR, x0
    /* Initial entry point is caml_program */
        ADDRGLOBAL(ARG, caml_program)
    /* Common code for caml_start_program and caml_callback* */
    /* Arguments to the OCaml code are in x0...x3 */
.Ljump_to_caml:
    /* Save system stack state. */
        PUSH_TL_PAIR(caml_system_exnptr_offset, caml_system_sp)
    /* Save parent stack reference and reset it. This ensures that callbacks do
     * not inherit the handler from the previous stack chunk. */
        LOAD_TL_VAR(caml_current_stack, TMP)
        ldr     TMP2, Stack_parent(TMP)
        str     TMP2, [sp, -16]!        /* 16-byte alignment */
        CFI_ADJUST(16)
        mov     TMP2, 1
        str     TMP2, Stack_parent(TMP)
    /* Build a handler for exceptions raised in C */
        adr     TMP, .Lc_trap_handler
        stp     xzr, TMP, [sp, -16]!    /* xzr => dummy previous trap */
        CFI_ADJUST(16)
        mov     TMP2, sp
        STORE_TL_VAR(TMP2, caml_system_sp)
        LOAD_TL_VAR(caml_system_stack_high, TRAP_OFF)
        sub     TRAP_OFF, TRAP_OFF, TMP2
        STORE_TL_VAR(TRAP_OFF, caml_system_exnptr_offset)
    /*  Save callback args */
        stp     x29, x30, [sp, -64]!
        CFI_ADJUST(64)
        add     x29, sp, 0
        stp     x0, x1, [sp, 16]
        stp     x2, x3, [sp, 32]
        stp     ARG, xzr, [sp, 48]
    /* Create a register array since caml_maybe_expand_stack may GC */
        add     x0, sp, 16
     /* Expand the stack if needed */
        ADDRGLOBAL(ARG, caml_maybe_expand_stack)
        blr     ARG
     /* Now caml_current_stack corresponds to the new stack. Still on C stack.
      * Restore callback args. */
        ldp     x0, x1, [sp, 16]
        ldp     x2, x3, [sp, 32]
        ldr     ARG, [sp, 48]
        ldp     x29, x30, [sp], 64
        CFI_ADJUST(-64)
     /* Switch from C to OCaml stack */
        SWITCH_C_TO_OCAML_NO_CTXT(TMP)
     /* Setup alloc ptr */
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR)
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT)
     /* Build a handler for exceptions raised in OCaml */
        adr   TMP, .Lcaml_trap_handler
        stp   TRAP_OFF, TMP, [sp, -16]!
        CFI_ADJUST(16)
        LOAD_TL_VAR(caml_stack_high, TRAP_OFF)
        mov   TMP, sp
        sub   TRAP_OFF, TRAP_OFF, TMP
    /* Call the OCaml code */
        blr   ARG
.Lcaml_retaddr:
    /* Pop the OCaml exception handler */
        ldr   TMP, [sp], 16
        CFI_ADJUST(-16)
.Lreturn_to_c:
    /* Update alloc ptr */
        STORE_TL_VAR(ALLOC_PTR, caml_young_ptr)
    /* Return to C stack */
        SWITCH_OCAML_TO_C_NO_CTXT(TMP)
    /* Pop the C exception handler */
        add     sp, sp, 16
        CFI_ADJUST(-16)
    /* Restore previous parent stack */
        LOAD_TL_VAR(caml_current_stack, TMP2)
        ldr   TMP, [sp], 16
        str   TMP, Stack_parent(TMP2)
    /* Restore previous system stack state */
        POP_TL_PAIR(caml_system_exnptr_offset, caml_system_sp)
    /* Restore the callee-saved registers */
        POP_CALLEE_SAVE_REGS
    /* Return to caller */
        ret
.Lcaml_trap_handler: /* 109 */
    /* Exception in OCaml code */
    /* Mark the bucket as an exception result and return it */
        orr     x0, x0, #2
        b       .Lreturn_to_c
.Lc_trap_handler: /* 115 */
    /* Exception in C code */
    /* Reinstall handler for exceptions raised in C */
        adr     TMP, .Lc_trap_handler
        stp     xzr, TMP, [sp, -16]!    /* xzr => dummy previous trap */
        CFI_ADJUST(16)
    /* Prepare for return to OCaml */
        LOAD_TL_VAR(caml_young_ptr, ALLOC_PTR)
        LOAD_TL_VAR(caml_young_limit, ALLOC_LIMIT)
    /* Load OCaml stack */
        SWITCH_C_TO_OCAML(TMP)
    /* Raise the exception in OCaml */
        b       .Lcaml_raise_exn
CFI_ENDPROC
        .type   .Lcaml_retaddr, %function
        .size   .Lcaml_retaddr, .-.Lcaml_retaddr
        .type   caml_start_program, %function
        .size   caml_start_program, .-caml_start_program

/******************************************************************************/
/* Exceptions */
/******************************************************************************/

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
CFI_STARTPROC
        PROFILE
.Lcaml_raise_exn:
    /* Test if backtrace is active */
        LOAD_TL_VAR(caml_backtrace_active, TMP)
        cbnz     TMP, 2f
1:  /* Cut stack at current trap handler */
        RESTORE_EXN_HANDLER_OCAML
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Preserve OCaml sp in callee-save register x20 */
        mov     x20, sp
    /* Stash the backtrace */
        LOAD_TL_VAR(caml_system_sp, TMP)
        mov     sp, TMP
                               /* arg1: exn bucket, already in x0 */
        mov     x1, x30        /* arg2: pc of raise */
        mov     x2, x20        /* arg3: sp of raise */
        mov     x3, TRAP_OFF   /* arg4: sp of handler */
        bl      caml_stash_backtrace
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
CFI_ENDPROC
        .type   caml_raise_exn, %function
        .size   caml_raise_exn, .-caml_raise_exn

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
CFI_STARTPROC
        PROFILE
        mov     ALLOC_PTR, x0
        mov     x0, x1
        LOAD_TL_VAR(caml_system_exnptr_offset, TRAP_OFF)
    /* Cut the stack at current trap handler */
        RESTORE_EXN_HANDLER_SYS
        br      TMP
CFI_ENDPROC
        .type   caml_raise_exception, %function
        .size   caml_raise_exception, .-caml_raise_exception

/******************************************************************************/
/* Callback from C to OCaml */
/******************************************************************************/

FUNCTION(caml_callback_asm)
CFI_STARTPROC
        PROFILE
        PUSH_CALLEE_SAVE_REGS
    /* Initial shuffling of arguments
       (x0 = alloc ptr, x1 = closure, x2 = first arg) */
        mov     ALLOC_PTR, x0   /* young ptr */
        mov     x0, x2          /* x0 = first arg */
                                /* x1 = closure environment already */
        ldr     ARG, [x1]       /* code pointer */
        b       .Ljump_to_caml
CFI_ENDPROC
        .type   caml_callback_asm, %function
        .size   caml_callback_asm, .-caml_callback_asm

FUNCTION(caml_callback2_asm)
CFI_STARTPROC
        PROFILE
        PUSH_CALLEE_SAVE_REGS
    /* Initial shuffling of arguments
       (x0 = alloc ptr, x1 = closure, x2 = arg1, x3 = arg2) */
        mov     ALLOC_PTR, x0   /* young ptr */
        mov     TMP, x1
        mov     x0, x2          /* x0 = first arg */
        mov     x1, x3          /* x1 = second arg */
        mov     x2, TMP         /* x2 = closure environment */
        ADDRGLOBAL(ARG, caml_apply2)
        b       .Ljump_to_caml
CFI_ENDPROC
        .type   caml_callback2_asm, %function
        .size   caml_callback2_asm, .-caml_callback2_asm

FUNCTION(caml_callback3_asm)
CFI_STARTPROC
        PROFILE
        PUSH_CALLEE_SAVE_REGS
    /* Initial shuffling of arguments
       (x0 = alloc ptr, x1 = closure, x2 = arg1, x3 = arg2, x4 = arg3) */
        mov     ALLOC_PTR, x0   /* young ptr */
        mov     TMP, x1
        ldr     x0, [x2]        /* x0 = first arg */
        ldr     x1, [x2,8]      /* x1 = second arg */
        ldr     x2, [x2,16]     /* x2 = third arg */
        mov     x3, TMP         /* x3 = closure environment */
        ADDRGLOBAL(ARG, caml_apply3)
        b       .Ljump_to_caml
CFI_ENDPROC
        .type   caml_callback3_asm, %function
        .size   caml_callback3_asm, .-caml_callback3_asm

/******************************************************************************/
/* Fibers */
/******************************************************************************/

FUNCTION(caml_fiber_exn_handler)
CFI_STARTPROC
    /* Save exception (x0) in callee-save regsiter x19 */
        mov     x19, x0
        LOAD_TL_VAR(caml_current_stack, x1)
        str     xzr, Stack_sp(x1)               /* zero SP */
        ldr     x20, Stack_handle_exception(x1) /* exception handler */
        ldr     x0, Stack_parent(x1)            /* parent stack. Never NULL here. */
    /* Reset stack */
        mov     TMP, 1
        str     TMP, Stack_handle_value(x1)
        str     TMP, Stack_handle_exception(x1)
        str     TMP, Stack_handle_effect(x1)
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke exception handler */
        mov     x0, x19       /* x0 = first argument */
        mov     x1, x20       /* x1 = closure environment */
        ldr     ARG, [x1]
        br      ARG
CFI_ENDPROC
        .type caml_fiber_exn_handler, %function
        .size caml_fiber_exn_handler, .-caml_fiber_exn_handler

FUNCTION(caml_fiber_val_handler)
CFI_STARTPROC
.Lfiber_val_handler:
    /* Save return value (x0) in callee-save register x19 */
        mov     x19, x0
        LOAD_TL_VAR(caml_current_stack, x1)
        str     xzr, Stack_sp(x1)             /* zero SP */
        ldr     x20, Stack_handle_value(x1)   /* value handler */
        ldr     x0, Stack_parent(x1)          /* parent stack. Never NULL here. */
    /* Reset stack. First pop off fiber exn handler. */
        add     sp, sp, 16
        CFI_ADJUST(-16)
        mov     TMP, 1
        str     TMP, Stack_handle_value(x1)
        str     TMP, Stack_handle_exception(x1)
        str     TMP, Stack_handle_effect(x1)
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke value handler */
        mov     x0, x19         /* x0 = first argument */
        mov     x1, x20         /* x1 = closure environment */
        ldr     ARG, [x1]
        br      ARG
CFI_ENDPROC
        .type caml_fiber_val_handler, %function
        .size caml_fiber_val_handler, .-caml_fiber_val_handler

FUNCTION(caml_perform)
CFI_STARTPROC
    /* Save effect (x0) in callee-saved register x19 */
        mov     x19, x0
        LOAD_TL_VAR(caml_current_stack, x20)
        ldr     x0, Stack_parent(x20)           /* Parent stack. */
        cmp     x0, 1                           /* Parent is NULL? */
        b.eq    1f
        ldr     x21, Stack_handle_effect(x20)   /* effect handler */
        mov     TMP, 1
        str     TMP, Stack_parent(x20)          /* Set parent stack of performer to NULL */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        mov     x0, x19           /* x0 = first argument */
        mov     x1, x20           /* x1 = second argument */
        mov     x2, x21           /* x2 = closure environment */
        ADDRGLOBAL(ARG, caml_apply2)
        br      ARG
1:  /* No parent stack. Raise Unhandled. */
        ADDRGLOBAL(x0, caml_exn_Unhandled)
        b       .Lcaml_raise_exn
CFI_ENDPROC
        .type caml_perform, %function
        .size caml_perform, .-caml_perform

FUNCTION(caml_reperform)
CFI_STARTPROC
    /* x0 -> effect (first argument), x1 -> performer */
        mov     x19, x0
        LOAD_TL_VAR(caml_current_stack, x2)
        ldr     x0, Stack_parent(x2)            /* Parent stack. */
        cmp     x0, 1                           /* Parent is NULL? */
        b.eq    1f
        str     x1, Stack_parent(x2)            /* Set performer as parent */
        mov     x20, x2                         /* Save current stack in callee-saved x20 */
        ldr     x21, Stack_handle_effect(x2)    /* Save effect handler in callee-saved x21 */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        mov     x0, x19           /* x0 = first argument */
        mov     x1, x20           /* x1 = second argument */
        mov     x2, x21           /* x2 = closure environment */
        ADDRGLOBAL(ARG, caml_apply2)
        br      ARG
1:  /* No parent stack. Raise Unhandled. */
        ADDRGLOBAL(x0, caml_exn_Unhandled)
        b       .Lcaml_raise_exn
CFI_ENDPROC
        .type caml_reperform, %function
        .size caml_reperform, .-caml_reperform

FUNCTION(caml_resume)
CFI_STARTPROC
    /* x0 -> tail of stack list, x1 -> fun, x2 -> arg */
        mov     x19, x2     /* Save arg in callee-saved x19 */
        mov     x20, x1     /* Save fun in callee-saved x20 */
        LOAD_TL_VAR(caml_current_stack, x3)
        mov     x2, x0
        /* x0 & x2 = tail */
.Lcaml_resume_loop:
    /* Rewind parent pointers to find the performer */
        mov     x0, x2
        ldr     x2, Stack_parent(x0)
        /* x0 = tail. x2 = parent(tail) */
        str     x3, Stack_parent(x0)
        /* Stack_parent(tail) <- x3. x0 = tail. x2 = parent(tail) */
        mov     x3, x0
        /* x3 & x0 = tail */
        cmp     x2, 1
        b.ne    .Lcaml_resume_loop
    /* x0 now has the desired head of stack list */
        SWITCH_OCAML_STACKS
        mov     x0, x19       /* x0 = first argument */
        mov     x1, x20       /* x1 = closure environment */
        ldr     ARG, [x1]
        br     ARG
CFI_ENDPROC
        .type caml_resume, %function
        .size caml_resume, .-caml_resume


FUNCTION(caml_ml_array_bound_error)
CFI_STARTPROC
        PROFILE
    /* Load address of [caml_array_bound_error] in ARG */
        ADDRGLOBAL(ARG, caml_array_bound_error)
    /* Call that function */
        b       caml_c_call
CFI_ENDPROC
        .type   caml_ml_array_bound_error, %function
        .size   caml_ml_array_bound_error, .-caml_ml_array_bound_error

        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

        .data
        .align  3
        .globl  caml_system__frametable
caml_system__frametable:
        .quad   2                   /* two descriptors */
        .quad   .Lcaml_retaddr      /* return address into callback */
        .short  -1                  /* negative frame size => use callback link */
        .short  0                   /* no roots */
        .align  3
        .quad   .Lfiber_val_handler /* return address into fiber_val_handler */
        .short  -1                  /* negative frame size => use callback link */
        .short  0                   /* no roots */
        .align  3
        .type   caml_system__frametable, %object
        .size   caml_system__frametable, .-caml_system__frametable

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
