/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2013 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
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

#define DOMAIN_STATE_PTR x25
#define TRAP_PTR x26
#define ALLOC_PTR x27
#define ALLOC_LIMIT x28
#define ARG x15
#define TMP x16
#define TMP2 x17
#define ARG_DOMAIN_STATE_PTR x18

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
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_REGISTER(r1,r2)
#define CFI_OFFSET(r,n)
#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) [x25, 8*domain_field_caml_##var]

#if defined(__PIC__)

#define ADDRGLOBAL(reg,symb) \
        adrp    TMP2, :got:symb; \
        ldr     reg, [TMP2, #:got_lo12:symb]
#else

#define ADDRGLOBAL(reg,symb) \
        adrp    reg, symb; \
        add     reg, reg, #:lo12:symb

#endif

#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  caml_hot__code_begin
caml_hot__code_begin:

        TEXT_SECTION(caml_hot__code_end)
        .globl  caml_hot__code_end
caml_hot__code_end:
#endif

#define FUNCTION(name) \
        TEXT_SECTION(caml.##name); \
        .align 2; \
        .globl name; \
        .type name, %function; \
name:

/* Allocation functions and GC interface */
        .globl  caml_system__code_begin
caml_system__code_begin:

FUNCTION(caml_call_gc)
        CFI_STARTPROC
.Lcaml_call_gc:
    /* Record return address */
        str     x30, Caml_state(last_return_address)
    /* Record lowest stack address */
        mov     TMP, sp
        str     TMP, Caml_state(bottom_of_stack)
    /* Set up stack space, saving return address and frame pointer */
    /* (2 regs RA/GP, 24 allocatable int regs, 24 caller-save float regs) * 8 */
        CFI_OFFSET(29, -400)
        CFI_OFFSET(30, -392)
        stp     x29, x30, [sp, -400]!
        CFI_ADJUST(400)
        add     x29, sp, #0
    /* Save allocatable integer registers on the stack, in the order
       given in proc.ml */
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
     /* Save caller-save floating-point registers on the stack
        (callee-saves are preserved by caml_garbage_collection) */
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
    /* Store pointer to saved integer registers in Caml_state->gc_regs */
        add     TMP, sp, #16
        str     TMP, Caml_state(gc_regs)
    /* Save current allocation pointer for debugging purposes */
        str     ALLOC_PTR, Caml_state(young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
        str     TRAP_PTR, Caml_state(exception_pointer)
    /* Call the garbage collector */
        bl      caml_garbage_collection
    /* Restore registers */
        ldp     x0, x1, [sp, 16]
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
        ldr     ALLOC_PTR, Caml_state(young_ptr)
        ldr     ALLOC_LIMIT, Caml_state(young_limit)
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp], 400
        ret
        CFI_ENDPROC
        .size   caml_call_gc, .-caml_call_gc

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    .Lcaml_call_gc
        ret
        CFI_ENDPROC
        .size   caml_alloc1, .-caml_alloc1

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    .Lcaml_call_gc
        ret
        CFI_ENDPROC
        .size   caml_alloc2, .-caml_alloc2

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    .Lcaml_call_gc
        ret
        CFI_ENDPROC
        .size   caml_alloc3, .-caml_alloc3

FUNCTION(caml_allocN)
        CFI_STARTPROC
        sub     ALLOC_PTR, ALLOC_PTR, ARG
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    .Lcaml_call_gc
        ret
        CFI_ENDPROC
        .size   caml_allocN, .-caml_allocN

/* Call a C function from OCaml */
/* Function to call is in ARG */

FUNCTION(caml_c_call)
        CFI_STARTPROC
    /* Preserve return address in callee-save register x19 */
        mov     x19, x30
        CFI_REGISTER(30, 19)
    /* Record lowest stack address and return address */
        str     x30, Caml_state(last_return_address)
        add     TMP, sp, #0
        str     TMP, Caml_state(bottom_of_stack)
    /* Make the exception handler alloc ptr available to the C code */
        str     ALLOC_PTR, Caml_state(young_ptr)
        str     TRAP_PTR, Caml_state(exception_pointer)
    /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
        ldr     ALLOC_LIMIT, Caml_state(young_limit)
    /* Return */
        ret     x19
        CFI_ENDPROC
        .size   caml_c_call, .-caml_c_call

/* Start the OCaml program */

FUNCTION(caml_start_program)
        CFI_STARTPROC
        mov     ARG_DOMAIN_STATE_PTR, C_ARG_1
        ADDRGLOBAL(ARG, caml_program)

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in ARG */
/* Arguments to the OCaml code are in x0...x7 */

.Ljump_to_caml:
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
        mov     DOMAIN_STATE_PTR, ARG_DOMAIN_STATE_PTR
    /* Setup a callback link on the stack */
        ldr     x8, Caml_state(bottom_of_stack)
        ldr     x9, Caml_state(last_return_address)
        ldr     x10, Caml_state(gc_regs)
        stp     x8, x9, [sp, -32]!     /* 16-byte alignment */
        CFI_ADJUST(32)
        str     x10, [sp, 16]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        ldr     x8, Caml_state(exception_pointer)
        adr     x9, .Ltrap_handler
        stp     x8, x9, [sp, -16]!
        CFI_ADJUST(16)
        add     TRAP_PTR, sp, #0
    /* Reload allocation pointers */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
        ldr     ALLOC_LIMIT, Caml_state(young_limit)
    /* Call the OCaml code */
        blr     ARG
.Lcaml_retaddr:
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     x8, [sp], 16
        CFI_ADJUST(-16)
        str     x8, Caml_state(exception_pointer)
    /* Pop the callback link, restoring the global variables */
.Lreturn_result:
        ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
        CFI_ADJUST(-32)
        str     x8, Caml_state(bottom_of_stack)
        str     x9, Caml_state(last_return_address)
        str     x10, Caml_state(gc_regs)
    /* Update allocation pointer */
        str     ALLOC_PTR, Caml_state(young_ptr)
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
        .type   .Lcaml_retaddr, %function
        .size   .Lcaml_retaddr, .-.Lcaml_retaddr
        .size   caml_start_program, .-caml_start_program

/* The trap handler */

        .align  2
.Ltrap_handler:
        CFI_STARTPROC
    /* Save exception pointer */
        str     TRAP_PTR, Caml_state(exception_pointer)
    /* Encode exception bucket as an exception result */
        orr     x0, x0, #2
    /* Return it */
        b       .Lreturn_result
        CFI_ENDPROC
        .type   .Ltrap_handler, %function
        .size   .Ltrap_handler, .-.Ltrap_handler

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        CFI_STARTPROC
    /* Test if backtrace is active */
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                               /* arg1: exn bucket, already in x0 */
        mov     x1, x30        /* arg2: pc of raise */
        add     x2, sp, #0     /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      caml_stash_backtrace
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        .size   caml_raise_exn, .-caml_raise_exn

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
    /* Load the domain state ptr */
        mov     DOMAIN_STATE_PTR, C_ARG_1
    /* Load the exception bucket */
        mov     x0, C_ARG_2
    /* Reload trap ptr, alloc ptr and alloc limit */
        ldr     TRAP_PTR, Caml_state(exception_pointer)
        ldr     ALLOC_PTR, Caml_state(young_ptr)
        ldr     ALLOC_LIMIT, Caml_state(young_limit)
    /* Test if backtrace is active */
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                                                      /* arg1: exn bucket */
        ldr     x1, Caml_state(last_return_address)   /* arg2: pc of raise */
        ldr     x2, Caml_state(bottom_of_stack)       /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      caml_stash_backtrace
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        .size   caml_raise_exception, .-caml_raise_exception

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = first arg) */
        mov     ARG_DOMAIN_STATE_PTR, x0
        ldr     x0, [x2]        /* x0 = first arg */
                                /* x1 = closure environment */
        ldr     ARG, [x1]       /* code pointer */
        b       .Ljump_to_caml
        CFI_ENDPROC
        .type   caml_callback_asm, %function
        .size   caml_callback_asm, .-caml_callback_asm

        TEXT_SECTION(caml_callback2_asm)
        .align  2
        .globl  caml_callback2_asm
caml_callback2_asm:
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2) */
        mov     ARG_DOMAIN_STATE_PTR, x0
        mov     TMP, x1
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        mov     x2, TMP         /* x2 = closure environment */
        ADDRGLOBAL(ARG, caml_apply2)
        b       .Ljump_to_caml
        CFI_ENDPROC
        .type   caml_callback2_asm, %function
        .size   caml_callback2_asm, .-caml_callback2_asm

        TEXT_SECTION(caml_callback3_asm)
        .align  2
        .globl  caml_callback3_asm
caml_callback3_asm:
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2,
        [x2,16] = arg3) */
        mov     ARG_DOMAIN_STATE_PTR, x0
        mov     x3, x1          /* x3 = closure environment */
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        ldr     x2, [x2, 16]    /* x2 = third arg */
        ADDRGLOBAL(ARG, caml_apply3)
        b       .Ljump_to_caml
        CFI_ENDPROC
        .size   caml_callback3_asm, .-caml_callback3_asm

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
    /* Load address of [caml_array_bound_error] in ARG */
        ADDRGLOBAL(ARG, caml_array_bound_error)
    /* Call that function */
        b       caml_c_call
        CFI_ENDPROC
        .size   caml_ml_array_bound_error, .-caml_ml_array_bound_error

        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

        .data
        .align  3
        .globl  caml_system__frametable
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .type   caml_system__frametable, %object
        .size   caml_system__frametable, .-caml_system__frametable

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
