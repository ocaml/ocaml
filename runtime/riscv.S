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

#define ARG_DOMAIN_STATE_PTR t0
#define DOMAIN_STATE_PTR s11
#define TRAP_PTR s1
#define ALLOC_PTR s10
#define TMP t1
#define ARG t2

#define STORE sd
#define LOAD ld

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

#define Caml_state(var) (8*domain_field_caml_##var)(DOMAIN_STATE_PTR)

#define FUNCTION(name) \
        .align 2; \
        .globl name; \
        .type name, @function; \
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

        .section        .text
/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

FUNCTION(caml_call_gc)
.Lcaml_call_gc:
        /* Record return address */
        STORE   ra, Caml_state(last_return_address)
        /* Record lowest stack address */
        STORE   sp, Caml_state(bottom_of_stack)
        /* Set up stack space, saving return address */
        /* (1 reg for RA, 1 reg for FP, 23 allocatable int regs,
            20 caller-save float regs) * 8 */
        /* + 1 for alignment */
        addi    sp, sp, -0x170
        CFI_ADJUST(0x170)
        STORE   ra, 0x8(sp)
        CFI_OFFSET(ra, -0x170+8)
        /* Save allocatable integer registers on the stack,
           in the order given in proc.ml */
        STORE   a0, 0x10(sp)
        STORE   a1, 0x18(sp)
        STORE   a2, 0x20(sp)
        STORE   a3, 0x28(sp)
        STORE   a4, 0x30(sp)
        STORE   a5, 0x38(sp)
        STORE   a6, 0x40(sp)
        STORE   a7, 0x48(sp)
        STORE   s2, 0x50(sp)
        STORE   s3, 0x58(sp)
        STORE   s4, 0x60(sp)
        STORE   s5, 0x68(sp)
        STORE   s6, 0x70(sp)
        STORE   s7, 0x78(sp)
        STORE   s8, 0x80(sp)
        STORE   s9, 0x88(sp)
        STORE   t2, 0x90(sp)
        STORE   t3, 0x98(sp)
        STORE   t4, 0xa0(sp)
        STORE   t5, 0xa8(sp)
        STORE   t6, 0xb0(sp)
        STORE   s0, 0xb8(sp)
        STORE   t0, 0xc0(sp)
        /* Save caller-save floating-point registers on the stack
           (callee-saves are preserved by caml_garbage_collection) */
        fsd     ft0, 0xd0(sp)
        fsd     ft1, 0xd8(sp)
        fsd     ft2, 0xe0(sp)
        fsd     ft3, 0xe8(sp)
        fsd     ft4, 0xf0(sp)
        fsd     ft5, 0xf8(sp)
        fsd     ft6, 0x100(sp)
        fsd     ft7, 0x108(sp)
        fsd     fa0, 0x110(sp)
        fsd     fa1, 0x118(sp)
        fsd     fa2, 0x120(sp)
        fsd     fa3, 0x128(sp)
        fsd     fa4, 0x130(sp)
        fsd     fa5, 0x138(sp)
        fsd     fa6, 0x140(sp)
        fsd     fa7, 0x148(sp)
        fsd     ft8, 0x150(sp)
        fsd     ft9, 0x158(sp)
        fsd     ft10, 0x160(sp)
        fsd     ft11, 0x168(sp)
        /* Store pointer to saved integer registers in caml_gc_regs */
        addi    TMP, sp, 0x10
        STORE   TMP, Caml_state(gc_regs)
        /* Save current allocation pointer for debugging purposes */
        STORE   ALLOC_PTR, Caml_state(young_ptr)
        /* Save trap pointer in case an exception is raised during GC */
        STORE   TRAP_PTR, Caml_state(exception_pointer)
        /* Call the garbage collector */
        call    PLT(caml_garbage_collection)
        /* Restore registers */
        LOAD    a0, 0x10(sp)
        LOAD    a1, 0x18(sp)
        LOAD    a2, 0x20(sp)
        LOAD    a3, 0x28(sp)
        LOAD    a4, 0x30(sp)
        LOAD    a5, 0x38(sp)
        LOAD    a6, 0x40(sp)
        LOAD    a7, 0x48(sp)
        LOAD    s2, 0x50(sp)
        LOAD    s3, 0x58(sp)
        LOAD    s4, 0x60(sp)
        LOAD    s5, 0x68(sp)
        LOAD    s6, 0x70(sp)
        LOAD    s7, 0x78(sp)
        LOAD    s8, 0x80(sp)
        LOAD    s9, 0x88(sp)
        LOAD    t2, 0x90(sp)
        LOAD    t3, 0x98(sp)
        LOAD    t4, 0xa0(sp)
        LOAD    t5, 0xa8(sp)
        LOAD    t6, 0xb0(sp)
        LOAD    s0, 0xb8(sp)
        LOAD    t0, 0xc0(sp)
        fld     ft0, 0xd0(sp)
        fld     ft1, 0xd8(sp)
        fld     ft2, 0xe0(sp)
        fld     ft3, 0xe8(sp)
        fld     ft4, 0xf0(sp)
        fld     ft5, 0xf8(sp)
        fld     ft6, 0x100(sp)
        fld     ft7, 0x108(sp)
        fld     fa0, 0x110(sp)
        fld     fa1, 0x118(sp)
        fld     fa2, 0x120(sp)
        fld     fa3, 0x128(sp)
        fld     fa4, 0x130(sp)
        fld     fa5, 0x138(sp)
        fld     fa6, 0x140(sp)
        fld     fa7, 0x148(sp)
        fld     ft8, 0x150(sp)
        fld     ft9, 0x158(sp)
        fld     ft10, 0x160(sp)
        fld     ft11, 0x168(sp)
        /* Reload new allocation pointer */
        LOAD    ALLOC_PTR, Caml_state(young_ptr)
        /* Free stack space and return to caller */
        LOAD    ra, 0x8(sp)
        addi    sp, sp, 0x170
        CFI_ADJUST(-0x170)
        ret
END_FUNCTION(caml_call_gc)

/* Call a C function from OCaml */
/* Function to call is in ARG */

FUNCTION(caml_c_call)
        /* Preserve return address in callee-save register s2 */
        mv      s2, ra
        CFI_REGISTER(ra, s2)
        /* Record lowest stack address and return address */
        STORE   ra, Caml_state(last_return_address)
        STORE   sp, Caml_state(bottom_of_stack)
        /* Make the exception handler alloc ptr available to the C code */
        STORE   ALLOC_PTR, Caml_state(young_ptr)
        STORE   TRAP_PTR, Caml_state(exception_pointer)
        /* Call the function */
        jalr    ARG
        /* Reload alloc ptr */
        LOAD    ALLOC_PTR, Caml_state(young_ptr)
        /* Return */
        jr      s2
END_FUNCTION(caml_c_call)

/* Raise an exception from OCaml */
FUNCTION(caml_raise_exn)
        /* Test if backtrace is active */
        LOAD    TMP, Caml_state(backtrace_active)
        bnez    TMP, 2f
1:      /* Cut stack at current trap handler */
        mv      sp, TRAP_PTR
        /* Pop previous handler and jump to it */
        LOAD    TMP, 8(sp)
        LOAD    TRAP_PTR, 0(sp)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
        jr      TMP
2:      /* Preserve exception bucket in callee-save register s2 */
        mv      s2, a0
        /* Stash the backtrace */
        mv      a1, ra
        mv      a2, sp
        mv      a3, TRAP_PTR
        call    PLT(caml_stash_backtrace)
        /* Restore exception bucket and raise */
        mv      a0, s2
        j       1b
END_FUNCTION(caml_raise_exn)

        .globl  caml_reraise_exn
        .type   caml_reraise_exn, @function

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        mv      DOMAIN_STATE_PTR, a0
        mv      a0, a1
        LOAD    TRAP_PTR, Caml_state(exception_pointer)
        LOAD    ALLOC_PTR, Caml_state(young_ptr)
        LOAD    TMP, Caml_state(backtrace_active)
        bnez    TMP, 2f
1:      /* Cut stack at current trap handler */
        mv      sp, TRAP_PTR
        LOAD    TMP, 8(sp)
        LOAD    TRAP_PTR, 0(sp)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
        jr      TMP
2:      /* Preserve exception bucket in callee-save register s2 */
        mv      s2, a0
        LOAD    a1, Caml_state(last_return_address)
        LOAD    a2, Caml_state(bottom_of_stack)
        mv      a3, TRAP_PTR
        call    PLT(caml_stash_backtrace)
        mv      a0, s2
        j       1b
END_FUNCTION(caml_raise_exception)

/* Start the OCaml program */

FUNCTION(caml_start_program)
        mv      ARG_DOMAIN_STATE_PTR, a0
        la      ARG, caml_program
        /* Code shared with caml_callback* */
        /* Address of OCaml code to call is in ARG */
        /* Arguments to the OCaml code are in a0 ... a7 */
.Ljump_to_caml:
        /* Set up stack frame and save callee-save registers */
        addi    sp, sp, -0xd0
        CFI_ADJUST(0xd0)
        STORE   ra, 0xc0(sp)
        CFI_OFFSET(ra, -0xd0+0xc0)
        STORE   s0, 0x0(sp)
        STORE   s1, 0x8(sp)
        STORE   s2, 0x10(sp)
        STORE   s3, 0x18(sp)
        STORE   s4, 0x20(sp)
        STORE   s5, 0x28(sp)
        STORE   s6, 0x30(sp)
        STORE   s7, 0x38(sp)
        STORE   s8, 0x40(sp)
        STORE   s9, 0x48(sp)
        STORE   s10, 0x50(sp)
        STORE   s11, 0x58(sp)
        fsd     fs0, 0x60(sp)
        fsd     fs1, 0x68(sp)
        fsd     fs2, 0x70(sp)
        fsd     fs3, 0x78(sp)
        fsd     fs4, 0x80(sp)
        fsd     fs5, 0x88(sp)
        fsd     fs6, 0x90(sp)
        fsd     fs7, 0x98(sp)
        fsd     fs8, 0xa0(sp)
        fsd     fs9, 0xa8(sp)
        fsd     fs10, 0xb0(sp)
        fsd     fs11, 0xb8(sp)
        addi    sp, sp, -32
        CFI_ADJUST(32)
        /* Load domain state pointer from argument */
        mv      DOMAIN_STATE_PTR, ARG_DOMAIN_STATE_PTR
        /* Setup a callback link on the stack */
        LOAD    TMP, Caml_state(bottom_of_stack)
        STORE   TMP, 0(sp)
        LOAD    TMP, Caml_state(last_return_address)
        STORE   TMP, 8(sp)
        LOAD    TMP, Caml_state(gc_regs)
        STORE   TMP, 16(sp)
        /* set up a trap frame */
        addi    sp, sp, -16
        CFI_ADJUST(16)
        LOAD    TMP, Caml_state(exception_pointer)
        STORE   TMP, 0(sp)
        lla     TMP, .Ltrap_handler
        STORE   TMP, 8(sp)
        mv      TRAP_PTR, sp
        LOAD    ALLOC_PTR, Caml_state(young_ptr)
        STORE   x0, Caml_state(last_return_address)
        jalr    ARG
.Lcaml_retaddr:         /* pop trap frame, restoring caml_exception_pointer */
        LOAD    TMP, 0(sp)
        STORE   TMP, Caml_state(exception_pointer)
        addi    sp, sp, 16
        CFI_ADJUST(-16)
.Lreturn_result:        /* pop callback link, restoring global variables */
        LOAD    TMP, 0(sp)
        STORE   TMP, Caml_state(bottom_of_stack)
        LOAD    TMP, 8(sp)
        STORE   TMP, Caml_state(last_return_address)
        LOAD    TMP, 16(sp)
        STORE   TMP, Caml_state(gc_regs)
        addi    sp, sp, 32
        CFI_ADJUST(-32)
        /* Update allocation pointer */
        STORE   ALLOC_PTR, Caml_state(young_ptr)
        /* reload callee-save registers and return */
        LOAD    ra, 0xc0(sp)
        LOAD    s0, 0x0(sp)
        LOAD    s1, 0x8(sp)
        LOAD    s2, 0x10(sp)
        LOAD    s3, 0x18(sp)
        LOAD    s4, 0x20(sp)
        LOAD    s5, 0x28(sp)
        LOAD    s6, 0x30(sp)
        LOAD    s7, 0x38(sp)
        LOAD    s8, 0x40(sp)
        LOAD    s9, 0x48(sp)
        LOAD    s10, 0x50(sp)
        LOAD    s11, 0x58(sp)
        fld     fs0, 0x60(sp)
        fld     fs1, 0x68(sp)
        fld     fs2, 0x70(sp)
        fld     fs3, 0x78(sp)
        fld     fs4, 0x80(sp)
        fld     fs5, 0x88(sp)
        fld     fs6, 0x90(sp)
        fld     fs7, 0x98(sp)
        fld     fs8, 0xa0(sp)
        fld     fs9, 0xa8(sp)
        fld     fs10, 0xb0(sp)
        fld     fs11, 0xb8(sp)
        addi    sp, sp, 0xd0
        CFI_ADJUST(-0xd0)
        ret
        .type   .Lcaml_retaddr, @function
        .size   .Lcaml_retaddr, .-.Lcaml_retaddr
END_FUNCTION(caml_start_program)

        .align  2
.Ltrap_handler:
        CFI_STARTPROC
        STORE   TRAP_PTR, Caml_state(exception_pointer)
        ori     a0, a0, 2
        j       .Lreturn_result
        .type   .Ltrap_handler, @function
END_FUNCTION(.Ltrap_handler)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        /* Initial shuffling of arguments */
        /* a0 = Caml_state, a1 = closure, (a2) = args */
        mv      ARG_DOMAIN_STATE_PTR, a0
        LOAD    a0, 0(a2)   /* a0 = first arg */
                            /* a1 = closure environment */
        LOAD    ARG, 0(a1)  /* code pointer */
        j       .Ljump_to_caml
END_FUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
        /* Initial shuffling of arguments */
        /* a0 = Caml_state, a1 = closure, (a2) = args */
        mv      ARG_DOMAIN_STATE_PTR, a0
        mv      TMP, a1
        LOAD    a0, 0(a2)
        LOAD    a1, 8(a2)
        mv      a2, TMP
        la      ARG, caml_apply2
        j       .Ljump_to_caml
END_FUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
        /* Initial shuffling of arguments */
        /* a0 = Caml_state, a1 = closure, (a2) = args */
        mv      ARG_DOMAIN_STATE_PTR, a0
        mv      a3, a1
        LOAD    a0, 0(a2)
        LOAD    a1, 8(a2)
        LOAD    a2, 16(a2)
        la      ARG, caml_apply3
        j       .Ljump_to_caml
END_FUNCTION(caml_callback3_asm)

FUNCTION(caml_ml_array_bound_error)
        /* Load address of [caml_array_bound_error_asm] in ARG */
        la      ARG, caml_array_bound_error_asm
        /* Call that function */
        tail    caml_c_call
END_FUNCTION(caml_ml_array_bound_error)

        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

        .section .data
        .align  3
        .globl  caml_system__frametable
        .type   caml_system__frametable, @object
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .size   caml_system__frametable, .-caml_system__frametable
