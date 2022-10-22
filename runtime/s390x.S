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

#if defined(__PIC__)

#define Addrglobal(reg,glob) \
        lgrl    reg, glob@GOTENT
#else

#define Addrglobal(reg,glob) \
        larl    reg, glob
#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) 8*domain_field_caml_##var(%r10)

        .section ".text"

/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

        .globl  caml_call_gc
        .type   caml_call_gc, @function
caml_call_gc:
    /* Set up stack frame */
#define FRAMESIZE (16*8 + 16*8)
        lay     %r15, -FRAMESIZE(%r15)
    /* Record return address into OCaml code */
        stg     %r14, Caml_state(last_return_address)
    /* Record lowest stack address */
        lay     %r0, FRAMESIZE(%r15)
        stg     %r0, Caml_state(bottom_of_stack)
    /* Record pointer to register array */
        lay     %r0, (8*16)(%r15)
        stg     %r0, Caml_state(gc_regs)
    /* Save current allocation pointer for debugging purposes */
        stg     %r11, Caml_state(young_ptr)
    /* Save exception pointer (if e.g. a sighandler raises) */
        stg     %r13, Caml_state(exception_pointer)
    /* Save all registers used by the code generator */
        stmg    %r2,%r9, (8*16)(%r15)
        stg     %r12, (8*16 + 8*8)(%r15)
        std     %f0, 0(%r15)
        std     %f1, 8(%r15)
        std     %f2, 16(%r15)
        std     %f3, 24(%r15)
        std     %f4, 32(%r15)
        std     %f5, 40(%r15)
        std     %f6, 48(%r15)
        std     %f7, 56(%r15)
        std     %f8, 64(%r15)
        std     %f9, 72(%r15)
        std     %f10, 80(%r15)
        std     %f11, 88(%r15)
        std     %f12, 96(%r15)
        std     %f13, 108(%r15)
        std     %f14, 112(%r15)
        std     %f15, 120(%r15)
    /* Call the GC */
        lay     %r15, -160(%r15)
        stg     %r15, 0(%r15)
        brasl   %r14, caml_garbage_collection@PLT
        lay     %r15, 160(%r15)
    /* Reload new allocation pointer */
        lg      %r11, Caml_state(young_ptr)
    /* Restore all regs used by the code generator */
        lmg     %r2,%r9, (8*16)(%r15)
        lg      %r12, (8*16 + 8*8)(%r15)
        ld      %f0, 0(%r15)
        ld      %f1, 8(%r15)
        ld      %f2, 16(%r15)
        ld      %f3, 24(%r15)
        ld      %f4, 32(%r15)
        ld      %f5, 40(%r15)
        ld      %f6, 48(%r15)
        ld      %f7, 56(%r15)
        ld      %f8, 64(%r15)
        ld      %f9, 72(%r15)
        ld      %f10, 80(%r15)
        ld      %f11, 88(%r15)
        ld      %f12, 96(%r15)
        ld      %f13, 108(%r15)
        ld      %f14, 112(%r15)
        ld      %f15, 120(%r15)
    /* Return to caller */
        lg      %r1, Caml_state(last_return_address)
    /* Deallocate stack frame */
        lay     %r15, FRAMESIZE(%r15)
    /* Return */
        br      %r1

/* Call a C function from OCaml */

        .globl  caml_c_call
        .type   caml_c_call, @function
caml_c_call:
        stg     %r15, Caml_state(bottom_of_stack)
.L101:
    /* Save return address */
        ldgr    %f15, %r14
    /* Get ready to call C function (address in r7) */
    /* Record lowest stack address and return address */
        stg     %r14, Caml_state(last_return_address)
    /* Make the exception handler and alloc ptr available to the C code */
        stg     %r11, Caml_state(young_ptr)
        stg     %r13, Caml_state(exception_pointer)
    /* Call the function */
        basr %r14, %r7
    /* restore return address */
        lgdr    %r14,%f15
    /* Reload allocation pointer */
        lg      %r11, Caml_state(young_ptr)
    /* Return to caller */
        br %r14

/* Raise an exception from OCaml */
        .globl  caml_raise_exn
        .type   caml_raise_exn, @function
caml_raise_exn:
        lg      %r0, Caml_state(backtrace_active)
        cgfi    %r0, 0
        jne     .L110
.L111:
    /* Pop trap frame */
        lg      %r1, 0(%r13)
        lgr     %r15, %r13
        lg      %r13, 8(13)
        agfi    %r15, 16
    /* Branch to handler */
        br      %r1
.L110:
        ldgr    %f15, %r2       /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r2 */
        lgr     %r3, %r14       /* arg2: PC of raise */
        lgr     %r4, %r15       /* arg3: SP of raise */
        lgr     %r5, %r13       /* arg4: SP of handler */
        agfi    %r15, -160      /* reserve stack space for C call */
        brasl   %r14, caml_stash_backtrace@PLT
        agfi    %r15, 160
        lgdr    %r2,%f15        /* restore exn bucket */
        j       .L111           /* raise the exn */

/* Raise an exception from C */

        .globl  caml_raise_exception
        .type   caml_raise_exception, @function
caml_raise_exception:
        lgr     %r10, %r2       /* Load domain state pointer */
        lgr     %r2, %r3        /* Move exception bucket to arg1 register */
        lg      %r0, Caml_state(backtrace_active)
        cgfi    %r0, 0
        jne    .L112
.L113:
    /* Reload OCaml global registers */
        lg      %r15, Caml_state(exception_pointer)
        lg      %r11, Caml_state(young_ptr)
    /* Pop trap frame */
        lg      %r1, 0(%r15)
        lg      %r13, 8(%r15)
        agfi    %r15, 16
    /* Branch to handler */
        br      %r1;
.L112:
        ldgr    %f15,%r2        /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r2 */
        lg      %r3, Caml_state(last_return_address) /* arg2: PC of raise */
        lg      %r4, Caml_state(bottom_of_stack)     /* arg3: SP of raise */
        lg      %r5, Caml_state(exception_pointer)   /* arg4: SP of handler */
    /* reserve stack space for C call */
        lay     %r15, -160(%r15)
        brasl   %r14, caml_stash_backtrace@PLT
        lay     %r15, 160(%r15)
        lgdr    %r2,%f15        /* restore exn bucket */
        j       .L113           /* raise the exn */

/* Start the OCaml program */

        .globl  caml_start_program
        .type   caml_start_program, @function
caml_start_program:
    /* Move Caml_state passed as first argument to r1 */
        lgr     %r1, %r2
        Addrglobal(%r0, caml_program)

/* Code shared between caml_start_program and caml_callback */
.L102:
    /* Allocate stack frame */
        lay     %r15, -144(%r15)
    /* Save all callee-save registers + return address */
    /* GPR 6..14 at sp + 0 ... sp + 64
       FPR 10..15 at sp + 72 ... sp + 128 */
        stmg    %r6,%r14, 0(%r15)
        std     %f8, 72(%r15)
        std     %f9, 80(%r15)
        std     %f10, 88(%r15)
        std     %f11, 96(%r15)
        std     %f12, 104(%r15)
        std     %f13, 112(%r15)
        std     %f14, 120(%r15)
        std     %f15, 128(%r15)

    /* Load Caml_state to r10 register */
        lgr     %r10, %r1
    /* Set up a callback link */
        lay     %r15, -32(%r15)
        lg      %r1, Caml_state(bottom_of_stack)
        stg     %r1, 0(%r15)
        lg      %r1, Caml_state(last_return_address)
        stg     %r1, 8(%r15)
        lg      %r1, Caml_state(gc_regs)
        stg     %r1, 16(%r15)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        brasl   %r14, .L103
        j       .L104
.L103:
        lay     %r15, -16(%r15)
        stg     %r14, 0(%r15)
        lg      %r1, Caml_state(exception_pointer)
        stg     %r1, 8(%r15)
        lgr     %r13, %r15
    /* Reload allocation pointer */
        lg      %r11, Caml_state(young_ptr)
    /* Call the OCaml code */
        lgr     %r1,%r0
        basr    %r14, %r1
.L105:
    /* Pop the trap frame, restoring caml_exception_pointer */
        lg      %r0, 8(%r15)
        stg     %r0, Caml_state(exception_pointer)
        la      %r15, 16(%r15)
    /* Pop the callback link, restoring the global variables */
.L106:
        lg      %r5, 0(%r15)
        lg      %r6, 8(%r15)
        lg      %r0, 16(%r15)
        stg     %r5, Caml_state(bottom_of_stack)
        stg     %r6, Caml_state(last_return_address)
        stg     %r0, Caml_state(gc_regs)
        la      %r15, 32(%r15)

    /* Update allocation pointer */
        stg     %r11, Caml_state(young_ptr)

    /* Restore registers */
        lmg     %r6,%r14, 0(%r15)
        ld      %f8, 72(%r15)
        ld      %f9, 80(%r15)
        ld      %f10, 88(%r15)
        ld      %f11, 96(%r15)
        ld      %f12, 104(%r15)
        ld      %f13, 112(%r15)
        ld      %f14, 120(%r15)
        ld      %f15, 128(%r15)

    /* Return */
        lay     %r15, 144(%r15)
        br      %r14

    /* The trap handler: */
.L104:
    /* Update caml_exception_pointer */
        stg     %r13, Caml_state(exception_pointer)
    /* Encode exception bucket as an exception result and return it */
        oill     %r2,  2
        j       .L106

/* Callback from C to OCaml */

        .globl  caml_callback_asm
        .type   caml_callback_asm, @function
caml_callback_asm:
    /* Initial shuffling of arguments */
    /* (r2 = Caml_state, r3 = closure, 0(r4) = arg1) */
        lgr     %r1, %r2            /* r1 = Caml_state */
        lg      %r2, 0(%r4)         /* r2 = Argument */
                                    /* r3 = Closure */
        lg      %r0, 0(%r3)         /* r0 = Code pointer */
        j       .L102

        .globl  caml_callback2_asm
        .type   caml_callback2_asm, @function
caml_callback2_asm:
    /* Initial shuffling of arguments */
    /* (r2 = Caml_state, r3 = closure, 0(r4) = arg1, 8(r4) = arg2) */
        lgr      %r1, %r2            /* r1 = Caml_state */
        lgr      %r0, %r3
        lg       %r2, 0(%r4)         /* r2 = First argument */
        lg       %r3, 8(%r4)         /* r3 = Second argument */
        lgr      %r4, %r0            /* r4 = Closure */
        Addrglobal(%r0, caml_apply2) /* r0 = Code pointer */
        j       .L102

        .globl  caml_callback3_asm
        .type   caml_callback3_asm, @function
caml_callback3_asm:
    /* Initial shuffling of arguments */
    /* (r2 = Caml_state, r3 = closure, 0(r4) = arg1, 8(r4) = arg2,
        16(r4) = arg3) */
        lgr      %r1, %r2            /* r1 = Caml_state */
        lgr      %r5, %r3            /* r5 = Closure */
        lg       %r2, 0(%r4)         /* r2 = First argument */
        lg       %r3, 8(%r4)         /* r3 = Second argument */
        lg       %r4, 16(%r4)        /* r4 = Third argument */
        Addrglobal(%r0, caml_apply3) /* r0 = Code pointer */
        j        .L102

        .globl  caml_ml_array_bound_error
        .type   caml_ml_array_bound_error, @function
caml_ml_array_bound_error:
        /* Save return address before decrementing SP, otherwise
           the frame descriptor for the call site is not correct */
        stg     %r15, Caml_state(bottom_of_stack)
        lay     %r15, -160(%r15)    /* Reserve stack space for C call */
        Addrglobal(%r7, caml_array_bound_error_asm)
        j       .L101
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .align 8
        .globl  caml_system.frametable
        .type   caml_system.frametable, @object
caml_system.frametable:
        .quad   1               /* one descriptor */
        .quad   .L105           /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
        .align  8

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
