/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                          Bill O'Farrell, IBM                        */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique. Copyright 2015 IBM (Bill O'Farrell with help from  */
/*  Tristan Amini). All rights reserved.  This file is distributed     */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/
#define Addrglobal(reg,glob) \
        larl    reg, glob
#define Loadglobal(reg,glob) \
        lgrl    reg, glob
#define Storeglobal(reg,glob) \
        stgrl   reg, glob
#define Loadglobal32(reg,glob) \
        lgfrl   reg, glob
#define Storeglobal32(reg,glob) \
        strl   reg, glob


        .section ".text"

/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

        .globl  caml_call_gc
        .type   caml_call_gc, @function
caml_call_gc:
    /* Set up stack frame */
#define FRAMESIZE (16*8 + 16*8)
        lay	%r15, -FRAMESIZE(%r15)
    /* Record return address into OCaml code */
        Storeglobal(%r14, caml_last_return_address)
    /* Record lowest stack address */
        lay	%r0, FRAMESIZE(%r15)
        Storeglobal(%r0, caml_bottom_of_stack)
    /* Record pointer to register array */
        lay	%r0, (8*16)(%r15)
        Storeglobal(%r0, caml_gc_regs)
    /* Save current allocation pointer for debugging purposes */
        Storeglobal(%r11, caml_young_ptr)
    /* Save exception pointer (if e.g. a sighandler raises) */
        Storeglobal(%r13, caml_exception_pointer)
    /* Save all registers used by the code generator */
        stmg    %r2,%r9, (8*16)(%r15)
	stg	%r12, (8*16 + 8*8)(%r15)
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
        lay %r15, -160(%r15)
        stg     %r15, 0(%r15)
        brasl   %r14, caml_garbage_collection@PLT
        lay %r15, 160(%r15)
    /* Reload new allocation pointer and allocation limit */
        Loadglobal(%r11, caml_young_ptr)
        Loadglobal(%r10, caml_young_limit)
    /* Restore all regs used by the code generator */
        lmg     %r2,%r9, (8*16)(%r15)
	lg	%r12, (8*16 + 8*8)(%r15)
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
        Loadglobal(%r1, caml_last_return_address)
    /* Deallocate stack frame */
        lay	%r15, FRAMESIZE(%r15)
    /* Return */
        br    %r1

/* Call a C function from OCaml */

        .globl  caml_c_call
        .type   caml_c_call, @function
caml_c_call:
        Storeglobal(%r15, caml_bottom_of_stack)
    /* Save return address */
        ldgr	%f15, %r14
    /* Get ready to call C function (address in r7) */
    /* Record lowest stack address and return address */
        Storeglobal(%r14, caml_last_return_address)
    /* Make the exception handler and alloc ptr available to the C code */
        Storeglobal(%r11, caml_young_ptr)
        Storeglobal(%r13, caml_exception_pointer)
    /* Call the function */
        basr %r14, %r7
    /* restore return address */
        lgdr	%r14,%f15
    /* Reload allocation pointer and allocation limit*/
        Loadglobal(%r11, caml_young_ptr)
        Loadglobal(%r10, caml_young_limit)
    /* Say we are back into OCaml code */
        lgfi   %r0, 0
        Storeglobal(%r0, caml_last_return_address)

    /* Return to caller */
        br %r14

/* Raise an exception from OCaml */
        .globl  caml_raise_exn
        .type   caml_raise_exn, @function
caml_raise_exn:
        Loadglobal32(%r0, caml_backtrace_active)
        cgfi	%r0, 0
        jne     .L110
.L111:
    /* Pop trap frame */
        lg	%r1, 0(%r13)
        lgr     %r15, %r13
        lg     %r13, 8(13)
        agfi   %r15, 16
    /* Branch to handler */
        br	%r1
.L110:
        lgfi      %r0, 0
        Storeglobal32(%r0, caml_backtrace_pos)
.L114:
        ldgr	%f15, %r2	/* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        lgr     %r3,%r14        /* arg2: PC of raise */
        lgr     %r4, %r15       /* arg3: SP of raise */
        lgr     %r5, %r13           /* arg4: SP of handler */
        agfi    %r15, -160       /* reserve stack space for C call */
        brasl   %r14, caml_stash_backtrace@PLT
        agfi    %r15, 160 
        lgdr	%r2,%f15        /* restore exn bucket */
        j       .L111           /* raise the exn */

        .globl  caml_reraise_exn
        .type   caml_reraise_exn, @function
caml_reraise_exn:
        Loadglobal32(%r0, caml_backtrace_active)
        cgfi	%r0, 0
        jne    .L114
    /* Pop trap frame */
        lg     %r1, 0(%r13)
        lgr    %r15, %r13
        lg    %r13, 8(%r13)
        agfi   %r15, 16
    /* Branch to handler */
        br     %r1;

/* Raise an exception from C */

        .globl  caml_raise_exception
        .type   caml_raise_exception, @function
caml_raise_exception:
        Loadglobal32(0, caml_backtrace_active)
        cgfi	%r0, 0
        jne    .L112
.L113:
    /* Reload OCaml global registers */
        Loadglobal(%r15, caml_exception_pointer)
        Loadglobal(%r11, caml_young_ptr)
        Loadglobal(%r10, caml_young_limit)
    /* Say we are back into OCaml code */
        lgfi   %r0, 0
        Storeglobal(%r0, caml_last_return_address)
    /* Pop trap frame */
        lg	%r1, 0(%r15)
        lg	%r13, 8(%r15)
        agfi	%r15, 16
    /* Branch to handler */
        br	%r1;
.L112:
        ldgr	%f15,%r2        /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r2 */
        Loadglobal(%r3, caml_last_return_address) /* arg2: PC of raise */
        Loadglobal(%r4, caml_bottom_of_stack)     /* arg3: SP of raise */
        Loadglobal(%r5, caml_exception_pointer)   /* arg4: SP of handler */
             /* reserve stack space for C call */
        lay %r15, -160(%r15)
        brasl   %r14, caml_stash_backtrace@PLT
        lay %r15, 160(%r15)
        lgdr	%r2,%f15        /* restore exn bucket */        /* restore exn bucket */
        j       .L113           /* raise the exn */

/* Start the OCaml program */

        .globl  caml_start_program
        .type   caml_start_program, @function
caml_start_program:
        Addrglobal(%r0, caml_program)

/* Code shared between caml_start_program and caml_callback */
.L102:
    /* Allocate and link stack frame */
        stg	%r15, -320(%r15)
        agfi	%r15, -320
    /* Save return address */
        stg     %r14, 8(%r15)
    /* Save all callee-save registers */
    /* GPR 14 at sp+16 ... GPR 31 at sp+84
       FPR 14 at sp+92 ... FPR 31 at sp+228 */
        lgr	%r1, %r15
        agfi    %r1, 16-8
        stmg    %r6,%r13, 8(%r1)
	stg	%r15, 72(%r1)
	std     %f0, 80(%r1)
        std     %f8, 88(%r1)
        std     %f9, 96(%r1)
        std     %f10, 104(%r1)
        std     %f11, 112(%r1)
        std     %f12, 120(%r1)
        std     %f13, 128(%r1)
        std     %f14, 136(%r1)
        std     %f15, 144(%r1)

    /* Set up a callback link */
        agfi    %r15, -32
        Loadglobal(%r1, caml_bottom_of_stack)
        stg     %r1, 0(%r15)
        Loadglobal(%r1, caml_last_return_address)
        stg     %r1, 8(%r15)
        Loadglobal(%r1, caml_gc_regs)
        stg     %r1, 16(%r15)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        brasl   %r14, .L103
        j       .L104
.L103:
        agfi    %r15,  -16
        stg     %r14, 0(%r15)
        Loadglobal(%r1, caml_exception_pointer)
        stg      %r1, 8(%r15)
        lgr     %r13, %r15
    /* Reload allocation pointers */
        Loadglobal(%r11, caml_young_ptr)
        Loadglobal(%r10, caml_young_limit)
    /* Say we are back into OCaml code */
        lgfi   %r1, 0
        Storeglobal(%r1, caml_last_return_address)
    /* Call the OCaml code */
        lgr %r1,%r0
.L105:
        basr %r14, %r1
    /* Pop the trap frame, restoring caml_exception_pointer */
        lg     %r0, 8(%r15)
        Storeglobal(%r0, caml_exception_pointer)
        agfi    %r15, 16
    /* Pop the callback link, restoring the global variables */
.L106:
        lg      %r5, 0(%r15)
        lg      %r6, 8(%r15)
        nill	%r6, 0xFFFE
        lg      %r1, 16(%r15)
        Storeglobal(%r5, caml_bottom_of_stack)
        Storeglobal(%r6, caml_last_return_address)
        Storeglobal(%r1, caml_gc_regs)
        agfi    %r15, 32
    /* Update allocation pointer */
        Storeglobal(%r11, caml_young_ptr)
    /* Restore callee-save registers */
        lgr	%r1, %r15
        agfi    %r1, 16-8
        lmg     %r6,%r13, 8(%r1)
	ld     %f0, 80(%r1)
        ld     %f8, 88(%r1)
        ld     %f9, 96(%r1)
        ld     %f10, 104(%r1)
        ld     %f11, 112(%r1)
        ld     %f12, 120(%r1)
        ld     %f13, 128(%r1)
        ld     %f14, 136(%r1)
        ld     %f15, 144(%r1)

    /* Reload return address */
        lg     %r1, 8(%r15)
    /* Return */
        agfi    %r15,  320
        br	%r1

    /* The trap handler: */
.L104:
    /* Update caml_exception_pointer */
        Storeglobal(%r13, caml_exception_pointer)
    /* Encode exception bucket as an exception result and return it */
        oill     %r2,  2
        j       .L106

/* Callback from C to OCaml */

        .globl  caml_callback_exn
        .type   caml_callback_exn, @function
caml_callback_exn:
    /* Initial shuffling of arguments */
        lgr     %r0, %r2            /* Closure */
        lgr     %r2, %r3            /* Argument */
        lgr     %r3, %r0
        lg      %r0, 0(%r3)        /* Code pointer */
        j       .L102

        .globl  caml_callback2_exn
        .type   caml_callback2_exn, @function
caml_callback2_exn:
        lgr      %r0, %r2            /* Closure */
        lgr      %r2, %r3            /* First argument */
        lgr      %r3, %r4            /* Second argument */
        lgr      %r4, %r0
        Addrglobal(%r0, caml_apply2)
        j       .L102

        .globl  caml_callback3_exn
        .type   caml_callback3_exn, @function
caml_callback3_exn:
        lgr      %r0, %r2            /* Closure */
        lgr      %r2, %r3            /* First argument */
        lgr      %r3, %r4            /* Second argument */
        lgr      %r4, %r5            /* Third argument */
        lgr      %r5, %r0
        Addrglobal(%r0, caml_apply3)
        j        .L102

	.globl	caml_ml_array_bound_error
	.type	caml_ml_array_bound_error, @function
caml_ml_array_bound_error:
	lay	%r15, -160(%r15)    /* Reserve stack space for C call */
	larl	%r1, caml_array_bound_error
	j	caml_c_call
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .align 8
        .globl  caml_system__frametable
        .type   caml_system__frametable, @object
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .L105 + 2       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
	.align	8
