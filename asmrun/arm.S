/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, ARM processor */

        .text

/* Allocation functions and GC interface */

        .global caml_call_gc
caml_call_gc:
    /* Record return address */
    /* We can use alloc_limit as a temp reg since it's not live here */
        ldr     alloc_limit, .Lcaml_last_return_address
        str     lr, [alloc_limit, #0]
    /* Branch to shared GC code */
        bl      .Linvoke_gc
    /* Restart allocation sequence (3 instructions before) */
        sub     lr, lr, #12
        mov     pc, lr

        .global caml_alloc1
caml_alloc1:
        sub     alloc_ptr, alloc_ptr, #8
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     alloc_limit, .Lcaml_last_return_address
        str     lr, [alloc_limit, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc1

        .global caml_alloc2
caml_alloc2:
        sub     alloc_ptr, alloc_ptr, #12
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     alloc_limit, .Lcaml_last_return_address
        str     lr, [alloc_limit, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc2

        .global caml_alloc3
caml_alloc3:
        sub     alloc_ptr, alloc_ptr, #16
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     alloc_limit, .Lcaml_last_return_address
        str     lr, [alloc_limit, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc3

/* Shared code to invoke the GC */
.Linvoke_gc:
    /* Record lowest stack address */
        ldr     alloc_limit, .Lcaml_bottom_of_stack
        str     sp, [alloc_limit, #0]
    /* Save integer registers and return address on stack */
        sub     sp, sp, #4*10+4
        stmia   sp, {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,lr}
    /* Store pointer to saved integer registers in caml_gc_regs */
        ldr     alloc_limit, .Lcaml_gc_regs
        str     sp, [alloc_limit, #0]
    /* Save non-callee-save float registers */
        stfd    f0, [sp, #-8]
        stfd    f1, [sp, #-8]
        stfd    f2, [sp, #-8]
        stfd    f3, [sp, #-8]
    /* Save current allocation pointer for debugging purposes */
        ldr     alloc_limit, .Lyoung_ptr
        str     alloc_ptr, [alloc_limit, #0]
    /* Save trap pointer in case an exception is raised during GC */
        ldr     alloc_limit, .Lcaml_exception_pointer
        str     trap_ptr, [alloc_limit, #0]
    /* Call the garbage collector */
        bl      garbage_collection
    /* Restore the registers from the stack */        
        ldfd    f4, [sp], #8
        ldfd    f5, [sp], #8
        ldfd    f6, [sp], #8
        ldfd    f7, [sp], #8
        ldmia   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9}
    /* Reload return address */
        ldr     alloc_limit, .Lcaml_last_return_address
        ldr     lr, [alloc_limit, #0]
    /* Say that we are back into Caml code */
        mov     alloc_ptr, #0
        str     alloc_ptr, [alloc_limit, #0]
    /* Reload new allocation pointer and allocation limit */
        ldr     alloc_limit, .Lyoung_ptr
        ldr     alloc_ptr, [alloc_limit, #0]
        ldr     alloc_limit, .Lyoung_limit
        ldr     alloc_limit, [alloc_limit, #0]
    /* Return to caller */
        ldmia   sp!, {pc}

/* Call a C function from Caml */
/* Function to call is in r9 */

        .global caml_c_call
caml_c_call:
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
    /* Record lowest stack address and return address */
        ldr     r5, .Lcaml_last_return_address
        ldr     r6, .Lcaml_bottom_of_stack
        str     lr, [r5, #0]
        str     sp, [r6, #0]
    /* Make the exception handler and alloc ptr available to the C code */
        ldr     r6, .Lyoung_ptr
        ldr     r7, .Lcaml_exception_pointer
        str     alloc_ptr, [r6, #0]
        str     trap_ptr, [r7, #0]
    /* Call the function */
        mov     lr, pc
        mov     pc, r9
    /* Reload alloc ptr and alloc limit */
        ldr     r7, .Lyoung_limit
        ldr     alloc_ptr, [r6, #0]    /* r6 still points to young_ptr */
        ldr     alloc_limit, [r7, #0]
    /* Say that we are back into Caml code */
        mov     r8, #0
        str     r8, [r5, #0] /* r5 still points to caml_last_return_address */
    /* Return */
        mov     pc, r4

/* Start the Caml program */

        .global caml_start_program
caml_start_program:
        ldr     r9, .Lcaml_program

/* Code shared with callback* */
/* Address of Caml code to call is in r9 */
/* Arguments to the Caml code are in r0...r3 */

.Ljump_to_caml:
    /* Save return address and callee-save registers */
        stmfd   sp!, {r4,r5,r6,r7,r8,r9,lr}
        stfd    f7, [sp, #-8]!
        stfd    f6, [sp, #-8]!
        stfd    f5, [sp, #-8]!
        stfd    f4, [sp, #-8]!
    /* Setup a callback link on the stack */
        sub     sp, sp, #4*3
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r4, [r4, #0]
        str     r4, [sp, #4]
        ldr     r4, .Lcaml_gc_regs
        ldr     r4, [r4, #0]
        str     r4, [sp, #8]
    /* Setup a trap frame to catch exceptions escaping the Caml code */
        sub     sp, sp, #4*2
        ldr     r4, .Lcaml_exception_pointer
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, .LLtrap_handler
        str     r4, [sp, #4]
    /* Reload allocation pointers */
        ldr     r4, .Lyoung_ptr
        ldr     alloc_ptr, [r4, #0]
        ldr     r4, .Lyoung_limit
        ldr     alloc_limit, [r4, #0]
    /* We are back into Caml code */
        ldr     r4, .Lcaml_last_return_address
        mov     r5, #0
        str     r5, [r4, #0]
    /* Call the Caml code */
        mov     lr, pc
        mov     pc, r9
.Lcaml_retaddr:
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     r4, .Lcaml_exception_pointer
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        add     sp, sp, #2 * 4
    /* Pop the callback link, restoring the global variables */
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_gc_regs
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
    /* Update allocation pointer */
        ldr     r4, .Lyoung_ptr
        str     alloc_ptr, [r4, #0]
    /* Reload callee-save registers and return */
        ldfd    f4, [sp], #8
        ldfd    f5, [sp], #8
        ldfd    f6, [sp], #8
        ldfd    f7, [sp], #8
        stmia   sp!, {r4,r5,r6,r7,r8,r9,pc}

    /* The trap handler */
.Ltrap_handler:
    /* Restore the global variables used by caml_c_call */
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_gc_regs
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
    /* Save young_ptr */
        ldr     r4, .Lyoung_ptr
        str     alloc_ptr, [r4, #0]
    /* Re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
        bl      mlraise         /* Keep retaddr in lr for debugging */
        /* never returns */

/* Raise an exception from C */

        .global raise_caml_exception
raise_caml_exception:
    /* Reload Caml allocation pointers */
        ldr     r1, .Lyoung_ptr
        ldr     alloc_ptr, [r1, #0]
        ldr     r1, .Lyoung_limit
        ldr     alloc_limit, [r1, #0]
    /* Say we're back into Caml */
        ldr     r1, .Lcaml_last_return_address
        mov     r2, #0
        str     r2, [r1, #0]
    /* Cut stack at current trap handler */
        ldr     r1, .Lcaml_exception_pointer
        ldr     sp, [r1, #0]
        ldr     trap_ptr, [sp, #0]
        ldr     r1, [sp, #4]
        add     sp, sp, #4
        mov     lr, pc          /* Keep retaddr in lr to help debugging */
        mov     pc, r1

/* Callback from C to Caml */

        .global callback
callback:
    /* Initial shuffling of arguments (r0 = closure, r1 = first arg) */
        mov     r9, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r9          /* r1 = closure environment */
        ldr     r9, [r9, #0]    /* code pointer */
        b       .Ljump_to_caml

        .global callback2
callback2:
    /* Initial shuffling of arguments (r0 = closure, r1 = arg1, r2 = arg2) */
        mov     r9, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r9          /* r2 = closure environment */
        ldr     r9, .Lcaml_apply2
        ldr     r9, [r9, #0]    /* code pointer */
        b       .Ljump_to_caml

        .global callback3
callback3:
    /* Initial shuffling of arguments */
    /* (r0 = closure, r1 = arg1, r2 = arg2, r3 = arg3) */
        mov     r9, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r3          /* r2 = third arg
        mov     r2, r9          /* r3 = closure environment */
        ldr     r9, .Lcaml_apply3
        ldr     r9, [r9, #0]    /* code pointer */
        b       .Ljump_to_caml

/* Global references */

.Lcaml_last_return_address:     .word caml_last_return_address
.Lcaml_bottom_of_stack:         .word caml_bottom_of_stack
.Lcaml_gc_regs:                 .word caml_gc_regs
.Lyoung_ptr:                    .word young_ptr
.Lyoung_limit:                  .word young_limit
.Lcaml_exception_pointer:       .word caml_exception_pointer
.Lcaml_program:                 .word caml_program
.LLtrap_handler:                .word .Ltrap_handler
.Lcaml_apply2:                  .word caml_apply2
.Lcaml_apply3:                  .word caml_apply3

/* GC roots for callback */

        .data

        .global system_frametable
system_frametable:
        .word   1               /* one descriptor */
        .word   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2
