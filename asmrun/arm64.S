/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, ARM processor, 64-bit mode */
/* Must be preprocessed by cpp */

/* Globals and labels */
#if defined(SYS_macosx)
#define G(sym) _##sym
#define L(lbl) L##lbl
#else
#define G(sym) sym
#define L(lbl) .L##lbl
#endif

/* Functions */

#if defined(SYS_macosx)
        .macro FUNCTION name
        .endm
        .macro OBJECT name
        .endm
#else
        .macro FUNCTION name
        .type  \name, %function
        .size  \name, .-\name
        .endm
        .macro OBJECT name
        .type  \name, %object
        .size  \name, .-\name
        .endm
#endif

/* Special registers */

#define TRAP_PTR x26
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

/* Support for profiling with gprof */

#define PROFILE

/* Macros to load and store global variables.  Destroy TMP2 */

#if defined(SYS_macosx)
        .macro ADDRGLOBAL reg, symb
        adrp    TMP2, \symb@GOTPAGE
        ldr     \reg, [TMP2, \symb@GOTPAGEOFF]
        .endm

        .macro LOADGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        ldr     \reg, [TMP2]
        .endm

        .macro STOREGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        str     \reg, [TMP2]
        .endm
#elif defined(__PIC__)
        .macro ADDRGLOBAL reg, symb
        adrp    TMP2, :got:\symb
        ldr     \reg, [TMP2, #:got_lo12:\symb]
        .endm

        .macro LOADGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        ldr     \reg, [TMP2]
        .endm

        .macro STOREGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        str     \reg, [TMP2]
        .endm
#else
        .macro ADDRGLOBAL reg, symb
        adrp    \reg, \symb
        ldr     \reg, \reg, #:lo12:symb
        .endm

        .macro LOADGLOBAL reg, symb
        adrp    TMP2, \symb
        ldr     \reg, [TMP2, #:lo12:\symb]
        .endm

        .macro STOREGLOBAL reg, symb
        adrp    TMP2, \symb
        str     \reg, [TMP2, #:lo12:\symb]
        .endm
#endif

/* Allocation functions and GC interface */

        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

        .align  2
        .globl  G(caml_call_gc)
G(caml_call_gc):
        CFI_STARTPROC
        PROFILE
    /* Record return address */
        STOREGLOBAL x30, G(caml_last_return_address)
    /* Record lowest stack address */
        mov     TMP, sp
        STOREGLOBAL TMP, G(caml_bottom_of_stack)
L(caml_call_gc):
    /* Set up stack space, saving return address and frame pointer */
    /* (2 regs RA/GP, 24 allocatable int regs, 24 caller-save float regs) * 8 */
        stp     x29, x30, [sp, #-400]!
        CFI_ADJUST(400)
        add     x29, sp, #0
    /* Save allocatable integer registers on the stack, in the order
       given in proc.ml */
        stp     x0, x1, [sp, #16]
        stp     x2, x3, [sp, #32]
        stp     x4, x5, [sp, #48]
        stp     x6, x7, [sp, #64]
        stp     x8, x9, [sp, #80]
        stp     x10, x11, [sp, #96]
        stp     x12, x13, [sp, #112]
        stp     x14, x15, [sp, #128]
        stp     x19, x20, [sp, #144]
        stp     x21, x22, [sp, #160]
        stp     x23, x24, [sp, #176]
        str     x25, [sp, #192]
     /* Save caller-save floating-point registers on the stack
        (callee-saves are preserved by caml_garbage_collection) */
        stp     d0, d1, [sp, #208]
        stp     d2, d3, [sp, #224]
        stp     d4, d5, [sp, #240]
        stp     d6, d7, [sp, #256]
        stp     d16, d17, [sp, #272]
        stp     d18, d19, [sp, #288]
        stp     d20, d21, [sp, #304]
        stp     d22, d23, [sp, #320]
        stp     d24, d25, [sp, #336]
        stp     d26, d27, [sp, #352]
        stp     d28, d29, [sp, #368]
        stp     d30, d31, [sp, #384]
    /* Store pointer to saved integer registers in caml_gc_regs */
        add     TMP, sp, #16
        STOREGLOBAL TMP, G(caml_gc_regs)
    /* Save current allocation pointer for debugging purposes */
        STOREGLOBAL ALLOC_PTR, G(caml_young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
        STOREGLOBAL TRAP_PTR, G(caml_exception_pointer)
    /* Call the garbage collector */
        bl      G(caml_garbage_collection)
    /* Restore registers */
        ldp     x0, x1, [sp, #16]
        ldp     x2, x3, [sp, #32]
        ldp     x4, x5, [sp, #48]
        ldp     x6, x7, [sp, #64]
        ldp     x8, x9, [sp, #80]
        ldp     x10, x11, [sp, #96]
        ldp     x12, x13, [sp, #112]
        ldp     x14, x15, [sp, #128]
        ldp     x19, x20, [sp, #144]
        ldp     x21, x22, [sp, #160]
        ldp     x23, x24, [sp, #176]
        ldr     x25, [sp, #192]
        ldp     d0, d1, [sp, #208]
        ldp     d2, d3, [sp, #224]
        ldp     d4, d5, [sp, #240]
        ldp     d6, d7, [sp, #256]
        ldp     d16, d17, [sp, #272]
        ldp     d18, d19, [sp, #288]
        ldp     d20, d21, [sp, #304]
        ldp     d22, d23, [sp, #320]
        ldp     d24, d25, [sp, #336]
        ldp     d26, d27, [sp, #352]
        ldp     d28, d29, [sp, #368]
        ldp     d30, d31, [sp, #384]
    /* Reload new allocation pointer and allocation limit */
        LOADGLOBAL ALLOC_PTR, G(caml_young_ptr)
        LOADGLOBAL ALLOC_LIMIT, G(caml_young_limit)
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp], #400
        ret
        CFI_ENDPROC
        FUNCTION G(caml_call_gc)

        .align  2
        .globl  G(caml_alloc1)
G(caml_alloc1):
        CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, #-16]!
        CFI_ADJUST(16)
    /* Record the lowest address of the caller's stack frame.  This is the address
       immediately above the pair of words (x29 and x30) we just pushed.  Those must
       not be included since otherwise the distance from [caml_bottom_of_stack] to the
       highest address in the caller's stack frame won't match the frame size contained
       in the relevant frame descriptor. */
        add     x29, sp, #16
        STOREGLOBAL x29, G(caml_bottom_of_stack)
        add     x29, sp, #0
    /* Record return address */
        STOREGLOBAL x30, G(caml_last_return_address)
    /* Call GC */
        bl      L(caml_call_gc)
    /* Restore return address */
        ldp     x29, x30, [sp], #16
        CFI_ADJUST(-16)
    /* Try again */
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_alloc1)

        .align  2
        .globl  G(caml_alloc2)
G(caml_alloc2):
        CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, #-16]!
        CFI_ADJUST(16)
    /* Record the lowest address of the caller's stack frame.  See comment above. */
        add     x29, sp, #16
        STOREGLOBAL x29, G(caml_bottom_of_stack)
        add     x29, sp, #0
    /* Record return address */
        STOREGLOBAL x30, G(caml_last_return_address)
    /* Call GC */
        bl      L(caml_call_gc)
    /* Restore return address */
        ldp     x29, x30, [sp], #16
        CFI_ADJUST(-16)
    /* Try again */
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_alloc2)

        .align  2
        .globl  G(caml_alloc3)
G(caml_alloc3):
        CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, #-16]!
        CFI_ADJUST(16)
    /* Record the lowest address of the caller's stack frame.  See comment above. */
        add     x29, sp, #16
        STOREGLOBAL x29, G(caml_bottom_of_stack)
        add     x29, sp, #0
    /* Record return address */
        STOREGLOBAL x30, G(caml_last_return_address)
    /* Call GC */
        bl      L(caml_call_gc)
    /* Restore return address */
        ldp     x29, x30, [sp], #16
        CFI_ADJUST(-16)
    /* Try again */
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_alloc3)

        .align  2
        .globl  G(caml_allocN)
G(caml_allocN):
        CFI_STARTPROC
        PROFILE
1:      sub     ALLOC_PTR, ALLOC_PTR, ARG
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, #-16]!
        CFI_ADJUST(16)
    /* Record the lowest address of the caller's stack frame.  See comment above. */
        add     x29, sp, #16
        STOREGLOBAL x29, G(caml_bottom_of_stack)
        add     x29, sp, #0
    /* Record return address */
        STOREGLOBAL x30, G(caml_last_return_address)
    /* Call GC.  This preserves ARG */
        bl      L(caml_call_gc)
    /* Restore return address */
        ldp     x29, x30, [sp], #16
        CFI_ADJUST(-16)
    /* Try again */
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_allocN)

/* Call a C function from OCaml */
/* Function to call is in ARG */

        .align  2
        .globl  G(caml_c_call)
G(caml_c_call):
        CFI_STARTPROC
        PROFILE
    /* Preserve return address in callee-save register x19 */
        mov     x19, x30
    /* Record lowest stack address and return address */
        STOREGLOBAL x30, G(caml_last_return_address)
        add     TMP, sp, #0
        STOREGLOBAL TMP, G(caml_bottom_of_stack)
    /* Make the exception handler alloc ptr available to the C code */
        STOREGLOBAL ALLOC_PTR, G(caml_young_ptr)
        STOREGLOBAL TRAP_PTR, G(caml_exception_pointer)
    /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
        LOADGLOBAL ALLOC_PTR, G(caml_young_ptr)
        LOADGLOBAL ALLOC_LIMIT, G(caml_young_limit)
    /* Return */
        ret     x19
        CFI_ENDPROC
        FUNCTION G(caml_c_call)

/* Start the OCaml program */

        .align  2
        .globl  G(caml_start_program)
G(caml_start_program):
        CFI_STARTPROC
        PROFILE
        ADDRGLOBAL ARG, G(caml_program)

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in ARG */
/* Arguments to the OCaml code are in x0...x7 */

L(jump_to_caml):
    /* Set up stack frame and save callee-save registers */
        stp     x29, x30, [sp, #-160]!
        CFI_ADJUST(160)
        add     x29, sp, #0
        stp     x19, x20, [sp, #16]
        stp     x21, x22, [sp, #32]
        stp     x23, x24, [sp, #48]
        stp     x25, x26, [sp, #64]
        stp     x27, x28, [sp, #80]
        stp     d8, d9, [sp, #96]
        stp     d10, d11, [sp, #112]
        stp     d12, d13, [sp, #128]
        stp     d14, d15, [sp, #144]
    /* Setup a callback link on the stack */
        LOADGLOBAL x8, G(caml_bottom_of_stack)
        LOADGLOBAL x9, G(caml_last_return_address)
        LOADGLOBAL x10, G(caml_gc_regs)
        stp     x8, x9, [sp, #-32]!     /* 16-byte alignment */
        CFI_ADJUST(32)
        str     x10, [sp, #16]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        LOADGLOBAL x8, G(caml_exception_pointer)
        adr     x9, L(trap_handler)
        stp     x8, x9, [sp, #-16]!
        CFI_ADJUST(16)
        add     TRAP_PTR, sp, #0
    /* Reload allocation pointers */
        LOADGLOBAL ALLOC_PTR, G(caml_young_ptr)
        LOADGLOBAL ALLOC_LIMIT, G(caml_young_limit)
    /* Call the OCaml code */
        blr     ARG
L(caml_retaddr):
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     x8, [sp], #16
        CFI_ADJUST(-16)
        STOREGLOBAL x8, G(caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
L(return_result):
        ldr     x10, [sp, #16]
        ldp     x8, x9, [sp], #32
        CFI_ADJUST(-32)
        STOREGLOBAL x8, G(caml_bottom_of_stack)
        STOREGLOBAL x9, G(caml_last_return_address)
        STOREGLOBAL x10, G(caml_gc_regs)
    /* Update allocation pointer */
        STOREGLOBAL ALLOC_PTR, G(caml_young_ptr)
    /* Reload callee-save registers and return address */
        ldp     x19, x20, [sp, #16]
        ldp     x21, x22, [sp, #32]
        ldp     x23, x24, [sp, #48]
        ldp     x25, x26, [sp, #64]
        ldp     x27, x28, [sp, #80]
        ldp     d8, d9, [sp, #96]
        ldp     d10, d11, [sp, #112]
        ldp     d12, d13, [sp, #128]
        ldp     d14, d15, [sp, #144]
        ldp     x29, x30, [sp], #160
        CFI_ADJUST(-160)
    /* Return to C caller */
        ret
        CFI_ENDPROC
        FUNCTION L(caml_retaddr)
        FUNCTION G(caml_start_program)

/* The trap handler */

        .align  2
L(trap_handler):
        CFI_STARTPROC
    /* Save exception pointer */
        STOREGLOBAL TRAP_PTR, G(caml_exception_pointer)
    /* Encode exception bucket as an exception result */
        orr     x0, x0, #2
    /* Return it */
        b       L(return_result)
        CFI_ENDPROC
        FUNCTION L(trap_handler)

/* Raise an exception from OCaml */

        .align  2
        .globl  G(caml_raise_exn)
G(caml_raise_exn):
        CFI_STARTPROC
        PROFILE
    /* Test if backtrace is active */
        LOADGLOBAL TMP, G(caml_backtrace_active)
        cbnz     TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, #8]
        ldr     TRAP_PTR, [sp], #16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                               /* arg1: exn bucket, already in x0 */
        mov     x1, x30        /* arg2: pc of raise */
        add     x2, sp, #0     /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      G(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_raise_exn)

/* Raise an exception from C */

        .align  2
        .globl  G(caml_raise_exception)
G(caml_raise_exception):
        CFI_STARTPROC
        PROFILE
    /* Reload trap ptr, alloc ptr and alloc limit */
        LOADGLOBAL TRAP_PTR, G(caml_exception_pointer)
        LOADGLOBAL ALLOC_PTR, G(caml_young_ptr)
        LOADGLOBAL ALLOC_LIMIT, G(caml_young_limit)
    /* Test if backtrace is active */
        LOADGLOBAL TMP, G(caml_backtrace_active)
        cbnz    TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, #8]
        ldr     TRAP_PTR, [sp], #16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                               /* arg1: exn bucket, already in x0 */
        LOADGLOBAL x1, G(caml_last_return_address)   /* arg2: pc of raise */
        LOADGLOBAL x2, G(caml_bottom_of_stack)       /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      G(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        FUNCTION G(caml_raise_exception)

/* Callback from C to OCaml */

        .align  2
        .globl  G(caml_callback_exn)
G(caml_callback_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments (x0 = closure, x1 = first arg) */
        mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, TMP         /* x1 = closure environment */
        ldr     ARG, [TMP]      /* code pointer */
        b       L(jump_to_caml)
        CFI_ENDPROC
        FUNCTION G(caml_callback_exn)

        .align  2
        .globl  G(caml_callback2_exn)
G(caml_callback2_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments (x0 = closure, x1 = arg1, x2 = arg2) */
        mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, x2          /* x1 = second arg */
        mov     x2, TMP         /* x2 = closure environment */
        ADDRGLOBAL ARG, G(caml_apply2)
        b       L(jump_to_caml)
        CFI_ENDPROC
        FUNCTION G(caml_callback2_exn)

        .align  2
        .globl  G(caml_callback3_exn)
G(caml_callback3_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments */
    /* (x0 = closure, x1 = arg1, x2 = arg2, x3 = arg3) */
        mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, x2          /* x1 = second arg */
        mov     x2, x3          /* x2 = third arg */
        mov     x3, TMP         /* x3 = closure environment */
        ADDRGLOBAL ARG, G(caml_apply3)
        b       L(jump_to_caml)
        CFI_ENDPROC
        FUNCTION G(caml_callback3_exn)

        .align  2
        .globl  G(caml_ml_array_bound_error)
G(caml_ml_array_bound_error):
        CFI_STARTPROC
        PROFILE
    /* Load address of [caml_array_bound_error] in ARG */
        ADDRGLOBAL ARG, G(caml_array_bound_error)
    /* Call that function */
        b       G(caml_c_call)
        CFI_ENDPROC
        FUNCTION G(caml_ml_array_bound_error)

        .globl  G(caml_system__code_end)
G(caml_system__code_end):

/* GC roots for callback */

        .data
        .align  3
        .globl  G(caml_system__frametable)
G(caml_system__frametable):
        .quad   1               /* one descriptor */
        .quad   L(caml_retaddr) /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        OBJECT G(caml_system__frametable)
