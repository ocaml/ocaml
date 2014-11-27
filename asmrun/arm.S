/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Benedikt Meurer, University of Siegen                  */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*   Copyright 2012 Benedikt Meurer.                                      */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, ARM processor */
/* Must be preprocessed by cpp */

        .syntax unified
        .text
#if defined(SYS_macosx)
#define Glo(s) _##s
#define Loc(s) L##s
#define P(s) LP##s
#if defined(MODEL_armv6)
        .arm
        .macro  .funtype
        .endm
        .macro  cbz
        cmp     $0, #0
        beq     $1
        .endm
        /* load global address, non-PIE */
        .macro ldgaddr
        ldr $0, P($1)
        .endm
        /* indirection for global address, non-PIE */
        .macro gaddr
P($0):  .word Glo($0)
        .endm
        /* load local address, non-PIE */
        .macro ldladdr
        ldr $0, P($1)
        .endm
        /* indirection for local address, non-PIE */
        .macro laddr
P($0):  .word Loc($0)
        .endm
#else
        /* for ARM>=7 we can easily support PIE because we have movw and movt */
        .thumb
        Lpcreloffs = 4     /* for .arm: 8 */
        .macro .funtype
        .thumb_func $0
        .endm
        /* load global address, PIE */
        .macro ldgaddr
        movw $0, #:lower16:(P($1) - (9f + Lpcreloffs))
        movt $0, #:upper16:(P($1) - (9f + Lpcreloffs))
9:
        add $0, pc               /* actually $0 := $0 + pc + Lpcreloffs */
        ldr $0, [$0]
        .endm
        /* indirection for global address, PIE */
        .macro gaddr
P($0):  .indirect_symbol Glo($0)
        .long 0
        .endm
        /* load local address, PIE */
        .macro ldladdr
        movw $0, #:lower16:(Loc($1) - (9f + Lpcreloffs))
        movt $0, #:upper16:(Loc($1) - (9f + Lpcreloffs))
9:
        add $0, pc               /* actually $0 := $0 + pc + Lpcreloffs */
        .endm
        /* indirection for local address, PIE */
        .macro laddr
        .endm
#endif
        .macro  .type
        .endm
        .macro  .size
        .endm
#endif
#if defined(SYS_linux_eabihf) && defined(MODEL_armv6)
        .arch   armv6
        .fpu    vfpv2
        .arm

    /* Compatibility macros */
        .macro  cbz reg, lbl
        cmp     \reg, #0
        beq     \lbl
        .endm
#elif defined(SYS_linux_eabihf)
        .arch   armv7-a
        .fpu    vfpv3-d16
        .thumb
#elif defined(SYS_linux_eabi)
        .arch   armv4t
        .arm

    /* Compatibility macros */
        .macro  blx reg
        mov     lr, pc
        bx      \reg
        .endm
        .macro  cbz reg, lbl
        cmp     \reg, #0
        beq     \lbl
        .endm
#elif defined(SYS_netbsd)

  #if defined(MODEL_armv6)
        .arch   armv6
        .fpu    vfpv2
        .arm

    /* Compatibility macros */
        .macro  cbz reg, lbl
        cmp     \reg, #0
        beq     \lbl
        .endm
  #elif defined(MODEL_armv7)
        .arch   armv7-a
        .fpu    vfpv3-d16
        .thumb
  #else
    #error "Only NetBSD eabihf supported"
  #endif

#elif defined(SYS_freebsd)
        .arch   armv6
        .arm

    /* Compatibility macros */
        .macro  cbz reg, lbl
        cmp     \reg, #0
        beq     \lbl
        .endm
#endif
#if !defined(SYS_macosx)
#define Glo(s) s
#define Loc(s) .L##s
#define P(s) .LP##s
        .macro  .funtype symbol
        .type  \symbol, %function
        .endm
        /* load global address, non-PIE GNU */
        .macro  ldgaddr dest, src
        ldr \dest, =Glo(\src)
        .endm
        .macro gaddr ref
        .endm
        /* load local address, non-PIE GNU */
        .macro  ldladdr dest, src
        ldr \dest, =Loc(\src)
        .endm
        .macro laddr ref
        .endm
#endif

trap_ptr        .req    r8
alloc_ptr       .req    r10
alloc_limit     .req    r11

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

#if defined(PROFILING) && (defined(SYS_linux_eabihf) \
        || defined(SYS_linux_eabi) \
        || defined(SYS_netbsd))
#define PROFILE \
        push    {lr}; CFI_ADJUST(4); \
        bl      __gnu_mcount_nc; CFI_ADJUST(-4)
#else
#define PROFILE
#endif

/* Allocation functions and GC interface */

        .globl  Glo(caml_system__code_begin)
Glo(caml_system__code_begin):

        .align  2
        .globl  Glo(caml_call_gc)
Glo(caml_call_gc):
        CFI_STARTPROC
        PROFILE
    /* Record return address */
        ldgaddr r12, caml_last_return_address
        str     lr, [r12]
Loc(caml_call_gc):
    /* Record lowest stack address */
        ldgaddr r12, caml_bottom_of_stack
        str     sp, [r12]
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd) || defined(SYS_macosx)
    /* Save caller floating-point registers on the stack */
        vpush   {d0-d7}; CFI_ADJUST(64)
#endif
    /* Save integer registers and return address on the stack */
        push    {r0-r7,r12,lr}; CFI_ADJUST(40)
    /* Store pointer to saved integer registers in caml_gc_regs */
        ldgaddr r12, caml_gc_regs
        str     sp, [r12]
    /* Save current allocation pointer for debugging purposes */
        ldgaddr alloc_limit, caml_young_ptr
        str     alloc_ptr, [alloc_limit]
    /* Save trap pointer in case an exception is raised during GC */
        ldgaddr r12, caml_exception_pointer
        str     trap_ptr, [r12]
    /* Call the garbage collector */
        bl      Glo(caml_garbage_collection)
    /* Restore integer registers and return address from the stack */
        pop     {r0-r7,r12,lr}; CFI_ADJUST(-40)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd) || defined(SYS_macosx)
    /* Restore floating-point registers from the stack */
        vpop    {d0-d7}; CFI_ADJUST(-64)
#endif
    /* Reload new allocation pointer and limit */
    /* alloc_limit still points to caml_young_ptr */
        ldgaddr r12, caml_young_limit
        ldr     alloc_ptr, [alloc_limit]
        ldr     alloc_limit, [r12]
    /* Return to caller */
        bx      lr
        CFI_ENDPROC
        .funtype Glo(caml_call_gc)
        .size    Glo(caml_call_gc), .-Glo(caml_call_gc)

        .align  2
        .globl  Glo(caml_alloc1)
Glo(caml_alloc1):
        CFI_STARTPROC
        PROFILE
Loc(caml_alloc1):
        sub     alloc_ptr, alloc_ptr, 8
        cmp     alloc_ptr, alloc_limit
        bcc     1f
        bx      lr
1:  /* Record return address */
        ldgaddr r7, caml_last_return_address
        str     lr, [r7]
    /* Call GC (preserves r7) */
        bl      Loc(caml_call_gc)
    /* Restore return address */
        ldr     lr, [r7]
    /* Try again */
        b       Loc(caml_alloc1)
        CFI_ENDPROC
        .funtype Glo(caml_alloc1)
        .size    Glo(caml_alloc1), .-Glo(caml_alloc1)

        .align  2
        .globl  Glo(caml_alloc2)
Glo(caml_alloc2):
        CFI_STARTPROC
        PROFILE
Loc(caml_alloc2):
        sub     alloc_ptr, alloc_ptr, 12
        cmp     alloc_ptr, alloc_limit
        bcc     1f
        bx      lr
1:  /* Record return address */
        ldgaddr r7, caml_last_return_address
        str     lr, [r7]
    /* Call GC (preserves r7) */
        bl      Loc(caml_call_gc)
    /* Restore return address */
        ldr     lr, [r7]
    /* Try again */
        b       Loc(caml_alloc2)
        CFI_ENDPROC
        .funtype Glo(caml_alloc2)
        .size    Glo(caml_alloc2), .-Glo(caml_alloc2)

        .align  2
        .globl  Glo(caml_alloc3)
Glo(caml_alloc3):
        CFI_STARTPROC
        PROFILE
Loc(caml_alloc3):
        sub     alloc_ptr, alloc_ptr, 16
        cmp     alloc_ptr, alloc_limit
        bcc     1f
        bx      lr
1:  /* Record return address */
        ldgaddr r7, caml_last_return_address
        str     lr, [r7]
    /* Call GC (preserves r7) */
        bl      Loc(caml_call_gc)
    /* Restore return address */
        ldr     lr, [r7]
    /* Try again */
        b       Loc(caml_alloc3)
        CFI_ENDPROC
        .funtype Glo(caml_alloc3)
        .size    Glo(caml_alloc3), .-Glo(caml_alloc3)

        .align  2
        .globl  Glo(caml_allocN)
Glo(caml_allocN):
        CFI_STARTPROC
        PROFILE
Loc(caml_allocN):
        sub     alloc_ptr, alloc_ptr, r7
        cmp     alloc_ptr, alloc_limit
        bcc     1f
        bx      lr
1:  /* Record return address */
        ldgaddr r12, caml_last_return_address
        str     lr, [r12]
    /* Call GC (preserves r7) */
        bl      Loc(caml_call_gc)
    /* Restore return address */
        ldgaddr r12, caml_last_return_address
        ldr     lr, [r12]
    /* Try again */
        b       Loc(caml_allocN)
        CFI_ENDPROC
        .funtype Glo(caml_allocN)
        .size    Glo(caml_allocN), .-Glo(caml_allocN)

/* Call a C function from OCaml */
/* Function to call is in r7 */

        .align  2
        .globl  Glo(caml_c_call)
Glo(caml_c_call):
        CFI_STARTPROC
        PROFILE
    /* Record lowest stack address and return address */
        ldgaddr r5, caml_last_return_address
        ldgaddr r6, caml_bottom_of_stack
        str     lr, [r5]
        str     sp, [r6]
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
    /* Make the exception handler alloc ptr available to the C code */
        ldgaddr r5, caml_young_ptr
        ldgaddr r6, caml_exception_pointer
        str     alloc_ptr, [r5]
        str     trap_ptr, [r6]
    /* Call the function */
        blx     r7
    /* Reload alloc ptr and alloc limit */
        ldgaddr r6, caml_young_limit
        ldr     alloc_ptr, [r5]         /* r5 still points to caml_young_ptr */
        ldr     alloc_limit, [r6]
    /* Return */
        bx      r4
        CFI_ENDPROC
        .funtype Glo(caml_c_call)
        .size    Glo(caml_c_call), .-Glo(caml_c_call)

/* Start the OCaml program */

        .align  2
        .globl  Glo(caml_start_program)
Glo(caml_start_program):
        CFI_STARTPROC
        PROFILE
        ldgaddr r12, caml_program

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in r12 */
/* Arguments to the OCaml code are in r0...r3 */

Loc(jump_to_caml):
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd) || defined(SYS_macosx)
    /* Save callee-save floating-point registers */
        vpush   {d8-d15}; CFI_ADJUST(64)
#endif
    /* Save return address and callee-save registers */
        push    {r4-r8,r10,r11,lr}; CFI_ADJUST(32)      /* 8-byte alignment */
    /* Setup a callback link on the stack */
        sub     sp, sp, 16; CFI_ADJUST(16)              /* 8-byte alignment */
        ldgaddr r4, caml_bottom_of_stack
        ldgaddr r5, caml_last_return_address
        ldgaddr r6, caml_gc_regs
        ldr     r4, [r4]
        ldr     r5, [r5]
        ldr     r6, [r6]
        str     r4, [sp, 0]
        str     r5, [sp, 4]
        str     r6, [sp, 8]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        sub     sp, sp, 8; CFI_ADJUST(8)
        ldgaddr r6, caml_exception_pointer
        ldladdr r5, trap_handler
        ldr     r4, [r6]
        str     r4, [sp, 0]
        str     r5, [sp, 4]
        mov     trap_ptr, sp
    /* Reload allocation pointers */
        ldgaddr r4, caml_young_ptr
        ldr     alloc_ptr, [r4]
        ldgaddr r4, caml_young_limit
        ldr     alloc_limit, [r4]
    /* Call the OCaml code */
        blx     r12
Loc(caml_retaddr):
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldgaddr r4, caml_exception_pointer
        ldr     r5, [sp, 0]
        str     r5, [r4]
        add     sp, sp, 8; CFI_ADJUST(-8)
    /* Pop the callback link, restoring the global variables */
Loc(return_result):
        ldgaddr r4, caml_bottom_of_stack
        ldr     r5, [sp, 0]
        str     r5, [r4]
        ldgaddr r4, caml_last_return_address
        ldr     r5, [sp, 4]
        str     r5, [r4]
        ldgaddr r4, caml_gc_regs
        ldr     r5, [sp, 8]
        str     r5, [r4]
        add     sp, sp, 16; CFI_ADJUST(-16)
    /* Update allocation pointer */
        ldgaddr r4, caml_young_ptr
        str     alloc_ptr, [r4]
    /* Reload callee-save registers and return address */
        pop     {r4-r8,r10,r11,lr}; CFI_ADJUST(-32)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd) || defined(SYS_macosx)
    /* Reload callee-save floating-point registers */
        vpop    {d8-d15}; CFI_ADJUST(-64)
#endif
        bx      lr
        CFI_ENDPROC
        .funtype Loc(caml_retaddr)
        .size    Loc(caml_retaddr), .-Loc(caml_retaddr)
        .funtype Glo(caml_start_program)
        .size    Glo(caml_start_program), .-Glo(caml_start_program)

/* The trap handler */

        .align  2
Loc(trap_handler):
        CFI_STARTPROC
    /* Save exception pointer */
        ldgaddr r12, caml_exception_pointer
        str     trap_ptr, [r12]
    /* Encode exception bucket as an exception result */
        orr     r0, r0, 2
    /* Return it */
        b       Loc(return_result)
        CFI_ENDPROC
        .funtype Loc(trap_handler)
        .size    Loc(trap_handler), .-Loc(trap_handler)

/* Raise an exception from OCaml */

        .align  2
        .globl  Glo(caml_raise_exn)
Glo(caml_raise_exn):
        CFI_STARTPROC
        PROFILE
    /* Test if backtrace is active */
        ldgaddr r1, caml_backtrace_active
        ldr     r1, [r1]
        cbz     r1, 1f
    /* Preserve exception bucket in callee-save register r4 */
        mov     r4, r0
    /* Stash the backtrace */
        mov     r1, lr                          /* arg2: pc of raise */
        mov     r2, sp                          /* arg3: sp of raise */
        mov     r3, trap_ptr                    /* arg4: sp of handler */
        bl      Glo(caml_stash_backtrace)
    /* Restore exception bucket */
        mov     r0, r4
1:  /* Cut stack at current trap handler */
        mov     sp, trap_ptr
    /* Pop previous handler and addr of trap, and jump to it */
        pop     {trap_ptr, pc}
        CFI_ENDPROC
        .funtype Glo(caml_raise_exn)
        .size    Glo(caml_raise_exn), .-Glo(caml_raise_exn)

/* Raise an exception from C */

        .align  2
        .globl  Glo(caml_raise_exception)
Glo(caml_raise_exception):
        CFI_STARTPROC
        PROFILE
    /* Reload trap ptr, alloc ptr and alloc limit */
        ldgaddr trap_ptr, caml_exception_pointer
        ldgaddr alloc_ptr, caml_young_ptr
        ldgaddr alloc_limit, caml_young_limit
        ldr     trap_ptr, [trap_ptr]
        ldr     alloc_ptr, [alloc_ptr]
        ldr     alloc_limit, [alloc_limit]
    /* Test if backtrace is active */
        ldgaddr r1, caml_backtrace_active
        ldr     r1, [r1]
        cbz     r1, 1f
    /* Preserve exception bucket in callee-save register r4 */
        mov     r4, r0
        ldgaddr r1, caml_last_return_address   /* arg2: pc of raise */
        ldr     r1, [r1]
        ldgaddr r2, caml_bottom_of_stack       /* arg3: sp of raise */
        ldr     r2, [r2]
        mov     r3, trap_ptr                    /* arg4: sp of handler */
        bl      Glo(caml_stash_backtrace)
    /* Restore exception bucket */
        mov     r0, r4
1:  /* Cut stack at current trap handler */
        mov     sp, trap_ptr
    /* Pop previous handler and addr of trap, and jump to it */
        pop     {trap_ptr, pc}
        CFI_ENDPROC
        .funtype Glo(caml_raise_exception)
        .size    Glo(caml_raise_exception), .-Glo(caml_raise_exception)

/* Callback from C to OCaml */

        .align  2
        .globl  Glo(caml_callback_exn)
Glo(caml_callback_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments (r0 = closure, r1 = first arg) */
        mov     r12, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r12         /* r1 = closure environment */
        ldr     r12, [r12]      /* code pointer */
        b       Loc(jump_to_caml)
        CFI_ENDPROC
        .funtype Glo(caml_callback_exn)
        .size    Glo(caml_callback_exn), .-Glo(caml_callback_exn)

        .align  2
        .globl  Glo(caml_callback2_exn)
Glo(caml_callback2_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments (r0 = closure, r1 = arg1, r2 = arg2) */
        mov     r12, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r12         /* r2 = closure environment */
        ldgaddr r12, caml_apply2
        b       Loc(jump_to_caml)
        CFI_ENDPROC
        .funtype Glo(caml_callback2_exn)
        .size    Glo(caml_callback2_exn), .-Glo(caml_callback2_exn)

        .align  2
        .globl  Glo(caml_callback3_exn)
Glo(caml_callback3_exn):
        CFI_STARTPROC
        PROFILE
    /* Initial shuffling of arguments */
    /* (r0 = closure, r1 = arg1, r2 = arg2, r3 = arg3) */
        mov     r12, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r3          /* r2 = third arg */
        mov     r3, r12         /* r3 = closure environment */
        ldgaddr r12, caml_apply3
        b       Loc(jump_to_caml)
        CFI_ENDPROC
        .funtype Glo(caml_callback3_exn)
        .size    Glo(caml_callback3_exn), .-Glo(caml_callback3_exn)

        .align  2
        .globl  Glo(caml_ml_array_bound_error)
Glo(caml_ml_array_bound_error):
        CFI_STARTPROC
        PROFILE
    /* Load address of [caml_array_bound_error] in r7 */
        ldgaddr r7, caml_array_bound_error
    /* Call that function */
        b       Glo(caml_c_call)
        CFI_ENDPROC
        .funtype Glo(caml_ml_array_bound_error)
        .size    Glo(caml_ml_array_bound_error), .-Glo(caml_ml_array_bound_error)

/* Indirection block for macosx */
#if defined(SYS_macosx)
#if !defined(MODEL_armv6)
        .non_lazy_symbol_pointer
#endif
        .align 2
         gaddr caml_last_return_address
         gaddr caml_bottom_of_stack
         gaddr caml_gc_regs
         gaddr caml_young_ptr
         gaddr caml_exception_pointer
         gaddr caml_young_limit
         gaddr caml_program
         gaddr caml_backtrace_active
         gaddr caml_apply2
         gaddr caml_apply3
         gaddr caml_array_bound_error
         laddr trap_handler
#endif

        .globl  Glo(caml_system__code_end)
Glo(caml_system__code_end):

/* GC roots for callback */

        .data
        .align  2
        .globl  Glo(caml_system__frametable)
Glo(caml_system__frametable):
        .word   1               /* one descriptor */
        .word   Loc(caml_retaddr)  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2
#ifndef SYS_macosx
        .type   caml_system__frametable, %object
        .size   caml_system__frametable, .-caml_system__frametable

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
#endif
