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

#include "caml/m.h"

        .syntax unified
        .text
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

trap_ptr          .req    r8
alloc_ptr         .req    r10
domain_state_ptr  .req    r11

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

#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif

#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .align 2; \
        .globl name; \
        .type name, %function; \
name:

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  caml_hot__code_begin
caml_hot__code_begin:

        TEXT_SECTION(caml_hot__code_end)
        .globl  caml_hot__code_end
caml_hot__code_end:
#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state_var(var) [domain_state_ptr, 8*domain_field_caml_##var]

/* Allocation functions and GC interface */
        TEXT_SECTION(caml_system__code_begin)
        .globl  caml_system__code_begin
caml_system__code_begin:

FUNCTION(caml_call_gc)
        CFI_STARTPROC
.Lcaml_call_gc:
    /* Record return address */
        str     lr, Caml_state_var(last_return_address)
    /* Record lowest stack address */
        str     sp, Caml_state_var(bottom_of_stack)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
    /* Save caller floating-point registers on the stack */
        vpush   {d0-d7}; CFI_ADJUST(64)
#endif
    /* Save integer registers and return address on the stack */
        push    {r0-r7,r12,lr}; CFI_ADJUST(40)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
        CFI_OFFSET(lr, -68)
#else
        CFI_OFFSET(lr, -4)
#endif
    /* Store pointer to saved integer registers in Caml_state->gc_regs */
        str     sp, Caml_state_var(gc_regs)
    /* Save current allocation pointer for debugging purposes */
        str     alloc_ptr, Caml_state_var(young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
        str     trap_ptr, Caml_state_var(exception_pointer)
    /* Call the garbage collector */
        bl      caml_garbage_collection
    /* Restore integer registers and return address from the stack */
        pop     {r0-r7,r12,lr}; CFI_ADJUST(-40)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
    /* Restore floating-point registers from the stack */
        vpop    {d0-d7}; CFI_ADJUST(-64)
#endif
    /* Reload new allocation pointer */
        ldr     alloc_ptr, Caml_state_var(young_ptr)
    /* Return to caller */
        bx      lr
        CFI_ENDPROC
        .size   caml_call_gc, .-caml_call_gc

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        sub     alloc_ptr, alloc_ptr, 8
        ldr     r7, Caml_state_var(young_limit)
        cmp     alloc_ptr, r7
        bcc     .Lcaml_call_gc
        bx      lr
        CFI_ENDPROC
        .size   caml_alloc1, .-caml_alloc1

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        sub     alloc_ptr, alloc_ptr, 12
        ldr     r7, Caml_state_var(young_limit)
        cmp     alloc_ptr, r7
        bcc     .Lcaml_call_gc
        bx      lr
        CFI_ENDPROC
        .size   caml_alloc2, .-caml_alloc2

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        sub     alloc_ptr, alloc_ptr, 16
        ldr     r7, Caml_state_var(young_limit)
        cmp     alloc_ptr, r7
        bcc     .Lcaml_call_gc
        bx      lr
        CFI_ENDPROC
        .size   caml_alloc3, .-caml_alloc3

FUNCTION(caml_allocN)
        CFI_STARTPROC
        sub     alloc_ptr, alloc_ptr, r7
        ldr     r7, Caml_state_var(young_limit)
        cmp     alloc_ptr, r7
        bcc     .Lcaml_call_gc
        bx      lr
        CFI_ENDPROC
        .size   caml_allocN, .-caml_allocN

/* Call a C function from OCaml */
/* Function to call is in r7 for non-PIC code, or is a
   named PLT function for PIC code */
/* A PLT function address is only available when "cited by a BL-class
   relocation directive" like `bl func(PLT)`. We can't, for example, do
   `adr r7, func(PLT)` and then jump to caml_c_call or Ljump_to_caml. So
   implemented as a macro that inserts the instructions inline and branch jumps
   (branch_op = bl or blx) to (c_code_addr or caml_code_funcptr). Use blx for
   r7/r12 register and bl for named PLT functions.
   Warning: Unlike blx the bl instruction does not switch to/from Thumb state
   since "it is generally pointless trying to construct a PLT entry entirely in
   16-bit Thumb instructions". In other words: Do not emit Thumb PLT entries
   for PIC code!
   Confer: https://github.com/ARM-software/abi-aa/blob/
   320a56971fdcba282b7001cf4b84abb4fd993131/aaelf32/
   aaelf32.rst#plt-sequences-and-usage-models
*/

        .macro inline_caml_c_call branch_op c_code_addr
    /* Record lowest stack address and return address */
        str     lr, Caml_state_var(last_return_address)
        str     sp, Caml_state_var(bottom_of_stack)
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
        CFI_REGISTER(lr, r4)
    /* Make the exception handler alloc ptr available to the C code */
        str     alloc_ptr, Caml_state_var(young_ptr)
        str     trap_ptr, Caml_state_var(exception_pointer)
    /* Call the function */
        \branch_op \c_code_addr
    /* Reload alloc ptr */
        ldr     alloc_ptr, Caml_state_var(young_ptr)
    /* Return */
        bx      r4
        .endm

FUNCTION(caml_c_call)
        CFI_STARTPROC
        inline_caml_c_call blx r7
        CFI_ENDPROC
        .size   caml_c_call, .-caml_c_call

/* Start the OCaml program */

/* Code shared with caml_callback* */
/* Arguments to the OCaml code are in r0...r3 */
/* Instructions for using call_caml_XXXX:
        1. (optional) set r12
        2. `call_caml_enter`
        3. `blx some_function(PLT)` ...or if you preset r12... `bx r12`
        4. `call_caml_exit`

        At end inside the CFI PROC do `call_caml_relocs`.
                Why? Linker will do PIC relocations in this word space.

        Add `call_caml_framedescr` in the PIC portion of
        caml_system__frametable.
        And increment caml_system__frametable.num_descriptors.
                Why? All callsite return addresses have to be visible to GC.
*/
        .macro call_caml_enter callsite:req
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
    /* Save callee-save floating-point registers */
        vpush   {d8-d15}; CFI_ADJUST(64)
#endif
    /* Save return address and callee-save registers */
        push    {r4-r8,r10,r11,lr}; CFI_ADJUST(32)      /* 8-byte alignment */
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
        CFI_OFFSET(lr, -68)
#else
        CFI_OFFSET(lr, -4)
#endif
#ifdef __PIC__
    /* Read the PREL word in the local .text section, which is the PC-relative
       address of Caml_state's GOT entry */
        ldr.w   domain_state_ptr, \callsite\()_isp
    /* Label for PC of next add instruction */
\callsite\()_pcnai: /* `.Lxyz_pcnai:` if callsite=`.Lxyz` */
        add.w   domain_state_ptr, pc
    /* Load the entry from the GOT; the address of Caml_state variable */
        ldr     domain_state_ptr, [domain_state_ptr]
#else
    /* Load the address of Caml_state variable */
        ldr     domain_state_ptr, =Caml_state
#endif
    /* Load the Caml_state pointer */
        ldr     domain_state_ptr, [domain_state_ptr]
    /* Setup a callback link on the stack */
        sub     sp, sp, 16; CFI_ADJUST(16)              /* 8-byte alignment */
        ldr     r4, Caml_state_var(bottom_of_stack)
        ldr     r5, Caml_state_var(last_return_address)
        ldr     r6, Caml_state_var(gc_regs)
        str     r4, [sp, 0]
        str     r5, [sp, 4]
        str     r6, [sp, 8]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        sub     sp, sp, 8; CFI_ADJUST(8)
        adr     r5, .Ltrap_handler
        ldr     r4, Caml_state_var(exception_pointer)
        str     r4, [sp, 0]
        str     r5, [sp, 4]
        mov     trap_ptr, sp
    /* Reload allocation pointer */
        ldr     alloc_ptr, Caml_state_var(young_ptr)
        .endm

        .macro  call_caml_exit retaddr_label:req return_result_label:req
\retaddr_label:
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     r5, [sp, 0]
        str     r5, Caml_state_var(exception_pointer)
        add     sp, sp, 8; CFI_ADJUST(-8)
    /* Pop the callback link, restoring the global variables */
\return_result_label:
        ldr     r5, [sp, 0]
        str     r5, Caml_state_var(bottom_of_stack)
        ldr     r5, [sp, 4]
        str     r5, Caml_state_var(last_return_address)
        ldr     r5, [sp, 8]
        str     r5, Caml_state_var(gc_regs)
        add     sp, sp, 16; CFI_ADJUST(-16)
    /* Update allocation pointer */
        str     alloc_ptr, Caml_state_var(young_ptr)
    /* Reload callee-save registers and return address */
        pop     {r4-r8,r10,r11,lr}; CFI_ADJUST(-32)
#if defined(SYS_linux_eabihf) || defined(SYS_netbsd)
    /* Reload callee-save floating-point registers */
        vpop    {d8-d15}; CFI_ADJUST(-64)
#endif
        bx      lr
        .endm

        .macro call_caml_relocs callsite:req
#ifdef __PIC__
        .align  2
    /* Local indirect symbol pointer for \label that will be linked statically
       with the correct GOT math. */
\callsite\()_isp: /* `.Lxyz_isp:` if callsite=`.Lxyz` */
    /* 8 = opcode_size( ldr.w ; \callsite\()_pcnai: adr.w ) */
        .word	Caml_state(GOT_PREL)-((\callsite\()_pcnai+8)-\callsite\()_isp)
#endif
        .endm

        .macro call_caml_framedescr retaddr_label:req
#ifdef __PIC__
        .word	  \retaddr_label  /* return address into callback */
        .short  0               /* zero frame size */
        .short  0               /* no roots */
        .align  2
#endif
        .endm

FUNCTION(caml_start_program)
        CFI_STARTPROC

#ifdef __PIC__
        call_caml_enter         .Lcaml_start_program
        bl      caml_program(PLT)
#else
        ldr     r12, =caml_program

/* Address of OCaml code to call is in r12 */
.Ljump_to_caml:
        call_caml_enter         .Lcaml_start_program
        blx     r12
#endif
        call_caml_exit          .Lcaml_retaddr, .Lreturn_result
        call_caml_relocs        .Lcaml_start_program
        CFI_ENDPROC
        .type   .Lcaml_retaddr, %function
        .size   .Lcaml_retaddr, .-.Lcaml_retaddr
        .size   caml_start_program, .-caml_start_program

/* The trap handler */

        .align  2
.Ltrap_handler:
        CFI_STARTPROC
    /* Save exception pointer */
        str     trap_ptr, Caml_state_var(exception_pointer)
    /* Encode exception bucket as an exception result */
        orr     r0, r0, 2
    /* Return it */
        b       .Lreturn_result
        CFI_ENDPROC
        .type   .Ltrap_handler, %function
        .size   .Ltrap_handler, .-.Ltrap_handler

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        CFI_STARTPROC
    /* Test if backtrace is active */
        ldr     r1, Caml_state_var(backtrace_active)
        cbz     r1, 1f
    /* Preserve exception bucket in callee-save register r4 */
        mov     r4, r0
    /* Stash the backtrace */
        mov     r1, lr                          /* arg2: pc of raise */
        mov     r2, sp                          /* arg3: sp of raise */
        mov     r3, trap_ptr                    /* arg4: sp of handler */
        bl      caml_stash_backtrace
    /* Restore exception bucket */
        mov     r0, r4
1:  /* Cut stack at current trap handler */
        mov     sp, trap_ptr
    /* Pop previous handler and addr of trap, and jump to it */
        pop     {trap_ptr, pc}
        CFI_ENDPROC
        .size   caml_raise_exn, .-caml_raise_exn

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
    /* Load the domain state ptr */
        mov     domain_state_ptr, r0
    /* Load exception bucket */
        mov     r0, r1
    /* Reload trap ptr and alloc ptr */
        ldr     trap_ptr, Caml_state_var(exception_pointer)
        ldr     alloc_ptr, Caml_state_var(young_ptr)
    /* Test if backtrace is active */
        ldr     r1, Caml_state_var(backtrace_active)
        cbz     r1, 1f
    /* Preserve exception bucket in callee-save register r4 */
        mov     r4, r0
        ldr     r1, Caml_state_var(last_return_address) /* arg2: pc of raise */
        ldr     r2, Caml_state_var(bottom_of_stack)     /* arg3: sp of raise */
        mov     r3, trap_ptr                        /* arg4: sp of handler */
        bl      caml_stash_backtrace
    /* Restore exception bucket */
        mov     r0, r4
1:  /* Cut stack at current trap handler */
        mov     sp, trap_ptr
    /* Pop previous handler and addr of trap, and jump to it */
        pop     {trap_ptr, pc}
        CFI_ENDPROC
        .size   caml_raise_exception, .-caml_raise_exception

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (r0 = Caml_state, r1 = closure, [r2] = first arg) */
        ldr     r0, [r2]        /* r0 = first arg */
                                /* r1 = closure environment */
        ldr     r12, [r1]       /* code pointer */
#ifdef __PIC__
        call_caml_enter         .Lcaml_callback_asm
        blx     r12
#else
        b       .Ljump_to_caml
#endif
        call_caml_exit          .Lcca_retaddr, .Lcca_return_result
        call_caml_relocs        .Lcaml_callback_asm
        CFI_ENDPROC
        .size   caml_callback_asm, .-caml_callback_asm
#ifdef __PIC__
        .type   .Lcca_retaddr, %function
        .size   .Lcca_retaddr, .-.Lcca_retaddr
#endif

FUNCTION(caml_callback2_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (r0 = Caml_state, r1 = closure, [r2] = arg1, [r2,4] = arg2) */
        mov     r12, r1
        ldr     r0, [r2]          /* r0 = first arg */
        ldr     r1, [r2,4]        /* r1 = second arg */
        mov     r2, r12           /* r2 = closure environment */
#ifdef __PIC__
        call_caml_enter         .Lcaml_callback2_asm
        bl      caml_apply2(PLT)
#else
        ldr     r12, =caml_apply2
        b       .Ljump_to_caml
#endif
        call_caml_exit          .Lcca2_retaddr, .Lcca2_return_result
        call_caml_relocs        .Lcaml_callback2_asm
        CFI_ENDPROC
        .size   caml_callback2_asm, .-caml_callback2_asm
#ifdef __PIC__
        .type   .Lcca2_retaddr, %function
        .size   .Lcca2_retaddr, .-.Lcaml2_callback_asm_retaddr
#endif

FUNCTION(caml_callback3_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (r0 = Caml_state, r1 = closure, [r2] = arg1, [r2,4] = arg2,
        [r2,8] = arg3) */
        mov     r3, r1            /* r3 = closure environment */
        ldr     r0, [r2]          /* r0 = first arg */
        ldr     r1, [r2,4]        /* r1 = second arg */
        ldr     r2, [r2,8]        /* r2 = third arg */
#ifdef __PIC__
        call_caml_enter         .Lcaml_callback3_asm
        bl      caml_apply3(PLT)
#else
        ldr     r12, =caml_apply3
        b       .Ljump_to_caml
#endif
        call_caml_exit          .Lcca3_retaddr, .Lcca3_return_result
        call_caml_relocs        .Lcaml_callback3_asm
        CFI_ENDPROC
        .size   caml_callback3_asm, .-caml_callback3_asm
#ifdef __PIC__
        .type   .Lcca3_retaddr, %function
        .size   .Lcca3_retaddr, .-.Lcaml3_callback_asm_retaddr
#endif

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
#ifdef __PIC__
        inline_caml_c_call bl caml_array_bound_error(PLT)
#else
    /* Load address of [caml_array_bound_error] in r7 */
        ldr  r7, =caml_array_bound_error
    /* Call that function */
        b       caml_c_call
#endif
        CFI_ENDPROC
        .size   caml_ml_array_bound_error, .-caml_ml_array_bound_error

        TEXT_SECTION(caml_system__code_end)
        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

        .data
        .align  2
        .globl  caml_system__frametable
caml_system__frametable:
#ifdef __PIC__
        .word   4               /* num_descriptors including .Lcaml_retaddr */
        call_caml_framedescr    .Lcca_retaddr
        call_caml_framedescr    .Lcca2_retaddr
        call_caml_framedescr    .Lcca3_retaddr
#else
        .word   1               /* one descriptor (only .Lcaml_retaddr) */
#endif
        .word   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2
        .type   caml_system__frametable, %object
        .size   caml_system__frametable, .-caml_system__frametable

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
