/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, Intel 386 processor */
/* Must be preprocessed by cpp */

#include "../config/m.h"

/* Linux/BSD with ELF binaries and Solaris do not prefix identifiers with _.
   Linux/BSD with a.out binaries and NextStep do. */

#if defined(SYS_solaris)
#define CONCAT(a,b) a/**/b
#else
#define CONCAT(a,b) a##b
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define LBL(x) CONCAT(.L,x)
#else
#define G(x) CONCAT(_,x)
#define LBL(x) CONCAT(L,x)
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_cygwin) \
 || defined(SYS_mingw) || defined(SYS_gnu)
#define FUNCTION_ALIGN 4
#else
#define FUNCTION_ALIGN 2
#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#endif

#if defined(PROFILING)
#if defined(SYS_linux_elf) || defined(SYS_gnu)
#define PROFILE_CAML \
        pushl %ebp; movl %esp, %ebp; pushl %eax; pushl %ecx; pushl %edx; \
        call mcount; \
        popl %edx; popl %ecx; popl %eax; popl %ebp
#define PROFILE_C \
        pushl %ebp; movl %esp, %ebp; call mcount; popl %ebp
#elif defined(SYS_bsd_elf)
#define PROFILE_CAML \
        pushl %ebp; movl %esp, %ebp; pushl %eax; pushl %ecx; pushl %edx; \
        call .mcount; \
        popl %edx; popl %ecx; popl %eax; popl %ebp
#define PROFILE_C \
        pushl %ebp; movl %esp, %ebp; call .mcount; popl %ebp
#elif defined(SYS_macosx)
#define PROFILE_CAML \
        pushl %ebp; movl %esp, %ebp; pushl %eax; pushl %ecx; pushl %edx; \
        call Lmcount$stub;  \
        popl %edx; popl %ecx; popl %eax; popl %ebp
#define PROFILE_C \
        pushl %ebp; movl %esp, %ebp; call Lmcount$stub; popl %ebp
#endif
#else
#define PROFILE_CAML
#define PROFILE_C
#endif

#ifdef SYS_macosx
#define ALIGN_STACK(amount) subl $ amount, %esp
#define UNDO_ALIGN_STACK(amount) addl $ amount, %esp
#else
#define ALIGN_STACK(amount)
#define UNDO_ALIGN_STACK(amount)
#endif

/* Allocation */

        .text
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

        .globl  G(caml_call_gc)
        .globl  G(caml_alloc1)
        .globl  G(caml_alloc2)
        .globl  G(caml_alloc3)
        .globl  G(caml_allocN)

        .align  FUNCTION_ALIGN
G(caml_call_gc):
        CFI_STARTPROC
        PROFILE_CAML
    /* Record lowest stack address and return address */
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
LBL(105):
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $16384, %esp
        movl    %eax, 0(%esp)
        addl    $16384, %esp
#endif
    /* Build array of registers, save it into caml_gc_regs */
        pushl   %ebp
        pushl   %edi
        pushl   %esi
        pushl   %edx
        pushl   %ecx
        pushl   %ebx
        pushl   %eax
        CFI_ADJUST(28)
        movl    %esp, G(caml_gc_regs)
        /* MacOSX note: 16-alignment of stack preserved at this point */
    /* Call the garbage collector */
        call    G(caml_garbage_collection)
    /* Restore all regs used by the code generator */
        popl    %eax
        popl    %ebx
        popl    %ecx
        popl    %edx
        popl    %esi
        popl    %edi
        popl    %ebp
        CFI_ADJUST(-28)
    /* Return to caller */
        ret
        CFI_ENDPROC

        .align  FUNCTION_ALIGN
G(caml_alloc1):
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $8, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(100)
        ret
LBL(100):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc1)

        .align  FUNCTION_ALIGN
G(caml_alloc2):
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $12, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(101)
        ret
LBL(101):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc2)

        .align  FUNCTION_ALIGN
G(caml_alloc3):
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $16, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(102)
        ret
LBL(102):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc3)

        .align  FUNCTION_ALIGN
G(caml_allocN):
        PROFILE_CAML
        subl    G(caml_young_ptr), %eax /* eax = size - caml_young_ptr */
        negl    %eax                    /* eax = caml_young_ptr - size */
        cmpl    G(caml_young_limit), %eax
        jb      LBL(103)
        movl    %eax, G(caml_young_ptr)
        ret
LBL(103):
        subl    G(caml_young_ptr), %eax /* eax = - size */
        negl    %eax                    /* eax = size */
        pushl   %eax                    /* save desired size */
        subl    %eax, G(caml_young_ptr) /* must update young_ptr */
        movl    4(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    8(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(8)
        call    LBL(105)
        UNDO_ALIGN_STACK(8)
        popl    %eax                    /* recover desired size */
        jmp     G(caml_allocN)

/* Call a C function from OCaml */

        .globl  G(caml_c_call)
        .align  FUNCTION_ALIGN
G(caml_c_call):
        PROFILE_CAML
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $16384, %esp
        movl    %eax, 0(%esp)
        addl    $16384, %esp
#endif
    /* Call the function (address in %eax) */
        jmp     *%eax

/* Start the OCaml program */

        .globl  G(caml_start_program)
        .align  FUNCTION_ALIGN
G(caml_start_program):
        CFI_STARTPROC
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
        CFI_ADJUST(16)
    /* Initial entry point is caml_program */
        movl    $ G(caml_program), %esi
    /* Common code for caml_start_program and caml_callback* */
LBL(106):
    /* Build a callback link */
        pushl   G(caml_gc_regs)
        pushl   G(caml_last_return_address)
        pushl   G(caml_bottom_of_stack)
        /* Note: 16-alignment preserved on MacOSX at this point */
    /* Build an exception handler */
        pushl   $ LBL(108)
        ALIGN_STACK(8)
        pushl   G(caml_exception_pointer)
        CFI_ADJUST(20)
        movl    %esp, G(caml_exception_pointer)
    /* Call the OCaml code */
        call    *%esi
LBL(107):
    /* Pop the exception handler */
        popl    G(caml_exception_pointer)
#ifdef SYS_macosx
        addl    $12, %esp
#else
        addl    $4, %esp
#endif
        CFI_ADJUST(-8)
LBL(109):
    /* Pop the callback link, restoring the global variables */
        popl    G(caml_bottom_of_stack)
        popl    G(caml_last_return_address)
        popl    G(caml_gc_regs)
    /* Restore callee-save registers. */
        popl    %ebp
        popl    %edi
        popl    %esi
        popl    %ebx
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orl     $2, %eax
        jmp     LBL(109)
        CFI_ENDPROC

/* Raise an exception from OCaml */

        .globl  G(caml_raise_exn)
        .align  FUNCTION_ALIGN
G(caml_raise_exn):
        testl   $1, G(caml_backtrace_active)
        jne     LBL(110)
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        UNDO_ALIGN_STACK(8)
        ret
LBL(110):
        movl    %eax, %esi          /* Save exception bucket in esi */
        movl    G(caml_exception_pointer), %edi /* SP of handler */
        movl    0(%esp), %eax       /* PC of raise */
        leal    4(%esp), %edx       /* SP of raise */
        ALIGN_STACK(12)
        pushl   %edi                        /* arg 4: sp of handler */
        pushl   %edx                        /* arg 3: sp of raise */
        pushl   %eax                        /* arg 2: pc of raise */
        pushl   %esi                        /* arg 1: exception bucket */
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    %edi, %esp
        popl    G(caml_exception_pointer)
        UNDO_ALIGN_STACK(8)
        ret

/* Raise an exception from C */

        .globl  G(caml_raise_exception)
        .align  FUNCTION_ALIGN
G(caml_raise_exception):
        PROFILE_C
        testl   $1, G(caml_backtrace_active)
        jne     LBL(111)
        movl    4(%esp), %eax
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        UNDO_ALIGN_STACK(8)
        ret
LBL(111):
        movl    4(%esp), %esi          /* Save exception bucket in esi */
        ALIGN_STACK(12)
        pushl   G(caml_exception_pointer)   /* arg 4: sp of handler */
        pushl   G(caml_bottom_of_stack)     /* arg 3: sp of raise */
        pushl   G(caml_last_return_address) /* arg 2: pc of raise */
        pushl   %esi                        /* arg 1: exception bucket */
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        UNDO_ALIGN_STACK(8)
        ret

/* Callback from C to OCaml */

        .globl  G(caml_callback_exn)
        .align  FUNCTION_ALIGN
G(caml_callback_exn):
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %ebx   /* closure */
        movl    24(%esp), %eax   /* argument */
        movl    0(%ebx), %esi    /* code pointer */
        jmp     LBL(106)

        .globl  G(caml_callback2_exn)
        .align  FUNCTION_ALIGN
G(caml_callback2_exn):
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %ecx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    $ G(caml_apply2), %esi   /* code pointer */
        jmp     LBL(106)

        .globl  G(caml_callback3_exn)
        .align  FUNCTION_ALIGN
G(caml_callback3_exn):
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %edx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    32(%esp), %ecx   /* third argument */
        movl    $ G(caml_apply3), %esi   /* code pointer */
        jmp     LBL(106)

        .globl  G(caml_ml_array_bound_error)
        .align  FUNCTION_ALIGN
G(caml_ml_array_bound_error):
    /* Empty the floating-point stack */
        ffree   %st(0)
        ffree   %st(1)
        ffree   %st(2)
        ffree   %st(3)
        ffree   %st(4)
        ffree   %st(5)
        ffree   %st(6)
        ffree   %st(7)
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
    /* For MacOS X: re-align the stack */
#ifdef SYS_macosx
        andl    $-16, %esp
#endif
    /* Branch to [caml_array_bound_error] (never returns) */
        call    G(caml_array_bound_error)

        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
G(caml_system__frametable):
        .long   1               /* one descriptor */
        .long   LBL(107)        /* return address into callback */
#ifndef SYS_solaris
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
#else
        .value  -1              /* negative frame size => use callback link */
        .value  0               /* no roots here */
#endif

        .globl  G(caml_extra_params)
G(caml_extra_params):
#ifndef SYS_solaris
        .space  64
#else
        .zero   64
#endif

#if defined(PROFILING) && defined(SYS_macosx)
        .section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
Lmcount$stub:
        .indirect_symbol mcount
        hlt ; hlt ; hlt ; hlt ; hlt
        .subsections_via_symbols
#endif

#if defined(SYS_linux_elf)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
