/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, Intel 386 processor */
/* Must be preprocessed by cpp */

#include "caml/m.h"

/* Linux/BSD with ELF binaries and Solaris do not prefix identifiers with _.
   Linux/BSD with a.out binaries and NextStep do. */

#if (defined(SYS_solaris) && !defined(__GNUC__))
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

#if defined(FUNCTION_SECTIONS)
#if defined(SYS_macosx) || defined(SYS_mingw) || defined(SYS_cygwin)
#define TEXT_SECTION(name)
#else
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#endif
#else
#define TEXT_SECTION(name)
#endif

#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl G(name); \
        .align FUNCTION_ALIGN; \
        G(name):

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) || defined(SYS_gnu)
#define ENDFUNCTION(name) \
        .type name,@function; \
        .size name, . - name
#else
#define ENDFUNCTION(name)
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

#if !defined(SYS_mingw) && !defined(SYS_cygwin)
#define STACK_PROBE_SIZE 16384
#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define CAML_STATE(var,reg) 8*domain_field_caml_##var(reg)

/* PR#6038: GCC and Clang seem to require 16-byte alignment nowadays,
   even if only MacOS X's ABI formally requires it. */
#define ALIGN_STACK(amount) subl $ amount, %esp ; CFI_ADJUST(amount)
#define UNDO_ALIGN_STACK(amount) addl $ amount, %esp ; CFI_ADJUST(-amount)

        .text
#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot.code_begin)
        .globl  G(caml_hot.code_begin)
G(caml_hot.code_begin):

        TEXT_SECTION(caml_hot.code_end)
        .globl  G(caml_hot.code_end)
G(caml_hot.code_end):
#endif

/* Allocation */
        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

FUNCTION(caml_call_gc)
        CFI_STARTPROC
LBL(caml_call_gc):
    /* Record lowest stack address and return address */
        movl    G(Caml_state), %ebx
        movl    (%esp), %eax
        movl    %eax, CAML_STATE(last_return_address, %ebx)
        leal    4(%esp), %eax
        movl    %eax, CAML_STATE(bottom_of_stack, %ebx)
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $(STACK_PROBE_SIZE), %esp; CFI_ADJUST(STACK_PROBE_SIZE);
        movl    %eax, 0(%esp)
        addl    $(STACK_PROBE_SIZE), %esp; CFI_ADJUST(-STACK_PROBE_SIZE);
#endif
    /* Build array of registers, save it into Caml_state->gc_regs */
        pushl   %ebp; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edx; CFI_ADJUST(4)
        pushl   %ecx; CFI_ADJUST(4)
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %eax; CFI_ADJUST(4)
        movl    %esp, CAML_STATE(gc_regs, %ebx)
        /* MacOSX note: 16-alignment of stack preserved at this point */
    /* Call the garbage collector */
        call    G(caml_garbage_collection)
    /* Restore all regs used by the code generator */
        popl    %eax; CFI_ADJUST(-4)
        popl    %ebx; CFI_ADJUST(-4)
        popl    %ecx; CFI_ADJUST(-4)
        popl    %edx; CFI_ADJUST(-4)
        popl    %esi; CFI_ADJUST(-4)
        popl    %edi; CFI_ADJUST(-4)
        popl    %ebp; CFI_ADJUST(-4)
    /* Return to caller. Returns young_ptr in %eax. */
        movl    CAML_STATE(young_ptr, %ebx), %eax
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_call_gc)

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        movl    CAML_STATE(young_ptr, %ebx), %eax
        subl    $8, %eax
        movl    %eax, CAML_STATE(young_ptr, %ebx)
        cmpl    CAML_STATE(young_limit, %ebx), %eax
        jb      LBL(caml_call_gc)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_alloc1)

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        movl    CAML_STATE(young_ptr, %ebx), %eax
        subl    $12, %eax
        movl    %eax, CAML_STATE(young_ptr, %ebx)
        cmpl    CAML_STATE(young_limit, %ebx), %eax
        jb      LBL(caml_call_gc)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_alloc2)

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        movl    CAML_STATE(young_ptr, %ebx), %eax
        subl    $16, %eax
        movl    %eax, CAML_STATE(young_ptr, %ebx)
        cmpl    CAML_STATE(young_limit, %ebx), %eax
        jb      LBL(caml_call_gc)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_alloc3)

FUNCTION(caml_allocN)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        /* eax = size - Caml_state->young_ptr */
        subl    CAML_STATE(young_ptr, %ebx), %eax
        negl    %eax              /* eax = Caml_state->young_ptr - size */
        movl    %eax, CAML_STATE(young_ptr, %ebx)
        cmpl    CAML_STATE(young_limit, %ebx), %eax
        jb      LBL(caml_call_gc)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_allocN)

/* Call a C function from OCaml */

FUNCTION(caml_c_call)
        CFI_STARTPROC
    /* Record lowest stack address and return address */
    /* ecx and edx are destroyed at C call. Use them as temp. */
        movl    G(Caml_state), %ecx
        movl    (%esp), %edx
        movl    %edx, CAML_STATE(last_return_address, %ecx)
        leal    4(%esp), %edx
        movl    %edx, CAML_STATE(bottom_of_stack, %ecx)
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $(STACK_PROBE_SIZE), %esp; CFI_ADJUST(STACK_PROBE_SIZE);
        movl    %eax, 0(%esp)
        addl    $(STACK_PROBE_SIZE), %esp; CFI_ADJUST(-STACK_PROBE_SIZE);
#endif
    /* Call the function (address in %eax) */
        jmp     *%eax
        CFI_ENDPROC
        ENDFUNCTION(caml_c_call)

/* Start the OCaml program */

FUNCTION(caml_start_program)
        CFI_STARTPROC
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial entry point is caml_program */
        movl    $ G(caml_program), %esi
    /* Common code for caml_start_program and caml_callback* */
LBL(106):
        movl    G(Caml_state), %edi
    /* Build a callback link */
        pushl   CAML_STATE(gc_regs, %edi); CFI_ADJUST(4)
        pushl   CAML_STATE(last_return_address, %edi); CFI_ADJUST(4)
        pushl   CAML_STATE(bottom_of_stack, %edi); CFI_ADJUST(4)
        /* Note: 16-alignment preserved on MacOSX at this point */
    /* Build an exception handler */
        pushl   $ LBL(108); CFI_ADJUST(4)
        ALIGN_STACK(8)
        pushl   CAML_STATE(exception_pointer, %edi); CFI_ADJUST(4)
        movl    %esp, CAML_STATE(exception_pointer, %edi)
    /* Call the OCaml code */
        call    *%esi
LBL(107):
        movl    G(Caml_state), %edi
    /* Pop the exception handler */
        popl    CAML_STATE(exception_pointer, %edi); CFI_ADJUST(-4)
        addl    $12, %esp       ; CFI_ADJUST(-12)
LBL(109):
        movl    G(Caml_state), %edi /* Reload for LBL(109) entry */
    /* Pop the callback link, restoring the global variables */
        popl    CAML_STATE(bottom_of_stack, %edi); CFI_ADJUST(-4)
        popl    CAML_STATE(last_return_address, %edi); CFI_ADJUST(-4)
        popl    CAML_STATE(gc_regs, %edi); CFI_ADJUST(-4)
    /* Restore callee-save registers. */
        popl    %ebp; CFI_ADJUST(-4)
        popl    %edi; CFI_ADJUST(-4)
        popl    %esi; CFI_ADJUST(-4)
        popl    %ebx; CFI_ADJUST(-4)
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orl     $2, %eax
        jmp     LBL(109)
        CFI_ENDPROC
        ENDFUNCTION(caml_start_program)

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        testl   $1, CAML_STATE(backtrace_active, %ebx)
        jne     LBL(110)
        movl    CAML_STATE(exception_pointer, %ebx), %esp
        popl    CAML_STATE(exception_pointer, %ebx); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
LBL(110):
        movl    %eax, %esi          /* Save exception bucket in esi */
        movl    CAML_STATE(exception_pointer, %ebx), %edi /* SP of handler */
        movl    0(%esp), %eax       /* PC of raise */
        leal    4(%esp), %edx       /* SP of raise */
        ALIGN_STACK(12)
        pushl   %edi; CFI_ADJUST(4)         /* arg 4: sp of handler */
        pushl   %edx; CFI_ADJUST(4)         /* arg 3: sp of raise */
        pushl   %eax; CFI_ADJUST(4)         /* arg 2: pc of raise */
        pushl   %esi; CFI_ADJUST(4)         /* arg 1: exception bucket */
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    %edi, %esp
        popl    CAML_STATE(exception_pointer, %ebx); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_raise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
        movl    G(Caml_state), %ebx
        testl   $1, CAML_STATE(backtrace_active, %ebx)
        jne     LBL(112)
        movl    8(%esp), %eax
        movl    CAML_STATE(exception_pointer, %ebx), %esp
        popl    CAML_STATE(exception_pointer, %ebx); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
LBL(112):
        movl    8(%esp), %esi          /* Save exception bucket in esi */
        ALIGN_STACK(12)
        /* 4: sp of handler */
        pushl   CAML_STATE(exception_pointer, %ebx); CFI_ADJUST(4)
        /* 3: sp of raise */
        pushl   CAML_STATE(bottom_of_stack, %ebx); CFI_ADJUST(4)
        /* 2: pc of raise */
        pushl   CAML_STATE(last_return_address, %ebx); CFI_ADJUST(4)
        /* 1: exception bucket */
        pushl   %esi; CFI_ADJUST(4)
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    CAML_STATE(exception_pointer, %ebx), %esp
        popl    CAML_STATE(exception_pointer, %ebx); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
        CFI_ENDPROC
        ENDFUNCTION(caml_raise_exception)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        CFI_STARTPROC
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    24(%esp), %ebx   /* arg2: closure */
        movl    28(%esp), %edi   /* arguments array */
        movl    0(%edi), %eax    /* arg1: argument */
        movl    0(%ebx), %esi    /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC
ENDFUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
        CFI_STARTPROC
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    24(%esp), %ecx   /* arg3: closure */
        movl    28(%esp), %edi   /* arguments array */
        movl    0(%edi), %eax    /* arg1: first argument */
        movl    4(%edi), %ebx    /* arg2: second argument */
        movl    $ G(caml_apply2), %esi   /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC
ENDFUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
        CFI_STARTPROC
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    24(%esp), %edx   /* arg4: closure */
        movl    28(%esp), %edi   /* arguments array */
        movl    0(%edi), %eax    /* arg1: first argument */
        movl    4(%edi), %ebx    /* arg2: second argument */
        movl    8(%edi), %ecx    /* arg3: third argument */
        movl    $ G(caml_apply3), %esi   /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC
ENDFUNCTION(caml_callback3_asm)

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
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
        movl    G(Caml_state), %ebx
        movl    (%esp), %edx
        movl    %edx, CAML_STATE(last_return_address, %ebx)
        leal    4(%esp), %edx
        movl    %edx, CAML_STATE(bottom_of_stack, %ebx)
    /* Re-align the stack */
        andl    $-16, %esp
    /* Branch to [caml_array_bound_error] (never returns) */
        call    G(caml_array_bound_error)
        CFI_ENDPROC
        ENDFUNCTION(caml_ml_array_bound_error)

        TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system.frametable)
G(caml_system.frametable):
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
        .space  256
#else
        .zero   256
#endif

#if defined(SYS_linux_elf)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
