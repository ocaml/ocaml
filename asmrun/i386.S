/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, Intel 386 processor */
/* Must be preprocessed by cpp */

/* Linux with ELF binaries does not prefix identifiers with _.
   Linux with a.out binaries, FreeBSD, and NextStep do. */

#ifdef SYS_linux_elf
#define G(x) x
#define FUNCTION_ALIGN 16
#else
#define G(x) _##x
#define FUNCTION_ALIGN 4
#endif

        .comm   G(young_limit), 4
        .comm   G(young_ptr), 4
        .comm   G(gc_entry_regs), 4 * 7
        .comm   G(caml_bottom_of_stack), 4
        .comm   G(caml_top_of_stack), 4
        .comm   G(caml_last_return_address), 4
        .comm   G(remembered_ptr), 4
        .comm   G(remembered_end), 4
        .comm   G(caml_exception_pointer), 4

/* Allocation */

        .text
        .globl  G(caml_alloc1)
        .globl  G(caml_alloc2)
        .globl  G(caml_alloc3)
        .globl  G(caml_alloc)
	.globl  G(caml_call_gc)

        .align  FUNCTION_ALIGN
G(caml_alloc1):
        movl    G(young_ptr), %eax
        subl    $8, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L100
        ret
L100:   movl    $8, %eax
        jmp     L105

        .align  FUNCTION_ALIGN
G(caml_alloc2):
        movl    G(young_ptr), %eax
        subl    $12, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L101
        ret
L101:   movl    $12, %eax
        jmp     L105

        .align  FUNCTION_ALIGN
G(caml_alloc3):
        movl    G(young_ptr), %eax
        subl    $16, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L102
        ret
L102:   movl    $16, %eax
        jmp     L105

        .align  FUNCTION_ALIGN
G(caml_alloc):
        pushl   %eax
        movl    G(young_ptr), %eax
        subl    (%esp), %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L103
        addl    $4, %esp
        ret
L103:   popl    %eax
        jmp     L105

G(caml_call_gc):
    /* Adjust return address and recover desired size in %eax */
        popl    %eax
        addl    $2, %eax
        pushl   %eax
        movzwl  -2(%eax), %eax
L105:
    /* Record lowest stack address and return address */
        popl    G(caml_last_return_address)
        movl    %esp, G(caml_bottom_of_stack)
    /* Save all regs used by the code generator */
        movl    %ebx, G(gc_entry_regs) + 4
        movl    %ecx, G(gc_entry_regs) + 8
        movl    %edx, G(gc_entry_regs) + 12
        movl    %esi, G(gc_entry_regs) + 16
        movl    %edi, G(gc_entry_regs) + 20
        movl    %ebp, G(gc_entry_regs) + 24
    /* Save desired size */
        pushl   %eax
    /* Call the garbage collector */
        call    G(minor_collection)
    /* Restore all regs used by the code generator */
        movl    G(gc_entry_regs) + 4, %ebx
        movl    G(gc_entry_regs) + 8, %ecx
        movl    G(gc_entry_regs) + 12, %edx
        movl    G(gc_entry_regs) + 16, %esi
        movl    G(gc_entry_regs) + 20, %edi
        movl    G(gc_entry_regs) + 24, %ebp
    /* Recover desired size */
        popl    %eax
    /* Decrement young_ptr by desired size */
        subl    %eax, G(young_ptr)
    /* Reload result of allocation in %eax */
        movl    G(young_ptr), %eax
    /* Return to caller */
        pushl   G(caml_last_return_address)
        ret

/* Call a C function from Caml */

        .globl  G(caml_c_call)

        .align  FUNCTION_ALIGN
G(caml_c_call):
    /* Record lowest stack address and return address */
    /* In parallel, free the floating point registers */
    /* (Pairing is expected on the Pentium.) */
        movl    (%esp), %edx
        ffree   %st(0)
        movl    %edx, G(caml_last_return_address)
        ffree   %st(1)
        leal    4(%esp), %edx
        ffree   %st(2)
        movl    %edx, G(caml_bottom_of_stack)
        ffree   %st(3)
    /* Call the function (address in %eax) */
        jmp     *%eax

/* Start the Caml program */

        .globl  G(caml_start_program)
        .align  FUNCTION_ALIGN
G(caml_start_program):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Build an exception handler */
        pushl   $L104
        pushl   $0
        movl    %esp, G(caml_exception_pointer)
    /* Record highest stack address */
        movl    %esp, G(caml_top_of_stack)
    /* Go for it */
        call    G(caml_program)
    /* Pop handler */
        addl    $8, %esp
    /* Zero return code */
        xorl    %eax, %eax
L104:
    /* Restore registers and return */
        popl    %ebp
        popl    %edi
        popl    %esi
        popl    %ebx
        ret

/* Raise an exception from C */

        .globl  G(raise_caml_exception)
        .align  FUNCTION_ALIGN
G(raise_caml_exception):
        movl    4(%esp), %eax
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        ret

/* Callback from C to Caml */

        .globl  G(callback)
        .align  FUNCTION_ALIGN
G(callback):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %ebx   /* closure */
        movl    24(%esp), %eax   /* argument */
        movl    0(%ebx), %esi    /* code pointer */
L106:
    /* Build an exception handler */
        pushl   $L108
        pushl   G(caml_exception_pointer)
        movl    %esp, G(caml_exception_pointer)
    /* Build a callback link */
        pushl   G(caml_last_return_address)
        pushl   G(caml_bottom_of_stack)
    /* Call the Caml code */
        call    *%esi
L107:
    /* Pop the callback link, restoring the global variables
       used by caml_c_call */
        popl    G(caml_bottom_of_stack)
        popl    G(caml_last_return_address)
    /* Pop the exception handler */
        popl    G(caml_exception_pointer)
        popl    %esi    /* dummy register */
    /* Restore callee-save registers.
       In parallel, free the floating-point registers
       that may have been used by Caml. */
        popl    %ebp
        ffree   %st(0)
        popl    %edi
        ffree   %st(1)
        popl    %esi
        ffree   %st(2)
        popl    %ebx
        ffree   %st(3)
    /* Return to caller. */
        ret
L108:
    /* Exception handler: re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
        pushl   %eax            /* exn bucket is the argument */
        call    G(mlraise)      /* never returns */

        .globl  G(callback2)
        .align  FUNCTION_ALIGN
G(callback2):
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
        jmp     L106

        .globl  G(callback3)
        .align  FUNCTION_ALIGN
G(callback3):
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
        jmp     L106

        .data
        .globl  G(system_frametable)
G(system_frametable):
        .long   1               /* one descriptor */
        .long   L107            /* return address into callback */
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
