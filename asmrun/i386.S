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
#else
#define G(x) _##x
#endif

        .comm   G(young_start), 4
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

        .align  4
G(caml_alloc1):
        movl    G(young_ptr), %eax
        subl    $8, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_start), %eax
        jb      L100
        ret
L100:   movl    $8, %eax
        jmp     L105

        .align  4
G(caml_alloc2):
        movl    G(young_ptr), %eax
        subl    $12, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_start), %eax
        jb      L101
        ret
L101:   movl    $12, %eax
        jmp     L105

        .align  4
G(caml_alloc3):
        movl    G(young_ptr), %eax
        subl    $16, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_start), %eax
        jb      L102
        ret
L102:   movl    $16, %eax
        jmp     L105

        .align  4
G(caml_alloc):
        pushl   %eax
        movl    G(young_ptr), %eax
        subl    (%esp), %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_start), %eax
        jb      L103
        addl    $4, %esp
        ret
L103:   popl    %eax
        jmp     L105

G(caml_call_gc):
    /* Recover desired size and adjust return address */
        popl    %eax
        addl    $2, %eax
        pushl   %eax
	movzwl	-2(%eax), %eax
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
    /* Decrement young_ptr by desired size */
        popl    %eax
        subl    %eax, G(young_ptr)
    /* Reload result of allocation in %eax */
        movl    G(young_ptr), %eax
    /* Return to caller */
        pushl   G(caml_last_return_address)
        ret

/* Call a C function from Caml */

        .globl  G(caml_c_call)

        .align  4
G(caml_c_call):
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
    /* Free the floating-point register stack */
        finit
    /* Call the function (address in %eax) */
        jmp     *%eax

/* Start the Caml program */

        .globl  G(caml_start_program)
        .align  4
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
        .align  4
G(raise_caml_exception):
        movl    4(%esp), %eax
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        ret
