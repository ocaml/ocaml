|***********************************************************************
|*                                                                     *
|*                           Objective Caml                            *
|*                                                                     *
|*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
|*                                                                     *
|*  Copyright 1996 Institut National de Recherche en Informatique et   *
|*  Automatique.  Distributed only by permission.                      *
|*                                                                     *
|***********************************************************************

| $Id$

| Asm part of the runtime system, Motorola 68k processor

        .comm   _young_limit, 4
        .comm   _young_ptr, 4
        .comm   _gc_entry_regs, 48
        .comm   _caml_bottom_of_stack, 4
        .comm   _caml_top_of_stack, 4
        .comm   _caml_last_return_address, 4
        .comm   _caml_exception_pointer, 4
        .comm   _caml_requested_size, 4

| Allocation

        .text
	.globl  _caml_call_gc
        .globl  _caml_alloc1
        .globl  _caml_alloc2
        .globl  _caml_alloc3
        .globl  _caml_alloc

_caml_call_gc:
    | Save desired size
        movel   d5, _caml_requested_size
    | Record lowest stack address and return address
        movel   a7@, _caml_last_return_address
        movel   a7, d5
        addql   #4, d5
        movel   d5, _caml_bottom_of_stack
    | Record current allocation pointer (for debugging)
        movel   d6, _young_ptr
    | Save all regs used by the code generator
        movel   a0, _gc_entry_regs
        movel   a1, _gc_entry_regs + 4
        movel   a2, _gc_entry_regs + 8
        movel   a3, _gc_entry_regs + 12
        movel   a4, _gc_entry_regs + 16
        movel   a5, _gc_entry_regs + 20
        movel   a6, _gc_entry_regs + 24
        movel   d0, _gc_entry_regs + 28
        movel   d1, _gc_entry_regs + 32
        movel   d2, _gc_entry_regs + 36
        movel   d3, _gc_entry_regs + 40
        movel   d4, _gc_entry_regs + 44
        fmovem  fp0-fp7, a7@-
    | Call the garbage collector
        jbsr    _garbage_collection
    | Restore all regs used by the code generator
        fmovem  a7@+, fp0-fp7
        movel   _gc_entry_regs, a0
        movel   _gc_entry_regs + 4, a1
        movel   _gc_entry_regs + 8, a2
        movel   _gc_entry_regs + 12, a3
        movel   _gc_entry_regs + 16, a4
        movel   _gc_entry_regs + 20, a5
        movel   _gc_entry_regs + 24, a6
        movel   _gc_entry_regs + 28, d0
        movel   _gc_entry_regs + 32, d1
        movel   _gc_entry_regs + 36, d2
        movel   _gc_entry_regs + 40, d3
        movel   _gc_entry_regs + 44, d4
    | Reload allocation pointer and allocate block
        movel   _young_ptr, d6
        subl    _caml_requested_size, d6
    | Return to caller
        rts

_caml_alloc1:
        subql   #8, d6
        cmpl    _young_limit, d6
        bcs     L100
        rts
L100:   moveq   #8, d5
        bra     _caml_call_gc

_caml_alloc2:
        subl    #12, d6
        cmpl    _young_limit, d6
        bcs     L101
        rts
L101:   moveq   #12, d5
        bra     _caml_call_gc

_caml_alloc3:
        subl    #16, d6
        cmpl    _young_limit, d6
        bcs     L102
        rts
L102:   moveq   #16, d5
        bra     _caml_call_gc

_caml_alloc:
        subl    d5, d6
        cmpl    _young_limit, d6
        bcs     _caml_call_gc
        rts

| Call a C function from Caml

        .globl  _caml_c_call

_caml_c_call:
    | Record lowest stack address and return address
        movel   a7@+, _caml_last_return_address
        movel   a7, _caml_bottom_of_stack
    | Save allocation pointer and exception pointer
        movel   d6, _young_ptr
        movel   d7, _caml_exception_pointer
    | Call the function (address in a0)
        jbsr    a0@
    | Reload allocation pointer
        movel   _young_ptr, d6
    | Return to caller
        movel   _caml_last_return_address, a1
        jmp     a1@

| Start the Caml program

        .globl  _caml_start_program

_caml_start_program:
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Build an exception handler
        pea     L103
        pea     0
        movel   a7, d7
    | Record highest stack address
        movel   a7, _caml_top_of_stack
    | Load allocation pointer
        movel   _young_ptr, d6
    | Go for it
        jbsr    _caml_program
    | Pop handler
        addql   #8, a7
    | Zero return code
        clrl    d0
        bra     L104
L103:
    | Return exception bucket
        movel   a0, d0
L104:
    | Restore registers and return
        fmovem  a7@+, fp2-fp7
        moveml  a7@+, a2-a6/d2-d7
        rts

| Raise an exception from C

        .globl  _raise_caml_exception
_raise_caml_exception:
        movel   a7@(4), a0     | exception bucket
        movel   _young_ptr, d6
        movel   _caml_exception_pointer, a7
        movel   a7@+, d7
        rts

| Callback from C to Caml

        .globl  _callback
_callback:
        link    a6, #0
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Initial loading of arguments
        movel   a6@(8), a1     | closure
        movel   a6@(12), a0    | argument
        movel   a1@(0), a5     | code pointer
L106:
    | Build a callback link
        movel   _caml_last_return_address, a7@-
        movel   _caml_bottom_of_stack, a7@-
    | Build an exception handler
        pea     L108
        movel   _caml_exception_pointer, a7@-
        movel   a7, d7
    | Load allocation pointer
        movel   _young_ptr, d6
    | Call the Caml code
        jbsr    a5@
L107:
    | Move result where C code expects it
        movel   a0, d0
    | Save allocation pointer
        movel   d6, _young_ptr
    | Pop the exception handler
        movel   a7@+, _caml_exception_pointer
        addql   #4, a7
    | Pop the callback link, restoring the global variables
    | used by caml_c_call
        movel   a7@+, _caml_bottom_of_stack
        movel   a7@+, _caml_last_return_address
    | Restore callee-save registers and return
        fmovem  a7@+, fp2-fp7
        moveml  a7@+, a2-a6/d2-d7
        unlk    a6
        rts
L108:
    | Exception handler
    | Save allocation pointer and exception pointer
        movel   d6, _young_ptr
        movel   d7, _caml_exception_pointer
    | Pop the callback link, restoring the global variables
    | used by caml_c_call
        movel   a7@+, _caml_bottom_of_stack
        movel   a7@+, _caml_last_return_address
    | Re-raise the exception through mlraise,
    | so that local C roots are cleaned up correctly.
        movel   a0, a7@-       | exn bucket is the argument
        jbsr    _mlraise        | never returns

        .globl  _callback2
_callback2:
        link    a6, #0
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Initial loading of arguments
        movel   a6@(8), a2     | closure
        movel   a6@(12), a0    | first argument
        movel   a6@(16), a1    | second argument
        lea     _caml_apply2, a5 | code pointer
        bra     L106

        .globl  _callback3
_callback3:
        link    a6, #0
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Initial loading of arguments
        movel   a6@(8), a3     | closure
        movel   a6@(12), a0    | first argument
        movel   a6@(16), a1    | second argument
        movel   a6@(20), a2    | third argument
        lea     _caml_apply3, a5 | code pointer
        bra     L106

        .data
        .globl  _system_frametable
_system_frametable:
        .long   1               | one descriptor
        .long   L107            | return address into callback
        .word   -1              | negative frame size => use callback link
        .word   0               | no roots here
