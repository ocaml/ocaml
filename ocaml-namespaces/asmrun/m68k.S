|***********************************************************************
|*                                                                     *
|*                           Objective Caml                            *
|*                                                                     *
|*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
|*                                                                     *
|*  Copyright 1996 Institut National de Recherche en Informatique et   *
|*  en Automatique.  All rights reserved.  This file is distributed    *
|*  under the terms of the GNU Library General Public License, with    *
|*  the special exception on linking described in file ../LICENSE.     *
|*                                                                     *
|***********************************************************************

| $Id$

| Asm part of the runtime system, Motorola 68k processor

        .comm   _caml_requested_size, 4

| Allocation

        .text
        .globl  _caml_call_gc
        .globl  _caml_alloc1
        .globl  _caml_alloc2
        .globl  _caml_alloc3
        .globl  _caml_allocN

_caml_call_gc:
    | Save desired size
        movel   d5, _caml_requested_size
    | Record lowest stack address and return address
        movel   a7@, _caml_last_return_address
        movel   a7, d5
        addql   #4, d5
        movel   d5, _caml_bottom_of_stack
    | Record current allocation pointer (for debugging)
        movel   d6, _caml_young_ptr
    | Save all regs used by the code generator
        movel   d4, a7@-
        movel   d3, a7@-
        movel   d2, a7@-
        movel   d1, a7@-
        movel   d0, a7@-
        movel   a6, a7@-
        movel   a5, a7@-
        movel   a4, a7@-
        movel   a3, a7@-
        movel   a2, a7@-
        movel   a1, a7@-
        movel   a0, a7@-
        movel   a7, _caml_gc_regs
        fmovem  fp0-fp7, a7@-
    | Call the garbage collector
        jbsr    _caml_garbage_collection
    | Restore all regs used by the code generator
        fmovem  a7@+, fp0-fp7
        movel   a7@+, a0
        movel   a7@+, a1
        movel   a7@+, a2
        movel   a7@+, a3
        movel   a7@+, a4
        movel   a7@+, a5
        movel   a7@+, a6
        movel   a7@+, d0
        movel   a7@+, d1
        movel   a7@+, d2
        movel   a7@+, d3
        movel   a7@+, d4
    | Reload allocation pointer and allocate block
        movel   _caml_young_ptr, d6
        subl    _caml_requested_size, d6
    | Return to caller
        rts

_caml_alloc1:
        subql   #8, d6
        cmpl    _caml_young_limit, d6
        bcs     L100
        rts
L100:   moveq   #8, d5
        bra     _caml_call_gc

_caml_alloc2:
        subl    #12, d6
        cmpl    _caml_young_limit, d6
        bcs     L101
        rts
L101:   moveq   #12, d5
        bra     _caml_call_gc

_caml_alloc3:
        subl    #16, d6
        cmpl    _caml_young_limit, d6
        bcs     L102
        rts
L102:   moveq   #16, d5
        bra     _caml_call_gc

_caml_allocN:
        subl    d5, d6
        cmpl    _caml_young_limit, d6
        bcs     _caml_call_gc
        rts

| Call a C function from Caml

        .globl  _caml_c_call

_caml_c_call:
    | Record lowest stack address and return address
        movel   a7@+, _caml_last_return_address
        movel   a7, _caml_bottom_of_stack
    | Save allocation pointer and exception pointer
        movel   d6, _caml_young_ptr
        movel   d7, _caml_exception_pointer
    | Call the function (address in a0)
        jbsr    a0@
    | Reload allocation pointer
        movel   _caml_young_ptr, d6
    | Return to caller
        movel   _caml_last_return_address, a1
        jmp     a1@

| Start the Caml program

        .globl  _caml_start_program

_caml_start_program:
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Initial code point is caml_program
        lea     _caml_program, a5

| Code shared between caml_start_program and caml_callback*

L106:
    | Build a callback link
        movel   _caml_gc_regs, a7@-
        movel   _caml_last_return_address, a7@-
        movel   _caml_bottom_of_stack, a7@-
    | Build an exception handler
        pea     L108
        movel   _caml_exception_pointer, a7@-
        movel   a7, d7
    | Load allocation pointer
        movel   _caml_young_ptr, d6
    | Call the Caml code
        jbsr    a5@
L107:
    | Move result where C code expects it
        movel   a0, d0
    | Save allocation pointer
        movel   d6, _caml_young_ptr
    | Pop the exception handler
        movel   a7@+, _caml_exception_pointer
        addql   #4, a7
L109:
    | Pop the callback link, restoring the global variables
    | used by caml_c_call
        movel   a7@+, _caml_bottom_of_stack
        movel   a7@+, _caml_last_return_address
        movel   a7@+, _caml_gc_regs
    | Restore callee-save registers and return
        fmovem  a7@+, fp2-fp7
        moveml  a7@+, a2-a6/d2-d7
        unlk    a6
        rts
L108:
    | Exception handler
    | Save allocation pointer and exception pointer
        movel   d6, _caml_young_ptr
        movel   d7, _caml_exception_pointer
    | Encode exception bucket as an exception result
        movel   a0, d0
        orl     #2, d0
    | Return it
        bra     L109

| Raise an exception from C

        .globl  _caml_raise_exception
_caml_raise_exception:
        movel   a7@(4), a0     | exception bucket
        movel   _caml_young_ptr, d6
        movel   _caml_exception_pointer, a7
        movel   a7@+, d7
        rts

| Callback from C to Caml

        .globl  _caml_callback_exn
_caml_callback_exn:
        link    a6, #0
    | Save callee-save registers
        moveml  a2-a6/d2-d7, a7@-
        fmovem  fp2-fp7, a7@-
    | Initial loading of arguments
        movel   a6@(8), a1     | closure
        movel   a6@(12), a0    | argument
        movel   a1@(0), a5     | code pointer
        bra     L106

        .globl  _caml_callback2_exn
_caml_callback2_exn:
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

        .globl  _caml_callback3_exn
_caml_callback3_exn:
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

        .globl  _caml_ml_array_bound_error
_caml_ml_array_bound_error:
    | Load address of [caml_array_bound_error] in a0 and call it
        lea     _caml_array_bound_error, a0
        bra     _caml_c_call

        .data
        .globl  _caml_system__frametable
_caml_system__frametable:
        .long   1               | one descriptor
        .long   L107            | return address into callback
        .word   -1              | negative frame size => use callback link
        .word   0               | no roots here
