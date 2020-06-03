;**************************************************************************
;*                                                                        *
;*                                 OCaml                                  *
;*                                                                        *
;*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *
;*                                                                        *
;*   Copyright 1996 Institut National de Recherche en Informatique et     *
;*     en Automatique.                                                    *
;*                                                                        *
;*   All rights reserved.  This file is distributed under the terms of    *
;*   the GNU Lesser General Public License version 2.1, with the          *
;*   special exception on linking described in the file LICENSE.          *
;*                                                                        *
;**************************************************************************

; Asm part of the runtime system, Intel 386 processor, Intel syntax

        .386
        .MODEL FLAT

        EXTERN  _caml_garbage_collection: PROC
        EXTERN  _caml_apply2: PROC
        EXTERN  _caml_apply3: PROC
        EXTERN  _caml_program: PROC
        EXTERN  _caml_array_bound_error: PROC
        EXTERN  _caml_stash_backtrace: PROC
        EXTERN  _Caml_state: DWORD

; Allocation

        .CODE
        PUBLIC  _caml_call_gc
        PUBLIC  _caml_alloc1
        PUBLIC  _caml_alloc2
        PUBLIC  _caml_alloc3
        PUBLIC  _caml_allocN

INCLUDE domain_state32.inc

_caml_call_gc:
    ; Record lowest stack address and return address
        mov     ebx, _Caml_state
        mov     eax, [esp]
        Store_last_return_address ebx, eax
        lea     eax, [esp+4]
        Store_bottom_of_stack ebx, eax
    ; Save all regs used by the code generator
        push    ebp
        push    edi
        push    esi
        push    edx
        push    ecx
        push    ebx
        push    eax
        Store_gc_regs ebx, esp
    ; Call the garbage collector
        call    _caml_garbage_collection
    ; Restore all regs used by the code generator
        pop     eax
        pop     ebx
        pop     ecx
        pop     edx
        pop     esi
        pop     edi
        pop     ebp
    ; Return to caller. Returns young_ptr in eax
        Load_young_ptr ebx, eax
        ret

        ALIGN  4
_caml_alloc1:
        mov     ebx, _Caml_state
        Load_young_ptr ebx, eax
        sub     eax, 8
        Store_young_ptr ebx, eax
        Cmp_young_limit ebx, eax
        jb      _caml_call_gc
        ret

        ALIGN  4
_caml_alloc2:
        mov     ebx, _Caml_state
        Load_young_ptr ebx, eax
        sub     eax, 12
        Store_young_ptr ebx, eax
        Cmp_young_limit ebx, eax
        jb      _caml_call_gc
        ret

        ALIGN  4
_caml_alloc3:
        mov     ebx, _Caml_state
        Load_young_ptr ebx, eax
        sub     eax, 16
        Store_young_ptr ebx, eax
        Cmp_young_limit ebx, eax
        jb      _caml_call_gc
        ret

        ALIGN  4
_caml_allocN:
        mov     ebx, _Caml_state
        Sub_young_ptr ebx, eax ; eax = size - young_ptr
        neg     eax            ; eax = young_ptr - size
        Store_young_ptr ebx, eax
        Cmp_young_limit ebx, eax
        jb      _caml_call_gc
        ret

; Call a C function from OCaml

        PUBLIC  _caml_c_call
        ALIGN  4
_caml_c_call:
    ; Record lowest stack address and return address
    ; ecx and edx are destroyed at C call. Use them as temp.
        mov     ecx, _Caml_state
        mov     edx, [esp]
        Store_last_return_address ecx, edx
        lea     edx, [esp+4]
        Store_bottom_of_stack ecx, edx
    ; Call the function (address in %eax)
        jmp     eax

; Start the OCaml program

        PUBLIC  _caml_start_program
        ALIGN  4
_caml_start_program:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial code pointer is caml_program
        mov     esi, offset _caml_program

; Code shared between caml_start_program and callback*

L106:
        mov     edi, _Caml_state
    ; Build a callback link
        Push_gc_regs edi
        Push_last_return_address edi
        Push_bottom_of_stack edi
    ; Build an exception handler
        push    L108
        Push_exception_pointer edi
        Store_exception_pointer edi, esp
    ; Call the OCaml code
        call    esi
L107:
        mov     edi, _Caml_state
    ; Pop the exception handler
        Pop_exception_pointer edi
        add     esp, 4
L109:
        mov     edi, _Caml_state
    ; Pop the callback link, restoring the global variables
    ; used by caml_c_call
        Pop_bottom_of_stack edi
        Pop_last_return_address edi
        Pop_gc_regs edi
    ; Restore callee-save registers.
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
    ; Return to caller.
        ret
L108:
    ; Exception handler
    ; Mark the bucket as an exception result and return it
        or      eax, 2
        jmp     L109

; Raise an exception for OCaml

        PUBLIC  _caml_raise_exn
        ALIGN   4
_caml_raise_exn:
        mov     ebx, _Caml_state
        Load_backtrace_active ebx, ecx
        test    ecx, 1
        jne     L110
        Load_exception_pointer ebx, esp
        Pop_exception_pointer ebx
        ret
L110:
        mov     esi, eax                ; Save exception bucket in esi
        Load_exception_pointer ebx, edi ; SP of handler
        mov     eax, [esp]              ; PC of raise
        lea     edx, [esp+4]            ; SP of raise
        push    edi                     ; arg 4: SP of handler
        push    edx                     ; arg 3: SP of raise
        push    eax                     ; arg 2: PC of raise
        push    esi                     ; arg 1: exception bucket
        call    _caml_stash_backtrace
        mov     eax, esi                ; recover exception bucket
        mov     esp, edi                ; cut the stack
        Pop_exception_pointer ebx
        ret

; Raise an exception from C

        PUBLIC  _caml_raise_exception
        ALIGN  4
_caml_raise_exception:
        mov     ebx, _Caml_state
        Load_backtrace_active ebx, ecx
        test    ecx, 1
        jne     L112
        mov     eax, [esp+8]
        Load_exception_pointer ebx, esp
        Pop_exception_pointer ebx
        ret
L112:
        mov     esi, [esp+8]            ; Save exception bucket in esi
        Push_exception_pointer ebx      ; arg 4: SP of handler
        Push_bottom_of_stack ebx        ; arg 3: SP of raise
        Push_last_return_address ebx    ; arg 2: PC of raise
        push    esi                     ; arg 1: exception bucket
        call    _caml_stash_backtrace
        mov     eax, esi                ; recover exception bucket
        Load_exception_pointer ebx, esp ; cut the stack
        Pop_exception_pointer ebx
        ret

; Callback from C to OCaml

        PUBLIC  _caml_callback_asm
        ALIGN  4
_caml_callback_asm:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     ebx, [esp+24]   ; arg2: closure
        mov     edi, [esp+28]   ; arguments array
        mov     eax, [edi]      ; arg1: argument
        mov     esi, [ebx]      ; code pointer
        jmp     L106

        PUBLIC  _caml_callback2_asm
        ALIGN  4
_caml_callback2_asm:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     ecx, [esp+24]   ; arg3: closure
        mov     edi, [esp+28]   ; arguments array
        mov     eax, [edi]      ; arg1: first argument
        mov     ebx, [edi+4]    ; arg2: second argument
        mov     esi, offset _caml_apply2   ; code pointer
        jmp     L106

        PUBLIC  _caml_callback3_asm
        ALIGN   4
_caml_callback3_asm:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     edx, [esp+24]   ; arg4: closure
        mov     edi, [esp+28]   ; arguments array
        mov     eax, [edi]      ; arg1: first argument
        mov     ebx, [edi+4]    ; arg2: second argument
        mov     ecx, [edi+8]    ; arg3: third argument
        mov     esi, offset _caml_apply3   ; code pointer
        jmp     L106

        PUBLIC  _caml_ml_array_bound_error
        ALIGN   4
_caml_ml_array_bound_error:
    ; Empty the floating-point stack
        ffree   st(0)
        ffree   st(1)
        ffree   st(2)
        ffree   st(3)
        ffree   st(4)
        ffree   st(5)
        ffree   st(6)
        ffree   st(7)
    ; Branch to caml_array_bound_error
        mov     eax, offset _caml_array_bound_error
        jmp     _caml_c_call

        .DATA
        PUBLIC  _caml_system__frametable
_caml_system__frametable LABEL DWORD
        DWORD   1               ; one descriptor
        DWORD   L107            ; return address into callback
        WORD    -1              ; negative frame size => use callback link
        WORD    0               ; no roots here

        PUBLIC  _caml_extra_params
_caml_extra_params LABEL DWORD
        BYTE    64 DUP (?)

        END
