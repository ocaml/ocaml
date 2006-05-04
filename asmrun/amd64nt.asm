/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, AMD64 processor, Intel syntax */

/* Notes on Win64 calling conventions:
     function arguments in RCX, RDX, R8, R9 / XMM0 - XMM3
     caller must reserve 32 bytes of stack space
     callee must preserve RBX, RBP, RSI, RDI, R12-R15, XMM6-XMM15
*/

        EXTERN  _caml_garbage_collection: PROC
        EXTERN  _caml_apply2: PROC
        EXTERN  _caml_apply3: PROC
        EXTERN  _caml_program: PROC
        EXTERN  _caml_array_bound_error: PROC
        EXTERN  _caml_young_limit: QWORD
        EXTERN  _caml_young_ptr: QWORD
        EXTERN	_caml_bottom_of_stack: QWORD
        EXTERN	_caml_last_return_address: QWORD
        EXTERN	_caml_gc_regs: QWORD
	EXTERN	_caml_exception_pointer: QWORD

        .CODE

; Allocation

        PUBLIC  _caml_call_gc
        ALIGN   16
_caml_call_gc:
    ; Record lowest stack address and return address
        mov     rax, [rsp]
        mov     _caml_last_return_address, rax
        lea     rax, [rsp+8]
        mov     _caml_bottom_of_stack, rax
L105:  
    ; Save caml_young_ptr, caml_exception_pointer
	mov	_caml_young_ptr, r15
	mov	_caml_exception_pointer, r14
    ; Build array of registers, save it into caml_gc_regs 
        push    r13
        push    r12
        push    rbp
        push    r11
        push    r10
        push    r9
        push    r8
        push    rcx
        push    rdx
        push    rsi
        push    rdi
        push    rbx
        push    rax
        mov     _caml_gc_regs, rsp
    ; Save floating-point registers 
        sub     rsp, 16*8
        movlpd  [rsp + 0*8], xmm0
        movlpd  [rsp + 1*8], xmm1
        movlpd  [rsp + 2*8], xmm2
        movlpd  [rsp + 3*8], xmm3
        movlpd  [rsp + 4*8], xmm4
        movlpd  [rsp + 5*8], xmm5
        movlpd  [rsp + 6*8], xmm6
        movlpd  [rsp + 7*8], xmm7
        movlpd  [rsp + 8*8], xmm8
        movlpd  [rsp + 9*8], xmm9
        movlpd  [rsp + 10*8], xmm10
        movlpd  [rsp + 11*8], xmm11
        movlpd  [rsp + 12*8], xmm12
        movlpd  [rsp + 13*8], xmm13
        movlpd  [rsp + 14*8], xmm14
        movlpd  [rsp + 15*8], xmm15
    ; Call the garbage collector 
        call    _caml_garbage_collection
    ; Restore all regs used by the code generator 
        movlpd  xmm0, [rsp + 0*8]
        movlpd  xmm1, [rsp + 1*8]
        movlpd  xmm2, [rsp + 2*8]
        movlpd  xmm3, [rsp + 3*8]
        movlpd  xmm4, [rsp + 4*8]
        movlpd  xmm5, [rsp + 5*8]
        movlpd  xmm6, [rsp + 6*8]
        movlpd  xmm7, [rsp + 7*8]
        movlpd  xmm8, [rsp + 8*8]
        movlpd  xmm9, [rsp + 9*8]
        movlpd  xmm10, [rsp + 10*8]
        movlpd  xmm11, [rsp + 11*8]
        movlpd  xmm12, [rsp + 12*8]
        movlpd  xmm13, [rsp + 13*8]
        movlpd  xmm14, [rsp + 14*8]
        movlpd  xmm15, [rsp + 15*8]
        add     rsp, 16*8
        pop     rax
        pop     rbx
        pop     rdi
        pop     rsi
        pop     rdx
        pop     rcx
        pop     r8
        pop     r9
        pop     r10
        pop     r11
        pop     rbp
        pop     r12
        pop     r13
    ; Restore caml_young_ptr, caml_exception_pointer 
	mov	r15, _caml_young_ptr
	mov	r14, _caml_exception_pointer
    ; Return to caller 
        ret

        PUBLIC  _caml_alloc1
        ALIGN   16
_caml_alloc1:
        sub     r15, 16
        cmp     r15, _caml_young_limit
        jb      L100
        ret
L100:
        mov     rax, [rsp + 0]
        mov     _caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     _caml_bottom_of_stack, rax
	sub	rsp, 8
        call    L105
	add	rsp, 8
        jmp     _caml_alloc1

        PUBLIC  _caml_alloc2
        ALIGN   16
_caml_alloc2:
        sub     r15, 24
        cmp     r15, _caml_young_limit
        jb      L101
        ret
L101:
        mov     rax, [rsp + 0]
        mov     _caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     _caml_bottom_of_stack, rax
	sub	rsp, 8
        call    L105
	add	rsp, 8
        jmp     _caml_alloc2

        PUBLIC  _caml_alloc3
        ALIGN   16
_caml_alloc3:
        sub     r15, 32
        cmp     r15, _caml_young_limit
        jb      L102
        ret
L102:
        mov     rax, [rsp + 0]
        mov     _caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     _caml_bottom_of_stack, rax
	sub	rsp, 8
        call    L105
	add	rsp, 8
        jmp     _caml_alloc3

        PUBLIC  _caml_allocN
        ALIGN   16
_caml_allocN:
        sub     r15, rax
        cmp     r15, _caml_young_limit
        jb      L103
        ret
L103:
        push    rax                       ; save desired size 
        mov     rax, [rsp + 8]
        mov     _caml_last_return_address, rax
        lea     rax, [rsp + 16]
        mov     _caml_bottom_of_stack, rax
        call    L105
        pop     rax                      ; recover desired size 
        jmp     _caml_allocN

; Call a C function from Caml 

        PUBLIC  _caml_c_call
        ALIGN   16
_caml_c_call:
    ; Record lowest stack address and return address 
        pop     r12
        mov     _caml_last_return_address, r12
        mov     _caml_bottom_of_stack, rsp
    ; Make the exception handler and alloc ptr available to the C code 
	mov	_caml_young_ptr, r15
	mov	_caml_exception_pointer, r14
    ; Call the function (address in rax) 
        call    rax
    ; Reload alloc ptr 
	mov	r15, _caml_young_ptr
    ; Return to caller 
	push	r12
	ret

; Start the Caml program 

        PUBLIC  _caml_start_program
        ALIGN   16
_caml_start_program:
    ; Save callee-save registers 
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  [rsp + 0*16], xmm6
        movapd  [rsp + 1*16], xmm7
        movapd  [rsp + 2*16], xmm8
        movapd  [rsp + 3*16], xmm9
        movapd  [rsp + 4*16], xmm10
        movapd  [rsp + 5*16], xmm11
        movapd  [rsp + 6*16], xmm12
        movapd  [rsp + 7*16], xmm13
        movapd  [rsp + 8*16], xmm14
        movapd  [rsp + 9*16], xmm15
    ; Initial entry point is _caml_program 
        lea     r12, _caml_program
    ; Common code for caml_start_program and caml_callback* 
L106:
    ; Build a callback link 
	sub	rsp, 8	; stack 16-aligned 
        push    _caml_gc_regs
        push    _caml_last_return_address
        push    _caml_bottom_of_stack
    ; Setup alloc ptr and exception ptr 
	mov	r15, _caml_young_ptr
	mov	r14, _caml_exception_pointer
    ; Build an exception handler 
        lea     r13, L108
        push    r13
        push    r14
        mov     r14, rsp
    ; Call the Caml code 
        call    r12
L107:
    ; Pop the exception handler 
        pop     r14
        pop     r12    ; dummy register 
L109:
    ; Update alloc ptr and exception ptr 
	mov	_caml_young_ptr, r15
	mov	_caml_exception_pointer, r14
    ; Pop the callback restoring, link the global variables 
        pop     _caml_bottom_of_stack
        pop     _caml_last_return_address
        pop     _caml_gc_regs
	add	rsp, 8
    ; Restore callee-save registers. 
        movapd  xmm6, [rsp + 0*16]
        movapd  xmm7, [rsp + 1*16]
        movapd  xmm8, [rsp + 2*16]
        movapd  xmm9, [rsp + 3*16]
        movapd  xmm10, [rsp + 4*16]
        movapd  xmm11, [rsp + 5*16]
        movapd  xmm12, [rsp + 6*16]
        movapd  xmm13, [rsp + 7*16]
        movapd  xmm14, [rsp + 8*16]
        movapd  xmm15, [rsp + 9*16]
        add     rsp, 8+10*16
        pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rdi
        pop     rsi
        pop     rbp
        pop     rbx
    ; Return to caller
        ret
L108:
    ; Exception handler
    ; Mark the bucket as an exception result and return it 
        or      rax, 2
        jmp     L109

; Raise an exception from C 

        PUBLIC  _caml_raise_exception
        ALIGN   16
_caml_raise_exception:
        mov     rax, rcx             ; First argument is exn bucket
        mov     rsp, _caml_exception_pointer
        pop     r14                  ; Recover previous exception handler 
        mov     r15, _caml_young_ptr ; Reload alloc ptr 
        ret

; Callback from C to Caml 

        PUBLIC  _caml_callback_exn
        ALIGN   16
_caml_callback_exn:
    ; Save callee-save registers 
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  [rsp + 0*16], xmm6
        movapd  [rsp + 1*16], xmm7
        movapd  [rsp + 2*16], xmm8
        movapd  [rsp + 3*16], xmm9
        movapd  [rsp + 4*16], xmm10
        movapd  [rsp + 5*16], xmm11
        movapd  [rsp + 6*16], xmm12
        movapd  [rsp + 7*16], xmm13
        movapd  [rsp + 8*16], xmm14
        movapd  [rsp + 9*16], xmm15
    ; Initial loading of arguments 
        mov     rbx, rcx      ; closure 
        mov     rax, rdx      ; argument 
        mov     r12, [rbx]    ; code pointer 
        jmp     L106

        PUBLIC  _caml_callback2_exn
        ALIGN   16
_caml_callback2_exn:
    ; Save callee-save registers 
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  [rsp + 0*16], xmm6
        movapd  [rsp + 1*16], xmm7
        movapd  [rsp + 2*16], xmm8
        movapd  [rsp + 3*16], xmm9
        movapd  [rsp + 4*16], xmm10
        movapd  [rsp + 5*16], xmm11
        movapd  [rsp + 6*16], xmm12
        movapd  [rsp + 7*16], xmm13
        movapd  [rsp + 8*16], xmm14
        movapd  [rsp + 9*16], xmm15
    ; Initial loading of arguments 
        mov     rdi, rcx        ; closure
        mov     rax, rdx        ; first argument 
        mov     rbx, r8         ; second argument 
        lea     r12, _caml_apply2  ; code pointer 
        jmp     L106

        PUBLIC  _caml_callback3_exn
        ALIGN   16
_caml_callback3_exn:
    ; Save callee-save registers 
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  [rsp + 0*16], xmm6
        movapd  [rsp + 1*16], xmm7
        movapd  [rsp + 2*16], xmm8
        movapd  [rsp + 3*16], xmm9
        movapd  [rsp + 4*16], xmm10
        movapd  [rsp + 5*16], xmm11
        movapd  [rsp + 6*16], xmm12
        movapd  [rsp + 7*16], xmm13
        movapd  [rsp + 8*16], xmm14
        movapd  [rsp + 9*16], xmm15
    ; Initial loading of arguments 
        mov     rsi, rcx        ; closure
        mov     rax, rdx        ; first argument 
        mov     rbx, r8         ; second argument 
        mov     rdi, r9         ; third argument 
        lea     r12, _caml_apply3      ; code pointer 
        jmp     L106

        PUBLIC  _caml_ml_array_bound_error
        ALIGN   16
_caml_ml_array_bound_error:
    ; Make the exception handler and alloc ptr available to the C code 
	mov	_caml_young_ptr, r15
	mov	_caml_exception_pointer, r14
	jmp	_caml_array_bound_error	

        .DATA
        PUBLIC  _caml_system__frametable
_caml_system__frametable LABEL QWORD
        QWORD   1           ; one descriptor 
        QWORD   L107        ; return address into callback 
        WORD    -1          ; negative frame size => use callback link 
        WORD    0           ; no roots here 
        ALIGN   8

        PUBLIC  _caml_negf_mask
        ALIGN   16
_caml_negf_mask:
	QWORD	0x8000000000000000, 0

        PUBLIC  _caml_absf_mask
        ALIGN   16
_caml_absf_mask:
	QWORD	0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

        END
