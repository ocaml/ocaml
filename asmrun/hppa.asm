;*********************************************************************
;*                                                                   *
;*                        Caml Special Light                         *
;*                                                                   *
;*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        *
;*                                                                   *
;* Copyright 1995 Institut National de Recherche en Informatique et  *
;* Automatique.  Distributed only by permission.                     *
;*                                                                   *
;*********************************************************************

; $Id$

; Asm part of the suntime system for the HP PA-RISC processor.

	.comm	_young_limit, 8
	.comm	_young_ptr, 8
	.comm	_gc_entry_regs, 32 * 4
	.comm	_gc_entry_float_regs, 32 * 8
	.comm	_caml_top_of_stack, 8
	.comm	_caml_bottom_of_stack, 8
	.comm	_caml_last_return_address, 8
	.comm	_caml_exception_pointer, 8
	.comm	_caml_required_size, 8

; Allocation functions

        .text
	.align	2
        .globl  _caml_alloc
_caml_alloc:
; Required size in %r1
        ldw     0(%r4), %r31
        sub     %r3, %r1, %r3
        comb,<<,n %r3, %r31, _caml_call_gc ; nullify if taken (forward branch)
        bv      0(%r2)
        nop

        .globl  _caml_call_gc
_caml_call_gc:
; Save required size (%r1)
        ldil    L`_caml_required_size, %r31
        stw     %r1, R`_caml_required_size(%r31)
; Save current allocation pointer for debugging purposes
        ldil    L`_young_ptr, %r1
        stw     %r3, R`_young_ptr(%r1)
; Record lowest stack address
        ldil    L`_caml_bottom_of_stack, %r1
        stw     %r30, R`_caml_bottom_of_stack(%r1)
; Record return address
        ldil    L`_caml_last_return_address, %r1
        stw     %r2, R`_caml_last_return_address(%r1)
; Save the exception handler (if e.g. a sighandler raises)
        ldil    L`_caml_exception_pointer, %r1
        stw     %r5, R`_caml_exception_pointer(%r1)
; Save all regs used by the code generator
        ldil	L`_gc_entry_regs, %r1
        ldo     R`_gc_entry_regs(%r1), %r1
        stws,ma %r6, 4(%r1)
        stws,ma %r7, 4(%r1)
        stws,ma %r8, 4(%r1)
        stws,ma %r9, 4(%r1)
        stws,ma %r10, 4(%r1)
        stws,ma %r11, 4(%r1)
        stws,ma %r12, 4(%r1)
        stws,ma %r13, 4(%r1)
        stws,ma %r14, 4(%r1)
        stws,ma %r15, 4(%r1)
        stws,ma %r16, 4(%r1)
        stws,ma %r17, 4(%r1)
        stws,ma %r18, 4(%r1)
        stws,ma %r19, 4(%r1)
        stws,ma %r20, 4(%r1)
        stws,ma %r21, 4(%r1)
        stws,ma %r22, 4(%r1)
        stws,ma %r23, 4(%r1)
        stws,ma %r24, 4(%r1)
        stws,ma %r25, 4(%r1)
        stws,ma %r26, 4(%r1)
        stws,ma %r28, 4(%r1)
        stws,ma %r29, 4(%r1)
        ldil	L`_gc_entry_float_regs, %r1
        ldo     R`_gc_entry_float_regs(%r1), %r1
        fstds,ma %fr4, 8(%r1)
        fstds,ma %fr5, 8(%r1)
        fstds,ma %fr6, 8(%r1)
        fstds,ma %fr7, 8(%r1)
        fstds,ma %fr8, 8(%r1)
        fstds,ma %fr9, 8(%r1)
        fstds,ma %fr10, 8(%r1)
        fstds,ma %fr11, 8(%r1)
        fstds,ma %fr12, 8(%r1)
        fstds,ma %fr13, 8(%r1)
        fstds,ma %fr14, 8(%r1)
        fstds,ma %fr15, 8(%r1)
        fstds,ma %fr16, 8(%r1)
        fstds,ma %fr17, 8(%r1)
        fstds,ma %fr18, 8(%r1)
        fstds,ma %fr19, 8(%r1)
        fstds,ma %fr20, 8(%r1)
        fstds,ma %fr21, 8(%r1)
        fstds,ma %fr22, 8(%r1)
        fstds,ma %fr23, 8(%r1)
        fstds,ma %fr24, 8(%r1)
        fstds,ma %fr25, 8(%r1)
        fstds,ma %fr26, 8(%r1)
        fstds,ma %fr27, 8(%r1)
        fstds,ma %fr28, 8(%r1)
        fstds,ma %fr29, 8(%r1)
        fstds,ma %fr30, 8(%r1)

; Call the garbage collector
        ldo     64(%r30), %r30
        ldil    L`_garbage_collection, %r1
        ble     R`_garbage_collection(4, %r1)
        copy    %r31, %r2
        ldo     -64(%r30), %r30

; Restore all regs used by the code generator
        ldil	L`_gc_entry_regs, %r1
        ldo     R`_gc_entry_regs(%r1), %r1
        ldws,ma 4(%r1), %r6
        ldws,ma 4(%r1), %r7
        ldws,ma 4(%r1), %r8
        ldws,ma 4(%r1), %r9
        ldws,ma 4(%r1), %r10
        ldws,ma 4(%r1), %r11
        ldws,ma 4(%r1), %r12
        ldws,ma 4(%r1), %r13
        ldws,ma 4(%r1), %r14
        ldws,ma 4(%r1), %r15
        ldws,ma 4(%r1), %r16
        ldws,ma 4(%r1), %r17
        ldws,ma 4(%r1), %r18
        ldws,ma 4(%r1), %r19
        ldws,ma 4(%r1), %r20
        ldws,ma 4(%r1), %r21
        ldws,ma 4(%r1), %r22
        ldws,ma 4(%r1), %r23
        ldws,ma 4(%r1), %r24
        ldws,ma 4(%r1), %r25
        ldws,ma 4(%r1), %r26
        ldws,ma 4(%r1), %r28
        ldws,ma 4(%r1), %r29
        ldil	L`_gc_entry_float_regs, %r1
        ldo     R`_gc_entry_float_regs(%r1), %r1
        fldds,ma 8(%r1), %fr4
        fldds,ma 8(%r1), %fr5
        fldds,ma 8(%r1), %fr6
        fldds,ma 8(%r1), %fr7
        fldds,ma 8(%r1), %fr8
        fldds,ma 8(%r1), %fr9
        fldds,ma 8(%r1), %fr10
        fldds,ma 8(%r1), %fr11
        fldds,ma 8(%r1), %fr12
        fldds,ma 8(%r1), %fr13
        fldds,ma 8(%r1), %fr14
        fldds,ma 8(%r1), %fr15
        fldds,ma 8(%r1), %fr16
        fldds,ma 8(%r1), %fr17
        fldds,ma 8(%r1), %fr18
        fldds,ma 8(%r1), %fr19
        fldds,ma 8(%r1), %fr20
        fldds,ma 8(%r1), %fr21
        fldds,ma 8(%r1), %fr22
        fldds,ma 8(%r1), %fr23
        fldds,ma 8(%r1), %fr24
        fldds,ma 8(%r1), %fr25
        fldds,ma 8(%r1), %fr26
        fldds,ma 8(%r1), %fr27
        fldds,ma 8(%r1), %fr28
        fldds,ma 8(%r1), %fr29
        fldds,ma 8(%r1), %fr30

; Reload the allocation pointer
        ldil    L`_young_ptr, %r1
        ldw     R`_young_ptr(%r1), %r3
; Allocate space for block
        ldil    L`_caml_required_size, %r1
        ldw     R`_caml_required_size(%r1), %r1
        sub     %r3, %r1, %r3
; Return to caller
        ldil    L`_caml_last_return_address, %r1
        ldw     R`_caml_last_return_address(%r1), %r2
        bv      0(%r2)
        nop

; Call a C function from Caml
; Function to call is in %r22

	.align	2
	.globl  _caml_c_call
_caml_c_call:
; Record lowest stack address
        ldil    L`_caml_bottom_of_stack, %r1
        stw     %r30, R`_caml_bottom_of_stack(%r1)
; Record return address
        ldil    L`_caml_last_return_address, %r1
        stw     %r2, R`_caml_last_return_address(%r1)
; Save the exception handler
        ldil    L`_caml_exception_pointer, %r1
        stw     %r5, R`_caml_exception_pointer(%r1)
; Save the allocation pointer
        ldil    L`_young_ptr, %r1
        stw     %r3, R`_young_ptr(%r1)   ; in delay slot
; Call the C function
        ble     0(4, %r22)
        copy    %r31, %r2        
; Reload return address
        ldil    L`_caml_last_return_address, %r1
        ldw     R`_caml_last_return_address(%r1), %r2
; Reload allocation pointer
        ldil    L`_young_ptr, %r1
; Return to caller
        bv      0(%r2)
        ldw     R`_young_ptr(%r1), %r3   ; in delay slot

; Start the Caml program

	.align	2
	.globl  _caml_start_program
_caml_start_program:
	stw     %r2,-20(%r30)
        ldo	256(%r30), %r30
; Save the callee-save registers
        ldo     -32(%r30), %r1
        stws,ma %r3, -4(%r1)
        stws,ma %r4, -4(%r1)
        stws,ma %r5, -4(%r1)
        stws,ma %r6, -4(%r1)
        stws,ma %r7, -4(%r1)
        stws,ma %r8, -4(%r1)
        stws,ma %r9, -4(%r1)
        stws,ma %r10, -4(%r1)
        stws,ma %r11, -4(%r1)
        stws,ma %r12, -4(%r1)
        stws,ma %r13, -4(%r1)
        stws,ma %r14, -4(%r1)
        stws,ma %r15, -4(%r1)
        stws,ma %r16, -4(%r1)
        stws,ma %r17, -4(%r1)
        stws,ma %r18, -4(%r1)
	fstds,ma %fr12, -8(%r1)
	fstds,ma %fr13, -8(%r1)
	fstds,ma %fr14, -8(%r1)
	fstds,ma %fr15, -8(%r1)
	fstds,ma %fr16, -8(%r1)
	fstds,ma %fr17, -8(%r1)
	fstds,ma %fr18, -8(%r1)
	fstds,ma %fr19, -8(%r1)
	fstds,ma %fr20, -8(%r1)
	fstds,ma %fr21, -8(%r1)
	fstds,ma %fr22, -8(%r1)
	fstds,ma %fr23, -8(%r1)
	fstds,ma %fr24, -8(%r1)
	fstds,ma %fr25, -8(%r1)
	fstds,ma %fr26, -8(%r1)
	fstds,ma %fr27, -8(%r1)
	fstds,ma %fr28, -8(%r1)
	fstds,ma %fr29, -8(%r1)
	fstds,ma %fr30, -8(%r1)
	fstds,ma %fr31, -8(%r1)

; Build an exception handler
        bl      L100, %r1
        nop
        copy    %r26, %r28              ; return exception bucket as result
        b       L101
        nop
L100:
        ldo     8(%r30), %r30
        stw     %r1, -4(%r30)
        copy    %r30, %r5
; Record highest stack address
        ldil    L`_caml_top_of_stack, %r1
        stw     %r30, R`_caml_top_of_stack(%r1)
; Initialize allocation registers
        ldil    L`_young_ptr, %r1
        ldw     R`_young_ptr(%r1), %r3
        ldil    L`_young_limit, %r1
        ldo     R`_young_limit(%r1), %r4
; Go for it
        ldil    L`_caml_program, %r1
        ble     R`_caml_program(4, %r1)
        copy    %r31, %r2
; Pop handler
        ldo     -8(%r30), %r30
; Return with zero result
        ldi     0, %r28
; Restore callee-save registers
L101:
        ldo     -32(%r30), %r1
        ldws,ma -4(%r1), %r3
        ldws,ma -4(%r1), %r4
        ldws,ma -4(%r1), %r5
        ldws,ma -4(%r1), %r6
        ldws,ma -4(%r1), %r7
        ldws,ma -4(%r1), %r8
        ldws,ma -4(%r1), %r9
        ldws,ma -4(%r1), %r10
        ldws,ma -4(%r1), %r11
        ldws,ma -4(%r1), %r12
        ldws,ma -4(%r1), %r13
        ldws,ma -4(%r1), %r14
        ldws,ma -4(%r1), %r15
        ldws,ma -4(%r1), %r16
        ldws,ma -4(%r1), %r17
        ldws,ma -4(%r1), %r18
	fldds,ma -8(%r1), %fr12
	fldds,ma -8(%r1), %fr13
	fldds,ma -8(%r1), %fr14
	fldds,ma -8(%r1), %fr15
	fldds,ma -8(%r1), %fr16
	fldds,ma -8(%r1), %fr17
	fldds,ma -8(%r1), %fr18
	fldds,ma -8(%r1), %fr19
	fldds,ma -8(%r1), %fr20
	fldds,ma -8(%r1), %fr21
	fldds,ma -8(%r1), %fr22
	fldds,ma -8(%r1), %fr23
	fldds,ma -8(%r1), %fr24
	fldds,ma -8(%r1), %fr25
	fldds,ma -8(%r1), %fr26
	fldds,ma -8(%r1), %fr27
	fldds,ma -8(%r1), %fr28
	fldds,ma -8(%r1), %fr29
	fldds,ma -8(%r1), %fr30
	fldds,ma -8(%r1), %fr31
; Return to C
        ldo	-256(%r30), %r30
	ldw     -20(%r30), %r2
        bv      0(%r2)
        nop

; Raise an exception from C

	.align	2
	.globl  _raise_caml_exception
_raise_caml_exception:
; Cut the stack
        ldil    L`_caml_exception_pointer, %r1
        ldw     R`_caml_exception_pointer(%r1), %r30
; Reload allocation registers
        ldil    L`_young_ptr, %r1
        ldw     R`_young_ptr(%r1), %r3
        ldil    L`_young_limit, %r1
        ldo     R`_young_limit(%r1), %r4
; Raise the exception
        ldw     -4(%r30), %r1
        ldw     -8(%r30), %r5
        bv      0(%r1)
        ldo     -8(%r30), %r30  ; in delay slot

; Callbacks C -> ML

        .align  2
        .globl  _callback
_callback:
; Initial shuffling of arguments
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; Argument
        copy    %r1, %r25
        ldw     0(%r1), %r22    ; Code to call
L102:
; Save return address
	stw     %r2,-20(%r30)
        ldo	256(%r30), %r30
; Save the callee-save registers
        ldo     -32(%r30), %r1
        stws,ma %r3, -4(%r1)
        stws,ma %r4, -4(%r1)
        stws,ma %r5, -4(%r1)
        stws,ma %r6, -4(%r1)
        stws,ma %r7, -4(%r1)
        stws,ma %r8, -4(%r1)
        stws,ma %r9, -4(%r1)
        stws,ma %r10, -4(%r1)
        stws,ma %r11, -4(%r1)
        stws,ma %r12, -4(%r1)
        stws,ma %r13, -4(%r1)
        stws,ma %r14, -4(%r1)
        stws,ma %r15, -4(%r1)
        stws,ma %r16, -4(%r1)
        stws,ma %r17, -4(%r1)
        stws,ma %r18, -4(%r1)
	fstds,ma %fr12, -8(%r1)
	fstds,ma %fr13, -8(%r1)
	fstds,ma %fr14, -8(%r1)
	fstds,ma %fr15, -8(%r1)
	fstds,ma %fr16, -8(%r1)
	fstds,ma %fr17, -8(%r1)
	fstds,ma %fr18, -8(%r1)
	fstds,ma %fr19, -8(%r1)
	fstds,ma %fr20, -8(%r1)
	fstds,ma %fr21, -8(%r1)
	fstds,ma %fr22, -8(%r1)
	fstds,ma %fr23, -8(%r1)
	fstds,ma %fr24, -8(%r1)
	fstds,ma %fr25, -8(%r1)
	fstds,ma %fr26, -8(%r1)
	fstds,ma %fr27, -8(%r1)
	fstds,ma %fr28, -8(%r1)
	fstds,ma %fr29, -8(%r1)
	fstds,ma %fr30, -8(%r1)
	fstds,ma %fr31, -8(%r1)
; Set up a callback link
        ldo     8(%r30), %r30
        ldil    L`_caml_bottom_of_stack, %r1
        ldw     R`_caml_bottom_of_stack(%r1), %r1
        stw     %r1, -8(%r30)
        ldil    L`_caml_last_return_address, %r1
        ldw     R`_caml_last_return_address(%r1), %r1
        stw     %r1, -4(%r30)
; Set up a trap frame to catch exceptions escaping the Caml code
        ldo     8(%r30), %r30
        ldil    L`_caml_exception_pointer, %r1
        ldw     R`_caml_exception_pointer(%r1), %r1
        stw     %r1, -8(%r30)
        ldil    L`L103, %r1
        ldo     R`L103(%r1), %r1
        stw     %r1, -4(%r30)
        copy    %r30, %r5
; Reload allocation pointers
        ldil    L`_young_ptr, %r1
        ldw     R`_young_ptr(%r1), %r3
        ldil    L`_young_limit, %r1
        ldo     R`_young_limit(%r1), %r4
; Call the Caml code
        ble     0(4, %r22)
        copy    %r31, %r2        
L104:
; Pop the trap frame
        ldw     -8(%r30), %r31
        ldil    L`_caml_exception_pointer, %r1
        stw     %r31, R`_caml_exception_pointer(%r1)
        ldo     -8(%r30), %r30
; Pop the callback link
        ldw     -8(%r30), %r31
        ldil    L`_caml_bottom_of_stack, %r1
        stw     %r31, R`_caml_bottom_of_stack(%r1)
        ldw     -4(%r30), %r31
        ldil    L`_caml_last_return_address, %r1
        stw     %r31, R`_caml_last_return_address(%r1)
        ldo     -8(%r30), %r30
; Save allocation pointer
        ldil    L`_young_ptr, %r1
        stw     %r3, R`_young_ptr(%r1)
; Move result where C function expects it
        copy    %r26, %r28
; Reload callee-save registers
        ldo     -32(%r30), %r1
        ldws,ma -4(%r1), %r3
        ldws,ma -4(%r1), %r4
        ldws,ma -4(%r1), %r5
        ldws,ma -4(%r1), %r6
        ldws,ma -4(%r1), %r7
        ldws,ma -4(%r1), %r8
        ldws,ma -4(%r1), %r9
        ldws,ma -4(%r1), %r10
        ldws,ma -4(%r1), %r11
        ldws,ma -4(%r1), %r12
        ldws,ma -4(%r1), %r13
        ldws,ma -4(%r1), %r14
        ldws,ma -4(%r1), %r15
        ldws,ma -4(%r1), %r16
        ldws,ma -4(%r1), %r17
        ldws,ma -4(%r1), %r18
	fldds,ma -8(%r1), %fr12
	fldds,ma -8(%r1), %fr13
	fldds,ma -8(%r1), %fr14
	fldds,ma -8(%r1), %fr15
	fldds,ma -8(%r1), %fr16
	fldds,ma -8(%r1), %fr17
	fldds,ma -8(%r1), %fr18
	fldds,ma -8(%r1), %fr19
	fldds,ma -8(%r1), %fr20
	fldds,ma -8(%r1), %fr21
	fldds,ma -8(%r1), %fr22
	fldds,ma -8(%r1), %fr23
	fldds,ma -8(%r1), %fr24
	fldds,ma -8(%r1), %fr25
	fldds,ma -8(%r1), %fr26
	fldds,ma -8(%r1), %fr27
	fldds,ma -8(%r1), %fr28
	fldds,ma -8(%r1), %fr29
	fldds,ma -8(%r1), %fr30
	fldds,ma -8(%r1), %fr31
; Return to C
        ldo	-256(%r30), %r30
	ldw     -20(%r30), %r2
        bv      0(%r2)
        nop
; The trap handler
L103:
; Pop the callback link
        ldw     -8(%r30), %r31
        ldil    L`_caml_bottom_of_stack, %r1
        stw     %r31, R`_caml_bottom_of_stack(%r1)
        ldw     -4(%r30), %r31
        ldil    L`_caml_last_return_address, %r1
        stw     %r31, R`_caml_last_return_address(%r1)
        ldo     -8(%r30), %r30
; Save allocation pointer and exception pointer
        ldil    L`_young_ptr, %r1
        stw     %r3, R`_young_ptr(%r1)
        ldil    L`_caml_exception_pointer, %r1
        stw     %r5, R`_caml_exception_pointer(%r1)
; Re-raise the exception through mlraise, to clean up local C roots
        ldo     64(%r30), %r30
        ldil    L`_mlraise, %r1
        ble     R`_mlraise(4, %r1)
        copy    %r31, %r2               ; actually, never returns

        .align  2
        .globl  _callback2
_callback2:
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; First argument
        copy    %r24, %r25      ; Second argument
        copy    %r1, %r24
        ldil    L`_caml_apply2, %r22
        b       L102
        ldo     R`_caml_apply2(%r22), %r22

        .align  2
        .globl  _callback3
_callback3:
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; First argument
        copy    %r24, %r25      ; Second argument
        copy    %r23, %r24      ; Third argument
        copy    %r1, %r23
        ldil    L`_caml_apply3, %r22
        b       L102
        ldo     R`_caml_apply3(%r22), %r22

        .data
        .globl  _system_frametable
_system_frametable:
        .long   1               /* one descriptor */
        .long   L104 + 3        /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
