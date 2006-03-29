;*********************************************************************
;*                                                                   *
;*                          Objective Caml                           *
;*                                                                   *
;*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        *
;*                                                                   *
;* Copyright 1996 Institut National de Recherche en Informatique et  *
;* en Automatique.  All rights reserved.  This file is distributed   *
;* under the terms of the GNU Library General Public License, with   *
;* the special exception on linking described in file ../LICENSE.    *
;*                                                                   *
;*********************************************************************

; $Id$

; Asm part of the runtime system for the HP PA-RISC processor.
; Must be preprocessed by cpp

#ifdef SYS_hpux
#define G(x) x
#define CODESPACE .code
#define CODE_ALIGN 4
#define EXPORT_CODE(x) .export x, entry, priv_lev=3
#define EXPORT_DATA(x) .export x, data
#define STARTPROC .proc ! .callinfo frame=0, no_calls ! .entry
#define ENDPROC .exit ! .procend
#define LOADHIGH(x) addil LR%x-$global$, %r27
#define LOW(x) RR%x-$global$
#define LOADHIGHLABEL(x) ldil LR%x, %r1
#define LOWLABEL(x) RR%x
#endif

#if defined(SYS_linux) || defined(SYS_gnu)
#define G(x) x
#define CODESPACE .text
#define CODE_ALIGN 8
#define EXPORT_CODE(x) .globl x
#define EXPORT_DATA(x) .globl x
#define STARTPROC
#define ENDPROC
#define LOADHIGH(x) addil LR%x-$global$, %r27
#define LOW(x) RR%x-$global$
#define LOADHIGHLABEL(x) ldil LR%x, %r1
#define LOWLABEL(x) RR%x
#endif

#ifdef SYS_hpux
	.space $PRIVATE$
	.subspa $DATA$,quad=1,align=8,access=31
	.subspa $BSS$,quad=1,align=8,access=31,zero,sort=82
	.space $TEXT$
	.subspa $LIT$,quad=0,align=8,access=44
	.subspa $CODE$,quad=0,align=8,access=44,code_only
	.import $global$, data
        .import $$dyncall, millicode
	.import caml_garbage_collection, code
	.import caml_program, code
	.import caml_raise, code
	.import caml_apply2, code
	.import caml_apply3, code
	.import caml_array_bound_error, code

caml_young_limit             .comm 8
caml_young_ptr               .comm 8
caml_bottom_of_stack         .comm 8
caml_last_return_address     .comm 8
caml_gc_regs                 .comm 8
caml_exception_pointer       .comm 8
caml_required_size           .comm 8
#endif

#if defined(SYS_linux) || defined(SYS_gnu)
	.align	8
	.comm	G(young_limit), 4
	.comm	G(young_ptr), 4
	.comm	G(caml_bottom_of_stack), 4
	.comm	G(caml_last_return_address), 4
	.comm	G(caml_gc_regs), 4
	.comm	G(caml_exception_pointer), 4
	.comm	G(caml_required_size), 4
#endif

; Allocation functions

        CODESPACE
	.align	CODE_ALIGN
        EXPORT_CODE(G(caml_allocN))
G(caml_allocN):
        STARTPROC
; Required size in %r29
        ldw     0(%r4), %r1
        sub     %r3, %r29, %r3
        comb,<<,n %r3, %r1, G(caml_call_gc) ; nullify if taken (forward br.)
        bv      0(%r2)
        nop
        ENDPROC

        EXPORT_CODE(G(caml_call_gc))
G(caml_call_gc):
        STARTPROC
; Save required size (%r29)
        LOADHIGH(G(caml_required_size))
        stw     %r29, LOW(G(caml_required_size))(%r1)
; Save current allocation pointer for debugging purposes
        LOADHIGH(G(caml_young_ptr))
        stw     %r3, LOW(G(caml_young_ptr))(%r1)
; Record lowest stack address
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r30, LOW(G(caml_bottom_of_stack))(%r1)
; Record return address
        LOADHIGH(G(caml_last_return_address))
        stw     %r2, LOW(G(caml_last_return_address))(%r1)
; Save the exception handler (if e.g. a sighandler raises)
        LOADHIGH(G(caml_exception_pointer))
        stw     %r5, LOW(G(caml_exception_pointer))(%r1)
; Reserve stack space
; 0x1C0 = 4 * 32 (int regs) + 8 * 32 (float regs) + 64 (for calling C)
        ldo     0x1C0(%r30), %r30
; Save caml_gc_regs
L100:   ldo     -(64 + 4*32)(%r30), %r31
        LOADHIGH(G(caml_gc_regs))
        stw     %r31, LOW(G(caml_gc_regs))(%r1)
; Save all regs used by the code generator
        copy    %r31, %r1
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
        ldo     -0x1C0(%r30), %r1
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
        bl      G(caml_garbage_collection), %r2
        nop

; Restore all regs used by the code generator
        ldo     -(64 + 4*32)(%r30), %r1
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
        ldo     -0x1C0(%r30), %r1
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
        LOADHIGH(G(caml_young_ptr))
        ldw     LOW(G(caml_young_ptr))(%r1), %r3
; Allocate space for block
        LOADHIGH(G(caml_required_size))
        ldw     LOW(G(caml_required_size))(%r1), %r29
        ldw     0(%r4), %r1
        sub     %r3, %r29, %r3
        comb,<< %r3, %r1, L100
        nop
; Return to caller
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r2
        bv      0(%r2)
        ldo     -0x1C0(%r30), %r30
        ENDPROC

; Call a C function from Caml
; Function to call is in %r22

	.align	CODE_ALIGN
#ifdef SYS_hpux
        .export G(caml_c_call), ENTRY, ARGW0=GR, ARGW1=GR, ARGW2=GR, ARGW3=GR
#else
	EXPORT_CODE(G(caml_c_call))
#endif
G(caml_c_call):
        STARTPROC
; Record lowest stack address
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r30, LOW(G(caml_bottom_of_stack))(%r1)
; Record return address
        LOADHIGH(G(caml_last_return_address))
        stw     %r2, LOW(G(caml_last_return_address))(%r1)
; Save the exception handler
        LOADHIGH(G(caml_exception_pointer))
        stw     %r5, LOW(G(caml_exception_pointer))(%r1)
; Save the allocation pointer
        LOADHIGH(G(caml_young_ptr))
        stw     %r3, LOW(G(caml_young_ptr))(%r1)
; Call the C function
#ifdef SYS_hpux
        bl      $$dyncall, %r31
#else
        ble     0(4, %r22)
#endif
        copy    %r31, %r2                       ; in delay slot
; Reload return address
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r2
; Reload allocation pointer
        LOADHIGH(G(caml_young_ptr))
; Return to caller
        bv      0(%r2)
        ldw     LOW(G(caml_young_ptr))(%r1), %r3   ; in delay slot
        ENDPROC

; Start the Caml program

	.align	CODE_ALIGN
	EXPORT_CODE(G(caml_start_program))
G(caml_start_program):
        STARTPROC
        LOADHIGH(G(caml_program))
        ldo     LOW(G(caml_program))(%r1), %r22

; Code shared with caml_callback*
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
        ldo     16(%r30), %r30
        LOADHIGH(G(caml_bottom_of_stack))
        ldw     LOW(G(caml_bottom_of_stack))(%r1), %r1
        stw     %r1, -16(%r30)
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r1
        stw     %r1, -12(%r30)
        LOADHIGH(G(caml_gc_regs))
        ldw     LOW(G(caml_gc_regs))(%r1), %r1
        stw     %r1, -8(%r30)
; Set up a trap frame to catch exceptions escaping the Caml code
        ldo     8(%r30), %r30
        LOADHIGH(G(caml_exception_pointer))
        ldw     LOW(G(caml_exception_pointer))(%r1), %r1
        stw     %r1, -8(%r30)
        LOADHIGHLABEL(L103)
        ldo     LOWLABEL(L103)(%r1), %r1
        stw     %r1, -4(%r30)
        copy    %r30, %r5
; Reload allocation pointers
        LOADHIGH(G(caml_young_ptr))
        ldw     LOW(G(caml_young_ptr))(%r1), %r3
        LOADHIGH(G(caml_young_limit))
        ldo     LOW(G(caml_young_limit))(%r1), %r4
; Call the Caml code
        ble     0(4, %r22)
        copy    %r31, %r2        
L104:
; Pop the trap frame
        ldw     -8(%r30), %r31
        LOADHIGH(G(caml_exception_pointer))
        stw     %r31, LOW(G(caml_exception_pointer))(%r1)
        ldo     -8(%r30), %r30
; Pop the callback link
L105:
        ldw     -16(%r30), %r31
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r31, LOW(G(caml_bottom_of_stack))(%r1)
        ldw     -12(%r30), %r31
        LOADHIGH(G(caml_last_return_address))
        stw     %r31, LOW(G(caml_last_return_address))(%r1)
        ldw     -8(%r30), %r31
        LOADHIGH(G(caml_gc_regs))
        stw     %r31, LOW(G(caml_gc_regs))(%r1)
        ldo     -16(%r30), %r30
; Save allocation pointer
        LOADHIGH(G(caml_young_ptr))
        stw     %r3, LOW(G(caml_young_ptr))(%r1)
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
; Save exception pointer
        LOADHIGH(G(caml_exception_pointer))
        stw     %r5, LOW(G(caml_exception_pointer))(%r1)
; Encode exception bucket as an exception result and return it
        ldi     2, %r1
        or      %r26, %r1, %r26
; Return it
        b       L105
        nop

; Re-raise the exception through caml_raise, to clean up local C roots
        ldo     64(%r30), %r30
        bl      G(caml_raise), %r2
        nop
        ENDPROC

; Raise an exception from C

	.align	CODE_ALIGN
	EXPORT_CODE(G(caml_raise_exception))
G(caml_raise_exception):
        STARTPROC
; Cut the stack
        LOADHIGH(G(caml_exception_pointer))
        ldw     LOW(G(caml_exception_pointer))(%r1), %r30
; Reload allocation registers
        LOADHIGH(G(caml_young_ptr))
        ldw     LOW(G(caml_young_ptr))(%r1), %r3
        LOADHIGH(G(caml_young_limit))
        ldo     LOW(G(caml_young_limit))(%r1), %r4
; Raise the exception
        ldw     -4(%r30), %r1
        ldw     -8(%r30), %r5
        bv      0(%r1)
        ldo     -8(%r30), %r30  ; in delay slot
        ENDPROC

; Callbacks C -> ML

        .align	CODE_ALIGN
        EXPORT_CODE(G(caml_callback_exn))
G(caml_callback_exn):
        STARTPROC
; Initial shuffling of arguments
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; Argument
        copy    %r1, %r25
        b       L102
        ldw     0(%r1), %r22    ; Code to call (in delay slot)
        ENDPROC

        .align	CODE_ALIGN
        EXPORT_CODE(G(caml_callback2_exn))
G(caml_callback2_exn):
        STARTPROC
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; First argument
        copy    %r24, %r25      ; Second argument
        copy    %r1, %r24
        LOADHIGH(G(caml_apply2))
        b       L102
        ldo     LOW(G(caml_apply2))(%r1), %r22
        ENDPROC

        .align	CODE_ALIGN
        EXPORT_CODE(G(caml_callback3_exn))
G(caml_callback3_exn):
        STARTPROC
        copy    %r26, %r1       ; Closure
        copy    %r25, %r26      ; First argument
        copy    %r24, %r25      ; Second argument
        copy    %r23, %r24      ; Third argument
        copy    %r1, %r23
        LOADHIGH(G(caml_apply3))
        b       L102
        ldo     LOW(G(caml_apply3))(%r1), %r22
        ENDPROC

        .align  CODE_ALIGN
        EXPORT_CODE(G(caml_ml_array_bound_error))
G(caml_ml_array_bound_error):
        STARTPROC
; Load address of [caml_array_bound_error] in %r22
        ldil    LR%caml_array_bound_error, %r22
        ldo     RR%caml_array_bound_error(%r22), %r22
; Reserve 48 bytes of stack space and jump to caml_c_call
        b       G(caml_c_call)
	ldo	48(%r30), %r30	/* in delay slot */
        ENDPROC

        .data
        EXPORT_DATA(G(caml_system__frametable))
G(caml_system__frametable):
        .long   1               /* one descriptor */
        .long   L104 + 3        /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
