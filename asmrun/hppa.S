;*********************************************************************
;*                                                                   *
;*                          Objective Caml                           *
;*                                                                   *
;*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        *
;*                                                                   *
;* Copyright 1996 Institut National de Recherche en Informatique et  *
;* Automatique.  Distributed only by permission.                     *
;*                                                                   *
;*********************************************************************

; $Id$

; Asm part of the suntime system for the HP PA-RISC processor.
; Must be preprocessed by cpp

#ifdef SYS_hpux
#define G(x) x
#define CODE .code
#define CODE_ALIGN 4
#define EXPORT_CODE(x) .export x, entry, priv_lev=3
#define EXPORT_DATA(x) .export x, data
#define COMM(x,size) x .comm size
#define STARTPROC .proc ! .callinfo frame=0, no_calls ! .entry
#define ENDPROC .exit ! .procend
#define LOADHIGH(x) addil LR%x-$global$, %r27
#define LOW(x) RR%x-$global$
#define LOADHIGHLABEL(x) addil LR%x, %r27
#define LOWLABEL(x) RR%x
#define CALL(x) bl x, %r2 ! nop
#endif

#ifdef SYS_nextstep
#define G(x) _##x
#define CODE .text
#define CODE_ALIGN 2
#define EXPORT_CODE(x) .globl x
#define EXPORT_DATA(x) .globl x
#define COMM(x,size) .comm x, size
#define STARTPROC
#define ENDPROC
#define LOADHIGH(x) ldil L`x, %r1
#define LOW(x) R`x
#define LOADHIGHLABEL(x) ldil L`x, %r1
#define LOWLABEL(x) R`x
#define CALL(x) ldil L`x, %r1; ble R`x(4, %r1); copy %r31, %r2
#define WORD .long
#define HALF .short
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
	.import garbage_collection, code
	.import caml_program, code
	.import mlraise, code
	.import caml_apply2, code
	.import caml_apply3, code
#endif

COMM(G(young_limit), 8)
COMM(G(young_ptr), 8)
COMM(G(gc_entry_regs), 32 * 4)
COMM(G(gc_entry_float_regs), 32 * 8)
COMM(G(caml_top_of_stack), 8)
COMM(G(caml_bottom_of_stack), 8)
COMM(G(caml_last_return_address), 8)
COMM(G(caml_exception_pointer), 8)
COMM(G(caml_required_size), 8)

; Allocation functions

        CODE
	.align	CODE_ALIGN
        EXPORT_CODE(G(caml_alloc))
G(caml_alloc):
        STARTPROC
; Required size in %r1
        ldw     0(%r4), %r31
        sub     %r3, %r1, %r3
        comb,<<,n %r3, %r31, G(caml_call_gc) ; nullify if taken (forward br.)
        bv      0(%r2)
        nop
        ENDPROC

        EXPORT_CODE(G(caml_call_gc))
G(caml_call_gc):
        STARTPROC
; Save required size (%r1)
        copy    %r1, %r31
        LOADHIGH(G(caml_required_size))
        stw     %r31, LOW(G(caml_required_size))(%r1)
; Save current allocation pointer for debugging purposes
        LOADHIGH(G(young_ptr))
        stw     %r3, LOW(G(young_ptr))(%r1)
; Record lowest stack address
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r30, LOW(G(caml_bottom_of_stack))(%r1)
; Record return address
        LOADHIGH(G(caml_last_return_address))
        stw     %r2, LOW(G(caml_last_return_address))(%r1)
; Save the exception handler (if e.g. a sighandler raises)
        LOADHIGH(G(caml_exception_pointer))
        stw     %r5, LOW(G(caml_exception_pointer))(%r1)
; Save all regs used by the code generator
        LOADHIGH(G(gc_entry_regs))
        ldo     LOW(G(gc_entry_regs))(%r1), %r1
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
        LOADHIGH(G(gc_entry_float_regs))
        ldo     LOW(G(gc_entry_float_regs))(%r1), %r1
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
        CALL(G(garbage_collection))
        ldo     -64(%r30), %r30

; Restore all regs used by the code generator
        LOADHIGH(G(gc_entry_regs))
        ldo     LOW(G(gc_entry_regs))(%r1), %r1
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
        LOADHIGH(G(gc_entry_float_regs))
        ldo     LOW(G(gc_entry_float_regs))(%r1), %r1
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
        LOADHIGH(G(young_ptr))
        ldw     LOW(G(young_ptr))(%r1), %r3
; Allocate space for block
        LOADHIGH(G(caml_required_size))
        ldw     LOW(G(caml_required_size))(%r1), %r1
        sub     %r3, %r1, %r3
; Return to caller
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r2
        bv      0(%r2)
        nop
        ENDPROC

; Call a C function from Caml
; Function to call is in %r22

	.align	CODE_ALIGN
	EXPORT_CODE(G(caml_c_call))
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
        LOADHIGH(G(young_ptr))
; Call the C function
#ifdef SYS_hpux
        bl      $$dyncall, %r2
        stw     %r3, LOW(G(young_ptr))(%r1)     ; in delay slot
#else
        stw     %r3, LOW(G(young_ptr))(%r1)
        ble     0(4, %r22)
        copy    %r31, %r2
#endif
; Reload return address
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r2
; Reload allocation pointer
        LOADHIGH(G(young_ptr))
; Return to caller
        bv      0(%r2)
        ldw     LOW(G(young_ptr))(%r1), %r3   ; in delay slot
        ENDPROC

; Start the Caml program

	.align	CODE_ALIGN
	EXPORT_CODE(G(caml_start_program))
G(caml_start_program):
        STARTPROC
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
        LOADHIGH(G(caml_top_of_stack))
        stw     %r30, LOW(G(caml_top_of_stack))(%r1)
; Initialize allocation registers
        LOADHIGH(G(young_ptr))
        ldw     LOW(G(young_ptr))(%r1), %r3
        LOADHIGH(G(young_limit))
        ldo     LOW(G(young_limit))(%r1), %r4
; Go for it
	CALL(G(caml_program))
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
        ENDPROC

; Raise an exception from C

	.align	CODE_ALIGN
	EXPORT_CODE(G(raise_caml_exception))
G(raise_caml_exception):
        STARTPROC
; Cut the stack
        LOADHIGH(G(caml_exception_pointer))
        ldw     LOW(G(caml_exception_pointer))(%r1), %r30
; Reload allocation registers
        LOADHIGH(G(young_ptr))
        ldw     LOW(G(young_ptr))(%r1), %r3
        LOADHIGH(G(young_limit))
        ldo     LOW(G(young_limit))(%r1), %r4
; Raise the exception
        ldw     -4(%r30), %r1
        ldw     -8(%r30), %r5
        bv      0(%r1)
        ldo     -8(%r30), %r30  ; in delay slot
        ENDPROC

; Callbacks C -> ML

        .align	CODE_ALIGN
        EXPORT_CODE(G(callback))
G(callback):
        STARTPROC
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
        LOADHIGH(G(caml_bottom_of_stack))
        ldw     LOW(G(caml_bottom_of_stack))(%r1), %r1
        stw     %r1, -8(%r30)
        LOADHIGH(G(caml_last_return_address))
        ldw     LOW(G(caml_last_return_address))(%r1), %r1
        stw     %r1, -4(%r30)
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
        LOADHIGH(G(young_ptr))
        ldw     LOW(G(young_ptr))(%r1), %r3
        LOADHIGH(G(young_limit))
        ldo     LOW(G(young_limit))(%r1), %r4
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
        ldw     -8(%r30), %r31
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r31, LOW(G(caml_bottom_of_stack))(%r1)
        ldw     -4(%r30), %r31
        LOADHIGH(G(caml_last_return_address))
        stw     %r31, LOW(G(caml_last_return_address))(%r1)
        ldo     -8(%r30), %r30
; Save allocation pointer
        LOADHIGH(G(young_ptr))
        stw     %r3, LOW(G(young_ptr))(%r1)
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
        LOADHIGH(G(caml_bottom_of_stack))
        stw     %r31, LOW(G(caml_bottom_of_stack))(%r1)
        ldw     -4(%r30), %r31
        LOADHIGH(G(caml_last_return_address))
        stw     %r31, LOW(G(caml_last_return_address))(%r1)
        ldo     -8(%r30), %r30
; Save allocation pointer and exception pointer
        LOADHIGH(G(young_ptr))
        stw     %r3, LOW(G(young_ptr))(%r1)
        LOADHIGH(G(caml_exception_pointer))
        stw     %r5, LOW(G(caml_exception_pointer))(%r1)
; Re-raise the exception through mlraise, to clean up local C roots
        ldo     64(%r30), %r30
	CALL(G(mlraise))        ; never returns
        ENDPROC

        .align	CODE_ALIGN
        EXPORT_CODE(G(callback2))
G(callback2):
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
        EXPORT_CODE(G(callback3))
G(callback3):
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

        .data
        EXPORT_DATA(G(system_frametable))
G(system_frametable):
        .word   1               /* one descriptor */
        .word   L104 + 3        /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
