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

        .text
        .align  2
	.globl _call_gen_code
_call_gen_code:
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

; Shuffle the arguments and call
        copy    %r26, %r22
        copy    %r25, %r26
        copy    %r24, %r25
        copy    %r23, %r24
        fcpy,dbl %fr5, %fr4
        ble     0(4, %r22)
        copy    %r31, %r2
; Shuffle the results
        copy    %r26, %r28
; Restore the callee-save registers
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

        ldo	-256(%r30), %r30
	ldw     -20(%r30), %r2
        bv      0(%r2)
        nop

	.align	2
	.globl _caml_c_call
_caml_c_call:
        bv	0(%r22)
        nop
