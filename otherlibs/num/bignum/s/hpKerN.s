; Copyright     Digital Equipment Corporation & INRIA     1988, 1989
;
;               KerN for the HP 9000 600/700/800 (PA-RISC 1.1 only)
;               LERCIER Reynald (april 1993)
;



	.SPACE   $TEXT, SORT=8
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnSetToZero
	.PROC
	.CALLINFO 
	.ENTRY				; (nn, nl)
	comb,<=	%arg1, %r0, L$BSTZ0     ; if (nl <= 0) goto L$BSTZ0
	nop
L$BSTZ1	addibf,<=	-1, %arg1, L$BSTZ1	; while (nl-->0) 
	stwm	%r0, 4(0, %arg0)	; { *(nn++)=0 }
L$BSTZ0	bv,n	%r0(%r2)		; return
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnAssign
	.PROC
	.CALLINFO
	.ENTRY			; (mm, nn, nl)
	comb,<=	%arg2, %r0, L$BAG0	; if (nl <= 0) goto L$BAG0
	nop
	comb,>>=,n	%arg0, %arg1, L$BAG1	; if (mm>=nn) goto L$BAG1 
L$BAG2	ldwm	4(%arg1), %r19		; X=*(nn++)
	addibf,<=	-1, %arg2, L$BAG2	; if ((nl--)>=0) goto L$BAG2
	stwm	%r19, 4(%arg0)		; *(mm++)=X
	bv,n	%r0(%r2)		; return
L$BAG1	comb,=,n	%arg0, %arg1, L$BAG0	; if (mm==nn) goto L$BAG0
	shd	%arg2, %r0, 30, %r19	; X = nl <<2
	add %arg0, %r19, %arg0		; mm+=X
	add %arg1, %r19, %arg1		; nn+=X
L$BAG3	ldwm	-4(%arg1), %r19		; X=*(--nn)
	addibf,<=	-1, %arg2, L$BAG3	; if (--nl>=0) goto L$BAG3
	stwm	%r19, -4(%arg0)		; *(--mm)=X
L$BAG0	bv,n	%r0(%r2)		; return
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnSetDigit
	.PROC
	.CALLINFO
	.ENTRY				; (nn, d)
	bv	%r0(%r2)		; return	
	.EXIT
	stws	%arg1, 0(0, %arg0)	; *nn = d
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnGetDigit
	.PROC
	.CALLINFO
	.ENTRY				; (nn)
	bv	%r0(%r2)		
	.EXIT
	ldws	0(0, %arg0), %ret0	; return (*nn)
	.PROCEND



	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnNumDigits
	.PROC
	.CALLINFO
	.ENTRY			; (nn, nl)
	comb,<=,n	%arg1, %r0, L$BND0 	; if (nl <= 0) goto L$BND0
	shd	%arg1, %r0, 30, %r19	; X = nl<<2
	add	%arg0, %r19, %arg0	; nn+=nl
	ldwm	-4(%arg0), %r19		; X=*(--nn)
L$BND2	comb,<>	%r19, %r0, L$BND1	; if (X != 0) goto L$BND1
	nop
	addibf,<=	-1, %arg1, L$BND2	; if ((--nl)>0) goto L$BND2
	ldwm	-4(%arg0), %r19		; X=*(--nn)
L$BND0	bv	%r0(%r2)	; return(1)
	ldi	1, %ret0
L$BND1	bv	%r0(%r2)	; return(nl)
	copy	%arg1, %ret0
	.EXIT
	.PROCEND

	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnNumLeadingZeroBitsInDigit
	.PROC
	.CALLINFO
	.ENTRY			; (d)
	ldi	0, %ret0	; p=0
	comb,<>,n	%r0, %arg0, L$BLZ1	; if (d<>0) goto L$BLZ1
	bv	%r0(%r2)	; return(32)
	ldi 32, %ret0
L$BLZ2	addi	1, %ret0, %ret0	; p++
L$BLZ1  comib,<	0, %arg0, L$BLZ2	; if (d>0) goto L$BLZ2; 	
	shd	%arg0, %r0, 31, %arg0	; d<<=1
	bv,n	%r0(%r2)	; return(p)
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnDoesDigitFitInWord
	.PROC
	.CALLINFO
	.ENTRY
	bv	%r0(%r2)	; return
	ldi 1, %ret0
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnIsDigitZero
	.PROC
	.CALLINFO
	.ENTRY			; (d)
	ldi 	1, %ret0
	or,=	%r0, %arg0, %r0	; if (d==0) return(1)
	ldi 	0, %ret0	; return(0)
	bv,n	%r0(%r2)	
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnIsDigitNormalized
	.PROC
	.CALLINFO
	.ENTRY
	bv	%r0(%r2)	; return
	extru	%arg0, 0, 1, %ret0 ; the leftmost bit
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnIsDigitOdd
	.PROC
	.CALLINFO
	.ENTRY
	bv	%r0(%r2)	; return	
	extru	%arg0, 31, 1, %ret0 ; the rightmost bit
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnCompareDigits
	.PROC
	.CALLINFO
	.ENTRY				; (d1, d2)
	comb,=	%arg0, %arg1, L$BCD0	; if (d1==d2) goto L$BCD0
	ldi	0, %ret0		; return(0)
	comb,>>	%arg0, %arg1, L$BCD0	; if (d1>d2) goto L$BCD0
	ldi	1, %ret0		; return(1)
	ldi	-1, %ret0		; return(-1)
L$BCD0	bv,n	%r0(%r2)	
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnComplement
	.PROC
	.CALLINFO
	.ENTRY				; (nn, nl)
	comb,<=,n	%arg1, %r0, L$BCM0	; if (nl <= 0) goto L$BCM0
	ldi	-1, %ret0		; cste=-1
L$BCM1	ldw	(%arg0), %r19		; X=*(nn)
	xor	%r19, %ret0, %r19		; X ^= cste
	addibf,<=	-1, %arg1, L$BCM1	; if ((--nl)>=0) goto L$BCM1
	stwm	%r19, 4(%arg0)		; *(nn++)=X
L$BCM0	bv,n	%r0(%r2)	; return
	.EXIT
	.PROCEND



	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnAndDigits
	.PROC
	.CALLINFO
	.ENTRY			; (nn, d)
	ldw	(%arg0), %r19	; X=*nn
	and	%r19, %arg1, %r19	; X &= d
	stw	%r19, (%arg0)	; *nn=X
	bv,n	%r0(%r2)	; return	
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnOrDigits
	.PROC
	.CALLINFO
	.ENTRY		; (nn, d)
	ldw	(%arg0), %r19	; X=*nn
	or	%r19, %arg1, %r19	; X &= d
	stw	%r19, (%arg0)	; *nn=X
	bv,n	%r0(%r2)	; return	
	.EXIT
	.PROCEND



	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnXorDigits
	.PROC
	.CALLINFO
	.ENTRY		; (nn, d)
	ldw	(%arg0), %r19	; X=*nn
	xor	%r19, %arg1, %r19	; X &= d
	stw	%r19, (%arg0)	; *nn=X
	bv,n	%r0(%r2)	; return	
	.EXIT
	.PROCEND


; convention for BnnShiftLeft, BnnShiftRight
nn1	.REG	%arg0
nl1	.REG	%arg1
nbits	.REG	%arg2
res	.REG	%ret0
X	.REG	%r19
Y	.REG	%r20
Z	.REG	%r21
W	.REG	%r22

	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnShiftLeft
	.PROC
	.CALLINFO
	.ENTRY			; (nn1, nl1, nbits)
	ldi	0, res	; res=0
	comb,=	nbits, %r0, L$BSL0     ; if (nbits = 0) goto L$BSL0
	nop
	comb,<=	nl1, %r0, L$BSL0     ; if (nl1 <= 0) goto L$BSL0
	nop
	subi	32, nbits, nbits	; nbits-=32
	mtsar	nbits
L$BSL1	ldw	(nn1), X		; X=*(nn1)
	vshd	X, %r0, Y	; Y= X<<nbits
	or	Y, res, Y	; Y|=res
	vshd	%r0, X, res	; res= X>>nbits
	addibf,<=	-1, nl1, L$BSL1	; if ((nl1--)>=0) goto L$BSL1
	stwm	Y, 4(nn1)	; *(nn1++)=Y
L$BSL0	bv,n	%r0(%r2)	; return
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnShiftRight
	.PROC
	.CALLINFO
	.ENTRY		; (nn1, nl1, nbits)
	ldi	0, res	; res=0
	comb,=	nbits, %r0, L$BSR0     ; if (nbits = 0) goto L$BSR0
	nop
	comb,<=,n	nl1, %r0, L$BSR0     ; if (nl1 <= 0) goto L$BSR0
	mtsar nbits
	shd	nl1, %r0, 30, Y		; Y=nl1<<2
	add	Y, nn1, nn1		; nn1+=Y
L$BSR1	ldwm	-4(nn1), X		; X=*(--nn1)
	vshd	%r0, X, Y	; Y= X>>nbits
	or	Y, res, Y	; Y|=res
	vshd	X, %r0, res	; res= X<<rnbits
	addibf,<=	-1, nl1, L$BSR1	; if ((nl1--)>=0) goto L$BSR1
	stw	Y, (nn1)	; *(nn1)=Y
L$BSR0	bv,n	%r0(%r2)	; return
	.EXIT
	.PROCEND

; convention for BnnAddCarry, BnnSubtractBorrow
carryin	.REG	%arg2

	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnAddCarry
	.PROC
	.CALLINFO
	.ENTRY			; (nn1, nl1, carryin)
	comb,=	carryin, %r0, L$BAC0	; if (carryin == 0) goto L$BAC0
	nop
	comb,<=,n	nl1, %r0, L$BAC1	; if (nl1<= 0) goto L$BAC1
	ldw	(nn1), X		; X=*(nn1)
L$BAC2	addi,UV	1, X, X		; X++
	b	L$BAC0		; if (X<2^32) goto L$BAC0
	stwm	X, 4(nn1)	; *(nn1++)=X
	addibf,<=,n	-1, nl1, L$BAC2	; if ((nl1--)>=0) goto L$BAC2
	ldw	(nn1), X		; X=*(nn1)
L$BAC1	bv	%r0(%r2)	; return(1)	
	ldi	1, res
L$BAC0	bv	%r0(%r2)	; return(0)	
	ldi	0, res
	.EXIT
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnSubtractBorrow
	.PROC
	.CALLINFO
	.ENTRY	; (nn1, nl1, d)
	comib,=	1, carryin, L$BSB1	; if (carryin == 1) goto L$BSB1
	nop
	comb,<=,n	nl1, %r0, L$BSB0	; if (nl1<= 0) goto L$BSB0
	ldw	(nn1), X		; X=*(nn1)
L$BSB2	addi,nuv	-1, X, X	; X--
	b	L$BSB1		; if (X!=-1) goto L$BSB1
	stwm	X, 4(nn1)	; *(nn1++)=X
	addibf,<=,n	-1, nl1, L$BSB2	; if ((nl1--)>=0) goto L$BSB2
	ldw	(nn1), X		; X=*(nn1)
L$BSB0	bv	%r0(%r2)	; return(0)
	ldi	0, res
L$BSB1	bv	%r0(%r2)	; return(1)	
	ldi	1, res
	.EXIT
	.PROCEND

; convention for BnnAdd, BnnSubtract
mm2	.REG	%arg0
ml2	.REG	%arg1
nn2	.REG	%arg2
nl2	.REG	%arg3

	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnAdd
	.PROC
	.CALLINFO
	.ENTER			; (mm2, ml2, nn2, nl2, carryin)
	sub	ml2, nl2, ml2	; ml2 -= nl2
	ldw     -52(0, %r30), res	; res=carryin
	comb,=,n	nl2, %r0, L$BADD2	; if (nl2==0) goto L$BADD2
L$BADD1	ldwm	4(nn2), X	; X = *(nn2++)
	ldw	(mm2), Y		; Y = *(mm2)
	copy	res, Z		; Z=res
	ldi	0, res		; res=0
	add,nuv	Y, Z, Y		; Y+=Z;
	ldi	1, res		; if (Y>=2^32) res=1 Y-=2^32
	add,nuv	Y, X, Y		; Y+=X
	ldi	1, res		; if (Y>=2^32) res=1 Y-=2^32
	addibf,<=	-1, nl2, L$BADD1	; if ((nl2--)>=0) goto L$BADD1
	stwm	Y, 4(mm2)	; *(mm2++)=Y
L$BADD2	comclr,<>	res, %r0, %r0	; if (res<>0) skip next operation
	b,n	L$BADD4		; return(0)
	comclr,<>	ml2, %r0, %r0	; if (ml2<>0) skip next operation
	b	L$BADD5		; return(1)
	ldw	(mm2), X		; X=*mm2
L$BADD3	addi,uv	1, X, X		; X++
	b	 L$BADD4	; if (X<2^32) goto L$BADD4
	stwm	X, 4(mm2)	; *(mm2++)=X
	addibf,<=	-1, ml2, L$BADD3	; if ((ml2--)>=0) goto L$BADD3
	ldw	(mm2), X		; X=*mm2
	b,n	L$BADD5		; return(1)
L$BADD4	ldi	0, res
L$BADD5	.LEAVE
	.PROCEND


	.SPACE   $TEXT
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnSubtract
	.PROC
	.CALLINFO
	.ENTRY			; (mm2, ml2, nn2, nl2, carryin)
	sub	ml2, nl2, ml2	; ml2 -= nl2
	ldw     -52(0, %r30), res	; res=carryin
	subi	1, res, res		; res=1-res
	comb,=,n	nl2, %r0, L$BS2	; if (nl2==0) goto L$BS2
L$BS1	ldwm	4(nn2), X	; X = *(nn2++)
	ldw	(mm2), Y		; Y = *(mm2)
	copy	res, Z		; Z=res
	ldi	0, res		; res=0
	sub,>>=	Y, Z, Y		; Y-=Z;
	ldi	1, res		; if (Y<=0) res=1 Y+=2^32
	sub,>>=	Y, X, Y		; Y-=X
	ldi	1, res		; if (Y<=0) res=1 Y+=2^32
	addibf,<=	-1, nl2, L$BS1	; if ((nl2--)>=0) goto L$BS1
	stwm	Y, 4(mm2)	; *(mm2++)=Y
L$BS2	comb,=	res, %r0, L$BS4	; if (res==0) goto L$BS4
	nop
	comb,=,n	ml2, %r0, L$BS5	; if (ml2==0) goto L$BS5
	ldw	(mm2), X		; X=*mm2
L$BS3	addi,nuv	-1, X, X	; X--
	b	 L$BS4	; if (X!=-1) goto L$BS4
	stwm	X, 4(mm2)	; *(mm2++)=X
	addibf,<=,n	-1, ml2, L$BS3	; if ((ml2--)>=0) goto L$BS3
	ldw	(mm2), X		; X=*mm2
L$BS5	bv	%r0(%r2)	; return(0)
	ldi 	0, res
L$BS4	bv	%r0(%r2)	; return(1)
	ldi	1,res
	
	.EXIT
	.PROCEND


; conventions for BnnMultiplyDigit
pp	.REG	%arg0
pl1	.REG	%arg1
mm	.REG	%arg2
ml	.REG	%arg3
X1	.REG	%r22
X3	.REG	%r1
dm	.REG	%r29
fLd	.REG	%fr5L
fHd	.REG	%fr5R
fLm	.REG	%fr7L
fHm	.REG	%fr8L

	
        .SPACE  $TEXT$
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnMultiplyDigit
	.PROC
	.CALLINFO CALLER, FRAME=8, SAVE_RP
	.ENTER		; (pp, pl1, mm, ml, dm) 

	ldw     -108(0, %r30), dm   	; dm
	comb,=	dm, %r0, L$BMD7	; if (dm==0) goto L$BMD7
	nop
	comib,<>,n	1, dm, L$BMD2	; if (dm<>1) goto L$BMD2
	.CALL   ARGW0=GR,ARGW1=GR,ARGW2=GR,RTNVAL=GR    ;in=24,25,26;out=28;
	bl      BnnAdd, %r2 		; return(BnnAdd(pp, pl1, mm, ml, 0))
	stw	%r0, -52(0, %r30)
	b,n	 L$BMD8

L$BMD2	comb,=	ml, %r0, L$BMD7		; if (ml==0) goto L$BMD7
	nop
	sub	pl1, ml, pl1		; pl1-=ml

	ldo	-52(%r30), %r21
	extru	dm, 31, 16, X		; Ld=dm & (2^16-1);
	stws	X, 0(0, %r21)
	fldws	0(0, %r21), fLd
	extru	dm, 15, 16, X		; Hd=dm>>16;
	stws	X, 0(0, %r21)
	fldws	0(0, %r21), fHd
	ldi	0, dm			; dm=0
	
L$BMD3	ldwm	4(mm), X1		; X1=*(mm++)
	extru	X1, 31, 16, X		; Lm=X1 & (2^16-1)
	stws	X, 0(0, %r21)
	fldws	0(0, %r21), fLm
	extru	X1, 15, 16, X		; Hm=X1>>16
	stws	X, 0(0, %r21)			
	fldws   0(0, %r21), fHm	
	
	xmpyu	fHm, fHd, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), X3		

	xmpyu	fLm, fHd, %fr4
	fstws	%fr4R, -4(0, %r21)	

	xmpyu	fHm, fLd, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), X1		

	xmpyu	fLm, fLd, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), X	

	add,nuv	X, dm, dm		
	ldo	1(X3), X3	
	ldws	-4(0, %r21), X	
	add,nuv	X1, X, X1		; X1+=X
	addil	L%(65536), X3		; if overflow   X3+=2^16;
	extru	X1, 15, 16, X		; X = X1 >> 16
	add	X3, X, X3		; X3+=X
	zdep	X1, 15, 16, X1		; X1 =<< 16
	add,nuv	dm, X1, dm		; dm+=X1
	ldo	1(X3), X3		; if overflow	X3++;
	ldws	(pp), X			; X=*(pp)
	add,nuv	X, dm, dm		; dm+=X;
	ldo	1(X3), X3		; if overflow	X3++;
	stwm	dm, 4(pp)		; *(pp++)=dm
	addib,>,n	-1, ml, L$BMD3	; if ((--ml)>0) goto L$BMD3
	copy	X3, dm			; dm=X3
	
	ldo	-1(pl1), pl1		; pl1--
	ldi	0, dm			; dm=0
	ldw	(pp), X			; X=*pp
	add,nuv	X, X3, X		; X+= X3
	ldi	1, dm			; if overflow dm=1;
	comb,=	dm, %r0, L$BMD7		; if (dm==0) goto L$BMD7
	stwm	X, 4(pp)		; *(pp++)=X
	comb,=,n	pl1, %r0, L$BMD9		; if (pl1==0) goto L$BMD9
	ldw	(pp), X
L$BMD4	addi,uv	1, X, X		; X++
	b	L$BMD7			; if no overflow goto L$BMD7
	stwm	X, 4(pp)		; *(pp++)=X
	addib,>,n	-1, pl1, L$BMD4	; if ((--pl1)>0) goto L$BMD4
	ldw	(pp), X			; X=*(pp)
L$BMD9	b	L$BMD8			; return(1)
	ldi	1, res
L$BMD7	ldi	0, res			; return(0)
L$BMD8	.LEAVE
	.PROCEND

; conventions for BnnDivideDigit
qq	.REG	%r3
nn	.REG	%r4
nl	.REG	%r5
dd	.REG	%r6
ch	.REG	%r7
cl	.REG	%r8
k	.REG	%r9
f_qq	.REG	%r10
o_nl	.REG	%r11
rh	.REG	%r12
rl	.REG	%r13
ph	.REG	%r14
pl	.REG	%r15
qa	.REG	%r16
fcl	.REG	%fr5L
fch	.REG	%fr6L
fqa	.REG	%fr7L


	
        .SPACE  $TEXT$
        .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=44,CODE_ONLY,SORT=24
BnnDivideDigit
	.PROC
	.CALLINFO CALLER, FRAME=0, ENTRY_GR=16, SAVE_RP
	.ENTER				; (qq, nn, nl, dd)
	copy	%arg0, qq		; qq=%arg0
	copy	%arg1, nn		; nn=%arg1
	copy	%arg2, nl		; nl=%arg2
	copy	%arg3, dd		; dd=%arg3
	.CALL	;in=%arg0 ;out=%ret0	; res=BnnNumLeadingZeroBitsInDigit(dd)
	bl	BnnNumLeadingZeroBitsInDigit, %r2	
	copy	dd, %arg0	
	comib,=	0, res, L$BDD1		; k=res; if (k==0) goto L$BDD1
	copy	res, k		
	ldw	(qq), f_qq		; f_qq=*qq
	copy	nl, o_nl		; o_nl=nl
	subi	32, k, X
	mtsar	X
	vshd	dd, %r0, dd		; dd<<=k
	copy	nn, %arg0
	copy	nl, %arg1
	.CALL	;in=%arg0, %arg1, %arg2 ; out=%ret0
	bl	BnnShiftLeft, %r2	; BnnShiftLeft(nn, nl, k)
	copy	k, %arg2
	
L$BDD1	shd	nl, %r0, 30, X		; X=nl<<2
	add	nn, X, nn		; nn+=nl
	addi	-1, nl, nl		; nl--
	shd	nl, %r0, 30, X		; X=nl<<2
	add	qq, X, qq		; qq+=nl
	extru	dd, 15, 16, ch		; ch=dd>>16 
	extru	dd, 31, 16, cl		; cl=dd & (2^16-1)
	ldo	-48(%r30), %r21
	stws	cl, 0(0, %r21)
	fldws	0(0, %r21), fcl
	stws	ch, 0(0, %r21)
	fldws	0(0, %r21), fch
	comib,=	0, nl, L$BDD3	; if (nl==0) goto L$BDD3
	ldwm	-4(nn), rl		; rl=*(--nn)

L$BDD2	copy	rl, rh			; rh=rl
	ldwm	-4(nn), rl		; rl=*(--nn)

	copy	rh, %arg0
	.CALL	;in=25,26;out=29; (MILLICALL)
        bl 	$$divU,%r31 		; %r29=rh/ch
	copy	ch, %arg1
	copy	%r29, qa			; qa=%r29

	stws	qa, 0(0, %r21)
	fldws	0(0, %r21), fqa
	xmpyu	fcl, fqa, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), pl
	xmpyu	fch, fqa, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), %r29

	shd	%r0, pl, 16, X		; X=pl>>16 
	add	%r29, X, ph		; ph=X+%r29
	comb,>>	ph, rh, L$BDD84	; if (ph>rh) goto L$BDD84
	shd	pl, %r0, 16, pl		; pl<<=16 
	comb,<>	ph, rh, L$BDD88	; if (ph!=rh) goto L$BDD88
	nop
	comb,<<=,n	pl, rl, L$BDD88	; if (pl<=rl) goto L$BDD88
L$BDD84	shd	cl, %r0, 16, X		; X = cl << 16
L$BDD85	comb,<<=	X, pl, L$BDD86		; if (X<=pl) goto L$BDD86 
	addi	-1, qa, qa		; qa--
	addi	-1, ph, ph		; ph--
L$BDD86	sub	pl, X, pl		; pl-=X
	sub	ph, ch, ph		; ph-=ch
	comb,>>	ph, rh, L$BDD85	; if (ph>rh) goto L$BDD85
	nop
	comb,<>	ph, rh, L$BDD88 ; if (ph!=rh) goto L$BDD88
	nop
	comb,>>	pl, rl, L$BDD85 ; if (pl>rl) goto L$BDD85
	nop
L$BDD88	comb,<<=,n	pl, rl, L$BDD89 ; if (pl<=rl) goto L$BDD89
	addi	-1, rh, rh		; rh--
L$BDD89	sub	rl, pl, rl		; rl-=pl
	sub	rh, ph, rh		; rh-=ph
	shd	qa, %r0, 16, X		; X=qa<<16
	stwm	X, -4(qq)		; *(--qq)=X
	shd	rh, %r0, 16, X		; X=rh<<16
	shd	%r0, rl, 16, qa		; qa=rl>>16
	or	qa, X, qa		; qa |=X
	copy	qa, %arg0
	.CALL	;in=25,26;out=29; (MILLICALL)
        bl 	$$divU,%r31 		; %r29=qa/ch
	copy	ch, %arg1
	copy	%r29, qa			; qa=%r29

	stws	qa, 0(0, %r21)
	fldws	0(0, %r21), fqa
	xmpyu	fcl, fqa, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), pl
	xmpyu	fch, fqa, %fr4
	fstws	%fr4R, 0(0, %r21)
	ldws	0(0, %r21), %r29

	shd	%r0, pl, 16, X		; X=pl>>16 
	add	%r29, X, ph		; ph+=X
	extru	pl, 31, 16, pl		; pl &= (2^16-1)
	shd	ph, %r0, 16, X		; X = ph<<16
	shd	%r0, ph, 16, ph		; ph >>=16
	comb,>>	ph, rh, L$BDD41		; if (ph>rh) goto L$BDD41
	or	X, pl, pl		; pl |= X
	comb,<>	ph, rh, L$BDD44	; if (ph!=rh) goto L$BDD44
	nop
	comb,<<=	pl, rl, L$BDD44	; if (pl<=rl) goto L$BDD44
	nop
L$BDD41	comb,<<=	dd, pl, L$BDD42		; if (dd<=pl) goto L$BDD42 
	addi	-1, qa, qa		; qa--
	addi	-1, ph, ph		; ph--
L$BDD42	comb,>>	ph, rh, L$BDD41	; if (ph>rh) goto L$BDD4
	sub	pl, dd, pl		; pl-=dd
	comb,<>	ph, rh, L$BDD44 ; if (ph!=rh) goto L$BDD44
	nop
	comb,>>,n	pl, rl, L$BDD41 ; if (pl>rl) goto L$BDD41
	nop
L$BDD44	sub	rl, pl, rl		; rl-=pl
	ldw	(qq), X			; X=*qq
	or	X, qa, X		; X |= qa
	addib,>	-1, nl, L$BDD2		; if ((--nl)>0) goto L$BDD2
	stws	X, (qq)			; *qq=X
	

L$BDD3	comib,=	0, k, L$BDD5	; if (k==0) goto L$BDD5
	nop
	comb,<<,n	qq, nn, L$BDD31	; if (qq<nn) goto L$BDD31
	shd	o_nl, %r0, 30, Y
	add	nn, Y, X		; X=nn+o_nl
	comb,<<=	X, qq, L$BDD31	; if (X<=qq) goto L$BDD31
	nop
	sub	qq, nn, o_nl		; o_nl=qq-nn
	shd	%r0, o_nl, 2, o_nl	; o_nl>>=2
	ldw	(qq), W			; W=*qq
	stws	f_qq, (qq)		; *qq=f_qq
	copy	nn, %arg0
	addi	1, o_nl, %arg1		; %arg1=o_nl+1
	.CALL	;in=%arg0, %arg1, %arg2 ;out=%ret0
	bl	BnnShiftRight, %r2	; BnnShiftRight(nn, o_nl, k)
	copy	k, %arg2
	b	L$BDD5
	stws	W, (qq)			; *qq=W
L$BDD31	comb,<>,n	qq, nn, L$BDD32	; if (qq<>nn) goto L$BDD32
	addi	-1, o_nl, o_nl		; o_nl--
	shd	o_nl, %r0, 30, o_nl	; o_nl<<=2
	add	nn, o_nl, nn		; nn+=o_nl
	ldi	1, o_nl
L$BDD32	copy	nn, %arg0
	copy	o_nl, %arg1
	.CALL	;in=%arg0, %arg1, %arg2 ;out=%ret0
	bl	BnnShiftRight, %r2	; BnnShiftRight(nn, o_nl, k)
	copy	k, %arg2

L$BDD5  mtsar	k
	vshd	%r0, rl, res 		; return(rl>>k)
L$BDD6	.LEAVE
	.PROCEND


        .SPACE  $TEXT$
        .SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44,SORT=16
$THIS_LIT$

        .SUBSPA $LITSTATIC$,QUAD=0,ALIGN=8,ACCESS=44,SORT=16
$THIS_LITSTATIC$

        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SUBSPA $CODE$
        .SPACE  $PRIVATE$,SORT=16
        .SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31,SORT=16
$THIS_DATA$

        .SUBSPA $SHORTDATA$,QUAD=1,ALIGN=8,ACCESS=31,SORT=16
$THIS_SHORTDATA$

        .SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
$THIS_BSS$

        .SUBSPA $SHORTBSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=80
$THIS_SHORTBSS$

        .SUBSPA $STATICDATA$,QUAD=1,ALIGN=8,ACCESS=31,SORT=16
$THIS_STATICDATA$
        .ALIGN  4
        .STRINGZ "@(#)KerN.c: copyright Digital Equipment Corporation & INRIA 1988, 1989\n"
        .SUBSPA $SHORTSTATICDATA$,QUAD=1,ALIGN=8,ACCESS=31,SORT=24
$THIS_SHORTSTATICDATA$

        .SPACE  $TEXT$
        .SUBSPA $CODE$
        .EXPORT BnnSetToZero,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .IMPORT bzero,CODE
        .SUBSPA $CODE$
        .EXPORT BnnAssign,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR
        .IMPORT bcopy,CODE
        .SUBSPA $CODE$
        .EXPORT BnnSetDigit,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .SUBSPA $CODE$
        .EXPORT BnnGetDigit,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnNumDigits,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnNumLeadingZeroBitsInDigit,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnDoesDigitFitInWord,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnIsDigitZero,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnIsDigitNormalized,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnIsDigitOdd,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnCompareDigits,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnComplement,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .SUBSPA $CODE$
        .EXPORT BnnAndDigits,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .SUBSPA $CODE$
        .EXPORT BnnOrDigits,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .SUBSPA $CODE$
        .EXPORT BnnXorDigits,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
        .SUBSPA $CODE$
        .EXPORT BnnShiftLeft,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnShiftRight,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnAddCarry,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnAdd,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnSubtractBorrow,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnSubtract,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
        .SUBSPA $CODE$
        .EXPORT BnnMultiplyDigit,ENTRY,PRIV_LEV=0,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
        .IMPORT $$mulU,MILLICODE
        .SUBSPA $CODE$
        .EXPORT BnnDivideDigit,ENTRY,PRIV_LEV=0,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
        .IMPORT $$divU,MILLICODE
        .IMPORT $$remU,MILLICODE
        .END
