/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* */
/*		KerN for the 68020 : SONY syntax */
/*		[Bepaul] */
/* */
		.text

		.globl	_BnnSetToZero
_BnnSetToZero:
		.set	BSTZnn,4
		.set	BSTZnl,8
		move.l	BSTZnn(sp),a0
		move.l	BSTZnl(sp),d0
		dbf	d0,BSTZ1		/* if(nl--) goto BSTZ1; */
		rts				/* return; */
BSTZ1:		clr.l	(a0)+			/* *(nn++) = 0; */
		dbf	d0,BSTZ1		/* if(nl--) goto BSTZ1; */
		rts				/* return; */

		.globl	_BnnAssign
_BnnAssign:
		.set	BAGmm,4
		.set	BAGnn,8
		.set	BAGnl,12
		move.l	BAGmm(sp),a0
		move.l	BAGnn(sp),a1
		move.l	BAGnl(sp),d0
		cmp.l	a1,a0
		bcc	BAG2			/* if(mm >= nn) goto BAG2; */
		dbf	d0,BAG1			/* if(nl--) goto BAG1; */
		rts				/* return; */
BAG1:		move.l	(a1)+,(a0)+		/* *(mm++) = *(nn++); */
		dbf	d0,BAG1			/* if(nl--) goto BAG1; */
		rts				/* return; */
BAG2:		bls	BAG4			/* if(mm <= nn) goto BAG4; */
		lea	(0,a0,d0.l*4),a0	/* mm = &mm[nl]; */
		lea	(0,a1,d0.l*4),a1	/* nn = &nn[nl]; */
		dbf	d0,BAG3			/* if(nl--) goto BAG3; */
		rts				/* return; */
BAG3:		move.l	-(a1),-(a0)		/* *(--mm) = *(--nn); */
		dbf	d0,BAG3			/* if(nl--) goto BAG3; */
BAG4:		rts				/* return; */

		.globl	_BnnSetDigit
_BnnSetDigit:
		.set	BSDnn,4
		.set	BSDd,8
		move.l	BSDnn(sp),a0
		move.l	BSDd(sp),(a0)		/* *nn = d; */
		rts				/* return; */

		.globl	_BnnGetDigit
_BnnGetDigit:
		.set	BGDnn,4
		move.l	BGDnn(sp),a0
		move.l	(a0),d0			/* return(*nn); */
		rts

		.globl	_BnnNumDigits
_BnnNumDigits:
		.set	BNDnn,4
		.set	BNDnl,8
		move.l	BNDnn(sp),a0
		move.l	BNDnl(sp),d0
		lea	(0,a0,d0.l*4),a0	/* nn = &nn[nl]; */
		dbf	d0,BND1			/* if(nl--) goto BND1; */
		moveq	#1,d0
		rts				/* return(1); */
BND1:		tst.l	-(a0)
		bne	BND3			/* if(*(--nn) != 0) goto BND3; */
		dbf	d0,BND1			/* if(nl--) goto BND1; */
		moveq	#1,d0
		rts				/* return(1); */
BND3:		addq.l	#1,d0
		rts				/* return(nl + 1); */

		.globl	_BnnNumLeadingZeroBitsInDigit
_BnnNumLeadingZeroBitsInDigit:
		.set	BLZd,4
		bfffo	BLZd(sp){0:32},d0
		rts

		.globl	_BnnDoesDigitFitInWord
_BnnDoesDigitFitInWord:
		.set	BDFd,4
		moveq	#1,d0			/* C_VERSION */
		rts

		.globl	_BnnIsDigitZero
_BnnIsDigitZero:
		.set	BDZd,4
		clr.l	d0
		tst.l	BDZd(sp)
		seq	d0
		rts				/* return(d == 0); */

		.globl	_BnnIsDigitNormalized
_BnnIsDigitNormalized:
		.set	BDNd,4
		clr.l	d0
		tst.w	BDNd(sp)
		smi	d0
		rts				/* return(d < 0); */

		.globl	_BnnIsDigitOdd
_BnnIsDigitOdd:
		.set	BDOd,4
		clr.l	d0
		move.w	BDOd+2(sp),ccr
		scs	d0
		rts				/* return(d & 1); */

		.globl	_BnnCompareDigits
_BnnCompareDigits:
		.set	BCDd1,4
		.set	BCDd2,8
		move.l	BCDd1(sp),d1
		cmp.l	BCDd2(sp),d1
		bhi	BCDsup			/* if(d1 > d2) goto BCDsup; */
		sne	d0
		extb.l	d0
		rts				/* return(-(d1 < d2)); */
BCDsup:		moveq	#1,d0
		rts				/* return(1); */

		.globl	_BnnComplement
_BnnComplement:
		.set	BCMnn,4
		.set	BCMnl,8
		move.l	BCMnn(sp),a0
		move.l	BCMnl(sp),d0
		dbf	d0,BCM1			/* if(nl--) goto BCM1; */
		rts				/* return; */
BCM1:		not.l	(a0)+			/* *(nn++) ^= -1; */
		dbf	d0,BCM1			/* if(nl--) goto BCM1; */
		rts				/* return; */

		.globl	_BnnAndDigits
_BnnAndDigits:
		.set	BADnn,4
		.set	BADd,8
		move.l	BADnn(sp),a0
		move.l	BADd(sp),d0
		and.l	d0,(a0)			/* *n &= d; */
		rts				/* return; */

		.globl	_BnnOrDigits
_BnnOrDigits:
		.set	BODnn,4
		.set	BODd,8
		move.l	BODnn(sp),a0
		move.l	BODd(sp),d0
		or.l	d0,(a0)			/* *n |= d; */
		rts				/* return; */

		.globl	_BnnXorDigits
_BnnXorDigits:
		.set	BXDnn,4
		.set	BXDd,8
		move.l	BXDnn(sp),a0
		move.l	BXDd(sp),d0
		eor.l	d0,(a0)			/* *n ^= d; */
		rts				/* return; */

		.globl	_BnnShiftLeft
_BnnShiftLeft:
		.set	BSLmm,4
		.set	BSLml,8
		.set	BSLnbi,12
		clr.l	d0			/* res = 0; */
		move.l	BSLnbi(sp),d1
		bne	BSL0			/* if(nbi) goto BSL0; */
		rts				/* return(res); */
BSL0:		move.l	BSLmm(sp),a0
		movem.l	#0x3C00,-(sp)		/* Save 4 registers */
		move.l	BSLml + 16(sp),d2
		moveq	#32,d3			/* rnbi = BN_DIGIT_SIZE; */
		sub.l	d1,d3			/* rnbi -= nbi; */
		dbf	d2,BSL1			/* if(ml--) goto BSL1; */
		movem.l	(a7)+,#0x003C		/* Restore 4 registers */
		rts				/* return(res); */
BSL1:		move.l	(a0),d4			/* save = *mm; */
		move.l	d4,d5			/* X = save; */
		lsl.l	d1,d5			/* X <<= nbi; */
		or.l	d0,d5			/* X |= res; */
		move.l	d5,(a0)+			/* *(mm++) = X; */
		move.l	d4,d0			/* res = save; */
		lsr.l	d3,d0			/* res >>= rnbi; */
		dbf	d2,BSL1			/* if(ml--) goto BSL1; */
		movem.l	(a7)+,#0x003C		/* Restore 4 registers */
		rts				/* return(res); */

		.globl	_BnnShiftRight
_BnnShiftRight:
		.set	BSRmm,4
		.set	BSRml,8
		.set	BSRnbi,12
		clr.l	d0			/* res = 0; */
		move.l	BSRnbi(sp),d1
		bne	BSR0			/* if(nbi) goto BSR0; */
		rts				/* return(res); */
BSR0:		move.l	BSRmm(sp),a0
		movem.l	#0x3C00,-(sp)		/* Save 4 registers */
		move.l	BSRml + 16(sp),d2
		lea	(0,a0,d2.l*4),a0	/* mm = &mm[ml]; */
		moveq	#32,d3			/* lnbi = BN_DIGIT_SIZE; */
		sub.l	d1,d3			/* lnbi -= nbi; */
		dbf	d2,BSR1			/* if(ml--) goto BSR1; */
		movem.l	(a7)+,#0x003C		/* Restore 4 registers */
		rts				/* return(res); */
BSR1:		move.l	-(a0),d4			/* save = *(--mm); */
		move.l	d4,d5			/* X = save; */
		lsr.l	d1,d5			/* X >>= nbi; */
		or.l	d0,d5			/* X |= res; */
		move.l	d5,(a0)			/* *mm = X; */
		move.l	d4,d0			/* res = save; */
		lsl.l	d3,d0			/* res <<= lnbi; */
BSR2:		dbf	d2,BSR1			/* if(ml--) goto BSR1; */
		movem.l	(a7)+,#0x003C		/* Restore 4 registers */
		rts				/* return(res); */

		.globl	_BnnAddCarry
_BnnAddCarry:
		.set	BACnn,4
		.set	BACnl,8
		.set	BACcar,12
		move.l	BACcar(sp),d0		/* */
		beq	BAC2			/* if(car == 0) return(car); */
		move.l	BACnl(sp),d0		/* */
		beq	BAC3			/* if(nl == 0) return(1); */
		move.l	BACnn(sp),a0
		subq.l	#1,d0			/* nl--; */
BAC1:		addq.l	#1,(a0)+			/* ++(*nn++); */
		dbcc	d0,BAC1			/* if(Carry || nl--) goto BAC1 */
		scs	d0
		neg.b	d0
		extb.l	d0
BAC2:		rts				/* return(Carry) */
BAC3:		moveq	#1,d0
		rts				/* return(1); */
		
		.globl	_BnnAdd
_BnnAdd:
		.set	BADDmm,4
		.set	BADDml,8
		.set	BADDnn,12
		.set	BADDnl,16
		.set	BADDcar,20
		move.l	BADDmm(sp),a0
		move.l	BADDnn(sp),a1
		move.l	BADDnl(sp),d1
		sub.l	d1,BADDml(sp)		/* ml -= nl; */
		tst.l	d1
		bne	BADD1			/* if(nl) goto BADD1 */
		tst.l	BADDcar(sp)		/*| */
		bne	BADD7			/* if(car) goto BADD7 */
		clr.l	d0
		rts				/* return(0); */
BADD1:		subq.l	#1,d1			/* nl--; */
		move.l	BADDcar(sp),d0
		neg.b	d0			/* Bit No 4 */
		move.w	d0,ccr			/* X = car; */
		move.l	d2,-(sp)			/*|| Save register. */
BADDX:		move.l	(a1)+,d0
		move.l	(a0),d2
		addx.l	d0,d2			/* N = *mm + *(nn++) + X */
		move.l	d2,(a0)+			/* X = N >> 32; *(mn++) = N; */
		dbf	d1,BADDX		/* if(nl--) goto BADDX */
		move.l	(sp)+,d2			/*|| Restore register. */
		move.w	ccr,d0
		and.w	#0x10,d0
		bne	BADD7			/* if(X) goto BADD7; */
		clr.l	d0			/* return(0); */
		rts
BADD7:		move.l	BADDml(sp),d0
		beq	BADD9			/* if(ml == 0) return(1); */
		subq.l	#1,d0			/* ml--; */
BADD8:		addq.l	#1,(a0)+			/* ++(*mm++); */
		dbcc	d0,BADD8		/* if(Carry || ml--) goto BADD8 */
		scs	d0
		neg.b	d0
		extb.l	d0
		rts				/* return(Carry) */
BADD9:		moveq	#1,d0
		rts				/* return(1); */

		.globl	_BnnSubtractBorrow
_BnnSubtractBorrow:
		.set	BSBnn,4
		.set	BSBnl,8
		.set	BSBcar,12
		move.l	BSBcar(sp),d0
		bne	BSB2			/* if(car) return(car); */
		move.l	BSBnl(sp),d0
		beq	BSB3			/* if(nl == 0) return(0); */
		move.l	BSBnn(sp),a0
		subq.l	#1,d0			/* nl--; */
BSB1:		subq.l	#1,(a0)+			/* (*nn++)--; */
		dbcc	d0,BSB1			/* if(Carry || nl--) goto BSB1 */
		scc	d0
		neg.b	d0
		extb.l	d0
BSB2:		rts				/* return(Carry) */
BSB3:		moveq	#0,d0
		rts				/* return(0); */

		.globl	_BnnSubtract
_BnnSubtract:
		.set	BSmm,4
		.set	BSml,8
		.set	BSnn,12
		.set	BSnl,16
		.set	BScar,20
		move.l	BSmm(sp),a0
		move.l	BSnn(sp),a1
		move.l	BSnl(sp),d1
		sub.l	d1,BSml(sp)		/* ml -= nl; */
		tst.l	d1
		bne	BS1			/* if(nl) goto BS1 */
		tst.l	BScar(sp)
		beq	BS7			/* if(!car) goto BS7 */
		moveq	#1,d0
		rts				/* return(1); */
BS1:		subq.l	#1,d1			/* nl--; */
		move.l	BScar(sp),d0
		neg.b	d0			/* Bit No 4 */
		not.b	d0
		move.w	d0,ccr			/* X = ~car; */
		move.l	d2,-(sp)			/*|| Save register. */
BSX:		move.l	(a1)+,d0
		move.l	(a0),d2
		subx.l	d0,d2			/* N = *mm - *(nn++) - X */
		move.l	d2,(a0)+			/* X = N >> 32; *(mm++) = N; */
		dbf	d1,BSX			/* if(nl--) goto BSX */
		move.l	(sp)+,d2			/*|| Restore register. */
		move.w	ccr,d0
		and.w	#0x10,d0
		bne	BS7			/* if(X) goto BS7; */
		moveq	#1,d0			/* return(1); */
		rts
BS7:		move.l	BSml(sp),d1
		beq	BS9			/* if(ml == 0) goto BS9; */
		subq.l	#1,d1			/* ml--; */
BS8:		subq.l	#1,(a0)+			/* --(*m++); */
		dbcc	d1,BS8			/* if(Carry || ml--) goto BS8 */
		scc	d0
		neg.b	d0
		extb.l	d0
		rts				/* return(C) */
BS9:		clr.l	d0
		rts				/* return(0); */

		.globl	_BnnMultiplyDigit
_BnnMultiplyDigit:
		.set	BMDpp,4
		.set	BMDpl,8
		.set	BMDmm,12
		.set	BMDml,16
		.set	BMDd,20
		move.l	BMDd(sp),d0
		bne	BMD1			/* if(d) goto BMD1; */
		rts				/* return(0); */
BMD1:		cmp.l	#1,d0
		bne	BMD2			/* if(d != 1) goto BMD2; */
		clr.l	BMDd(sp)
		bra	_BnnAdd			/* BnnAdd(p,pl,m,ml,0); */
BMD2:		move.l	BMDpp(sp),a0
		move.l	BMDmm(sp),a1
		move.l	BMDml(sp),d1
		sub.l	d1,BMDpl(sp)		/* pl -= ml; */
		movem.l	#0x3c00,-(sp)		/* Save 4 registers */
		clr.l	d2			/* low = 0; */
		clr.l	d5
		bra	BMD6			/* goto BMD6; */
BMD3:		move.l	(a1)+,d4			/* X = *(mm++); */
		mulu.l	d0,d3:d4		/* X *= d; */
		add.l	d2,d4			/* X += low; */
		addx.l	d5,d3			/* X(hight) += Carry; */
		add.l	(a0),d4			/* X += *pp; */
		addx.l	d5,d3			/* X(hight) += Carry; */
		move.l	d4,(a0)+			/* *(pp++) = X(low); */
		move.l	d3,d2			/* low = X(hight); */
BMD6:		dbf	d1,BMD3			/* if(ml--) goto BMD3; */
		move.l	d2,d0
		movem.l	(a7)+,#0x003C		/* Restore 4 registers */
		add.l	d0,(a0)+			/* *(pp++) += low; */
		bcs	BMD7			/* if(Carry) goto BMD7; */
		clr.l	d0
		rts				/* return(0); */
BMD7:		move.l	BMDpl(sp),d0
		subq.l	#1,d0			/* pl--; */
		beq	BMD10			/* if(!pl) goto BM10; */
		subq.l	#1,d0			/* pl--; */
BMD8:		addq.l	#1,(a0)+			/* ++(*pp++); */
BMD9:		dbcc	d0,BMD8			/* if(Carry || pl--) goto BMD8 */
		scs	d0
		neg.b	d0
		extb.l	d0
		rts				/* return(Carry); */
BMD10:		moveq	#1,d0
		rts				/* return(1); */

		.globl	_BnnDivideDigit
_BnnDivideDigit:
		.set	BDDqq,12
		.set	BDDnn,16
		.set	BDDnl,20
		.set	BDDd,24
		movem.l	#0x3000,-(sp)		/* Save 2 registers */
		move.l	BDDqq(sp),a1
		move.l	BDDnn(sp),a0
		move.l	BDDnl(sp),d0
		move.l	BDDd(sp),d1
		lea	(0,a0,d0.l*4),a0	/* nn = &nn[nl]; */
		subq.l	#1,d0			/* nl--; */
		lea	(0,a1,d0.l*4),a1	/* qq = &qq[nl]; */
		move.l	-(a0),d2			/*| X(hight) = *(--nn); */
		bra	BDD2			/* goto BDD2; */
BDD1:		move.l	-(a0),d3			/* X(low) = *(--nn); */
		divu.l	d1,d2:d3		/* X(low) = X / d; */
						/* X(hight) = X % d; */
		move.l	d3,-(a1)			/* *(--qq) = X(low); */
BDD2:		dbf	d0,BDD1			/* if(nl--) goto BDD1; */
		move.l	d2,d0			/*| return(X(hight)); */
		movem.l	(a7)+,#0x000C		/* Restore 2 registers */
		rts
