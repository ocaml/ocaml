| Copyright     Digital Equipment Corporation & INRIA     1988, 1989
|
|		KerN for the 68020 : MIT syntax
|		[Bepaul]
|
		.text

		.globl	_BnnSetToZero
_BnnSetToZero:			BSTZnn	=	4
				BSTZnl	=	8
		movl	sp@(BSTZnn),a0
		movl	sp@(BSTZnl),d0
		dbf	d0,BSTZ1		| if(nl--) goto BSTZ1;
		rts				| return;
BSTZ1:		clrl	a0@+			| *(nn++) = 0;
		dbf	d0,BSTZ1		| if(nl--) goto BSTZ1;
		rts				| return;

		.globl	_BnnAssign
_BnnAssign:			BAGmm	=	4
				BAGnn	=	8
				BAGnl	=	12
		movl	sp@(BAGmm),a0
		movl	sp@(BAGnn),a1
		movl	sp@(BAGnl),d0
		cmpl	a1,a0
		jcc	BAG2			| if(mm >= nn) goto BAG2;
		dbf	d0,BAG1			| if(nl--) goto BAG1;
		rts				| return;
BAG1:		movl	a1@+,a0@+		| *(mm++) = *(nn++);
		dbf	d0,BAG1			| if(nl--) goto BAG1;
		rts				| return;
BAG2:		jls	BAG4			| if(mm <= nn) goto BAG4;
		lea	a0@(0,d0:l:4),a0	| mm = &mm[nl];
		lea	a1@(0,d0:l:4),a1	| nn = &nn[nl];
		dbf	d0,BAG3			| if(nl--) goto BAG3;
		rts				| return;
BAG3:		movl	a1@-,a0@-		| *(--mm) = *(--nn);
		dbf	d0,BAG3			| if(nl--) goto BAG3;
BAG4:		rts				| return;

		.globl	_BnnSetDigit
_BnnSetDigit:			BSDnn	=	4
				BSDd	=	8
		movl	sp@(BSDnn),a0
		movl	sp@(BSDd),a0@		| *nn = d;
		rts				| return;

		.globl	_BnnGetDigit
_BnnGetDigit:			BGDnn	=	4
		movl	sp@(BGDnn),a0
		movl	a0@,d0			| return(*nn);
		rts

		.globl	_BnnNumDigits
_BnnNumDigits:			BNDnn	=	4
				BNDnl	=	8
		movl	sp@(BNDnn),a0
		movl	sp@(BNDnl),d0
		lea	a0@(0,d0:l:4),a0	| nn = &nn[nl];
		dbf	d0,BND1			| if(nl--) goto BND1;
		moveq	#1,d0
		rts				| return(1);
BND1:		tstl	a0@-
		jne	BND3			| if(*(--nn) != 0) goto BND3;
		dbf	d0,BND1			| if(nl--) goto BND1;
		moveq	#1,d0
		rts				| return(1);
BND3:		addql	#1,d0
		rts				| return(nl + 1);

		.globl	_BnnNumLeadingZeroBitsInDigit
_BnnNumLeadingZeroBitsInDigit:	BLZd	=	4
		bfffo	sp@(BLZd){#0:#32},d0
		rts

		.globl	_BnnDoesDigitFitInWord
_BnnDoesDigitFitInWord:		BDFd	=	4
		moveq	#1,d0			| C_VERSION
		rts

		.globl	_BnnIsDigitZero
_BnnIsDigitZero:			BDZd	=	4
		clrl	d0
		tstl	sp@(BDZd)
		seq	d0
		rts				| return(d == 0);

		.globl	_BnnIsDigitNormalized
_BnnIsDigitNormalized:		BDNd	=	4
		clrl	d0
		tstw	sp@(BDNd)
		smi	d0
		rts				| return(d < 0);

		.globl	_BnnIsDigitOdd
_BnnIsDigitOdd:			BDOd	=	4
		clrl	d0
		movw	sp@(BDOd+2),cc
		scs	d0
		rts				| return(d & 1);

		.globl	_BnnCompareDigits
_BnnCompareDigits:		BCDd1	=	4
				BCDd2	=	8
		movl	sp@(BCDd1),d1
		cmpl	sp@(BCDd2),d1
		bhi	BCDsup			| if(d1 > d2) goto BCDsup;
		sne	d0
		extbl	d0
		rts				| return(-(d1 < d2));
BCDsup:		moveq	#1,d0
		rts				| return(1);

		.globl	_BnnComplement
_BnnComplement:			BCMnn	=	4
				BCMnl	=	8
		movl	sp@(BCMnn),a0
		movl	sp@(BCMnl),d0
		dbf	d0,BCM1			| if(nl--) goto BCM1;
		rts				| return;
BCM1:		notl	a0@+			| *(nn++) ^= -1;
		dbf	d0,BCM1			| if(nl--) goto BCM1;
		rts				| return;

		.globl	_BnnAndDigits
_BnnAndDigits:			BADnn	=	4
				BADd	=	8
		movl	sp@(BADnn),a0
		movl	sp@(BADd),d0
		andl	d0,a0@			| *n &= d;
		rts				| return;

		.globl	_BnnOrDigits
_BnnOrDigits:			BODnn	=	4
				BODd	=	8
		movl	sp@(BODnn),a0
		movl	sp@(BODd),d0
		orl	d0,a0@			| *n |= d;
		rts				| return;

		.globl	_BnnXorDigits
_BnnXorDigits:			BXDnn	=	4
				BXDd	=	8
		movl	sp@(BXDnn),a0
		movl	sp@(BXDd),d0
		eorl	d0,a0@			| *n ^= d;
		rts				| return;

		.globl	_BnnShiftLeft
_BnnShiftLeft:			BSLmm	=	4
				BSLml	=	8
				BSLnbi	=	12
		clrl	d0			| res = 0;
		movl	sp@(BSLnbi),d1
		jne	BSL0			| if(nbi) goto BSL0;
		rts				| return(res);
BSL0:		movl	sp@(BSLmm),a0
		moveml	#0x3C00,sp@-		| Save 4 registers
		movl	sp@(BSLml + 16),d2
		moveq	#32,d3			| rnbi = BN_DIGIT_SIZE;
		subl	d1,d3			| rnbi -= nbi;
		dbf	d2,BSL1			| if(ml--) goto BSL1;
		moveml	a7@+,#0x003C		| Restore 4 registers
		rts				| return(res);
BSL1:		movl	a0@,d4			| save = *mm;
		movl	d4,d5			| X = save;
		lsll	d1,d5			| X <<= nbi;
		orl	d0,d5			| X |= res;
		movl	d5,a0@+			| *(mm++) = X;
		movl	d4,d0			| res = save;
		lsrl	d3,d0			| res >>= rnbi;
		dbf	d2,BSL1			| if(ml--) goto BSL1;
		moveml	a7@+,#0x003C		| Restore 4 registers
		rts				| return(res);

		.globl	_BnnShiftRight
_BnnShiftRight:			BSRmm	=	4
				BSRml	=	8
				BSRnbi	=	12
		clrl	d0			| res = 0;
		movl	sp@(BSRnbi),d1
		jne	BSR0			| if(nbi) goto BSR0;
		rts				| return(res);
BSR0:		movl	sp@(BSRmm),a0
		moveml	#0x3C00,sp@-		| Save 4 registers
		movl	sp@(BSRml + 16),d2
		lea	a0@(0,d2:l:4),a0	| mm = &mm[ml];
		moveq	#32,d3			| lnbi = BN_DIGIT_SIZE;
		subl	d1,d3			| lnbi -= nbi;
		dbf	d2,BSR1			| if(ml--) goto BSR1;
		moveml	a7@+,#0x003C		| Restore 4 registers
		rts				| return(res);
BSR1:		movl	a0@-,d4			| save = *(--mm);
		movl	d4,d5			| X = save;
		lsrl	d1,d5			| X >>= nbi;
		orl	d0,d5			| X |= res;
		movl	d5,a0@			| *mm = X;
		movl	d4,d0			| res = save;
		lsll	d3,d0			| res <<= lnbi;
BSR2:		dbf	d2,BSR1			| if(ml--) goto BSR1;
		moveml	a7@+,#0x003C		| Restore 4 registers
		rts				| return(res);

		.globl	_BnnAddCarry
_BnnAddCarry:			BACnn	=	4
				BACnl	=	8
				BACcar	=	12
		movl	sp@(BACcar),d0		|
		jeq	BAC2			| if(car == 0) return(car);
		movl	sp@(BACnl),d0		|
		jeq	BAC3			| if(nl == 0) return(1);
		movl	sp@(BACnn),a0
		subql	#1,d0			| nl--;
BAC1:		addql	#1,a0@+			| ++(*nn++);
		dbcc	d0,BAC1			| if(Carry || nl--) goto BAC1
		scs	d0
		negb	d0
		extbl	d0
BAC2:		rts				| return(Carry)
BAC3:		moveq	#1,d0
		rts				| return(1);
		
		.globl	_BnnAdd
_BnnAdd:			BADDmm	=	4
				BADDml	=	8
				BADDnn	=	12
				BADDnl	=	16
				BADDcar	=	20
		movl	sp@(BADDmm),a0
		movl	sp@(BADDnn),a1
		movl	sp@(BADDnl),d1
		subl	d1,sp@(BADDml)		| ml -= nl;
		tstl	d1
		jne	BADD1			| if(nl) goto BADD1
		tstl	sp@(BADDcar)		||
		jne	BADD7			| if(car) goto BADD7
		clrl	d0
		rts				| return(0);
BADD1:		subql	#1,d1			| nl--;
		movl	sp@(BADDcar),d0
		negb	d0			| /* Bit No 4 */
		movw	d0,cc			| X = car;
		movl	d2,sp@-			||| Save register.
BADDX:		movl	a1@+,d0
		movl	a0@,d2
		addxl	d0,d2			| N = *mm + *(nn++) + X
		movl	d2,a0@+			| X = N >> 32; *(mn++) = N;
		dbf	d1,BADDX		| if(nl--) goto BADDX
		movl	sp@+,d2			||| Restore register.
		movw	cc,d0
		andw	#0x10,d0
		jne	BADD7			| if(X) goto BADD7;
		clrl	d0			| return(0);
		rts
BADD7:		movl	sp@(BADDml),d0
		jeq	BADD9			| if(ml == 0) return(1);
		subql	#1,d0			| ml--;
BADD8:		addql	#1,a0@+			| ++(*mm++);
		dbcc	d0,BADD8		| if(Carry || ml--) goto BADD8
		scs	d0
		negb	d0
		extbl	d0
		rts				| return(Carry)
BADD9:		moveq	#1,d0
		rts				| return(1);

		.globl	_BnnSubtractBorrow
_BnnSubtractBorrow:		BSBnn	=	4
				BSBnl	=	8
				BSBcar	=	12
		movl	sp@(BSBcar),d0
		jne	BSB2			| if(car) return(car);
		movl	sp@(BSBnl),d0
		jeq	BSB3			| if(nl == 0) return(0);
		movl	sp@(BSBnn),a0
		subql	#1,d0			| nl--;
BSB1:		subql	#1,a0@+			| (*nn++)--;
		dbcc	d0,BSB1			| if(Carry || nl--) goto BSB1
		scc	d0
		negb	d0
		extbl	d0
BSB2:		rts				| return(Carry)
BSB3:		moveq	#0,d0
		rts				| return(0);

		.globl	_BnnSubtract
_BnnSubtract:			BSmm	=	4
				BSml	=	8
				BSnn	=	12
				BSnl	=	16
				BScar	=	20
		movl	sp@(BSmm),a0
		movl	sp@(BSnn),a1
		movl	sp@(BSnl),d1
		subl	d1,sp@(BSml)		| ml -= nl;
		tstl	d1
		jne	BS1			| if(nl) goto BS1
		tstl	sp@(BScar)
		jeq	BS7			| if(!car) goto BS7
		moveq	#1,d0
		rts				| return(1);
BS1:		subql	#1,d1			| nl--;
		movl	sp@(BScar),d0
		negb	d0			| /* Bit No 4 */
		notb	d0
		movw	d0,cc			| X = ~car;
		movl	d2,sp@-			||| Save register.
BSX:		movl	a1@+,d0
		movl	a0@,d2
		subxl	d0,d2			| N = *mm - *(nn++) - X
		movl	d2,a0@+			| X = N >> 32; *(mm++) = N;
		dbf	d1,BSX			| if(nl--) goto BSX
		movl	sp@+,d2			||| Restore register.
		movw	cc,d0
		andw	#0x10,d0
		jne	BS7			| if(X) goto BS7;
		moveq	#1,d0			| return(1);
		rts
BS7:		movl	sp@(BSml),d1
		jeq	BS9			| if(ml == 0) goto BS9;
		subql	#1,d1			| ml--;
BS8:		subql	#1,a0@+			| --(*m++);
		dbcc	d1,BS8			| if(Carry || ml--) goto BS8
		scc	d0
		negb	d0
		extbl	d0
		rts				| return(C)
BS9:		clrl	d0
		rts				| return(0);

		.globl	_BnnMultiplyDigit
_BnnMultiplyDigit:		BMDpp	=	4
				BMDpl	=	8
				BMDmm	=	12
				BMDml	=	16
				BMDd	=	20
		movl	sp@(BMDd),d0
		jne	BMD1			| if(d) goto BMD1;
		rts				| return(0);
BMD1:		cmpl	#1,d0
		jne	BMD2			| if(d != 1) goto BMD2;
		clrl	sp@(BMDd)
		bra	_BnnAdd			| BnnAdd(p,pl,m,ml,0);
BMD2:		movl	sp@(BMDpp),a0
		movl	sp@(BMDmm),a1
		movl	sp@(BMDml),d1
		subl	d1,sp@(BMDpl)		| pl -= ml;
		moveml	#0x3c00,sp@-		| Save 4 registers
		clrl	d2			| low = 0;
		clrl	d5
		bra	BMD6			| goto BMD6;
BMD3:		movl	a1@+,d4			| X = *(mm++);
		mulul	d0,d3:d4		| X *= d;
		addl	d2,d4			| X += low;
		addxl	d5,d3			| X(hight) += Carry;
		addl	a0@,d4			| X += *pp;
		addxl	d5,d3			| X(hight) += Carry;
		movl	d4,a0@+			| *(pp++) = X(low);
		movl	d3,d2			| low = X(hight);
BMD6:		dbf	d1,BMD3			| if(ml--) goto BMD3;
		movl	d2,d0
		moveml	a7@+,#0x003C		| Restore 4 registers
		addl	d0,a0@+			| *(pp++) += low;
		bcs	BMD7			| if(Carry) goto BMD7;
		clrl	d0
		rts				| return(0);
BMD7:		movl	sp@(BMDpl),d0
		subql	#1,d0			| pl--;
		jeq	BMD10			| if(!pl) goto BM10;
		subql	#1,d0			| pl--;
BMD8:		addql	#1,a0@+			| ++(*pp++);
BMD9:		dbcc	d0,BMD8			| if(Carry || pl--) goto BMD8
		scs	d0
		negb	d0
		extbl	d0
		rts				| return(Carry);
BMD10:		moveq	#1,d0
		rts				| return(1);

		.globl	_BnnDivideDigit
_BnnDivideDigit:		BDDqq	=	12
				BDDnn	=	16
				BDDnl	=	20
				BDDd	=	24
		moveml	#0x3000,sp@-		| Save 2 registers
		movl	sp@(BDDqq),a1
		movl	sp@(BDDnn),a0
		movl	sp@(BDDnl),d0
		movl	sp@(BDDd),d1
		lea	a0@(0,d0:l:4),a0	| nn = &nn[nl];
		subql	#1,d0			| nl--;
		lea	a1@(0,d0:l:4),a1	| qq = &qq[nl];
		movl	a0@-,d2			|| X(hight) = *(--nn);
		bra	BDD2			| goto BDD2;
BDD1:		movl	a0@-,d3			| X(low) = *(--nn);
		divul	d1,d2:d3		| X(low) = X / d;
						| X(hight) = X % d;
		movl	d3,a1@-			| *(--qq) = X(low);
BDD2:		dbf	d0,BDD1			| if(nl--) goto BDD1;
		movl	d2,d0			|| return(X(hight));
		moveml	a7@+,#0x000C		| Restore 2 registers
		rts
