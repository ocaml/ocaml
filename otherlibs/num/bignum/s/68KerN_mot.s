| Copyright     Digital Equipment Corporation & INRIA     1988, 1989
|
|		KerN for the 68020 : MOTOROLA syntax
|		[Bepaul]
|
		SECTION	10

		XDEF	_BnnSetToZero
BSTZnn		EQU	4
BSTZnl		EQU	8
_BnnSetToZero	MOVE.L	BSTZnn(A7),A0
		MOVE.L	BSTZnl(A7),D0
		DBF	D0,BSTZ1		| if(nl--) goto BSTZ1;
		RTS				| return;
BSTZ1		CLR.L	(A0)+			| *(nn++) = 0;
		DBF	D0,BSTZ1		| if(nl--) goto BSTZ1;
		RTS				| return;

		XDEF	_BnnAssign
BAGmm		EQU	4
BAGnn		EQU	8
BAGnl		EQU	12
_BnnAssign	MOVE.L	BAGmm(A7),A0
		MOVE.L	BAGnn(A7),A1
		MOVE.L	BAGnl(A7),D0
		CMP.L	A1,A0
		BCC	BAG2			| if(mm >= nn) goto BAG2;
		DBF	D0,BAG1			| if(nl--) goto BAG1;
		RTS				| return;
BAG1		MOVE.L	(A1)+,(A0)+		| *(mm++) = *(nn++);
		DBF	D0,BAG1			| if(nl--) goto BAG1;
		RTS				| return;
BAG2		BLS	BAG4			| if(mm <= nn) goto BAG4;
		LEA	0(A0,D0.L*4),A0		| mm = &mm[nl];
		LEA	0(A1,D0.L*4),A1		| nn = &nn[nl];
		DBF	D0,BAG3			| if(nl--) goto BAG3;
		RTS				| return;
BAG3		MOVE.L	-(A1),-(A0)		| *(--mm) = *(--nn);
		DBF	D0,BAG3			| if(nl--) goto BAG3;
BAG4		RTS				| return;

		XDEF	_BnnSetDigit
BSDnn		EQU	4
BSDd		EQU	8
_BnnSetDigit	MOVE.L	BSDnn(A7),A0
		MOVE.L	BSDd(A7),(A0)		| *nn = d;
		RTS				| return;

		XDEF	_BnnGetDigit
BGDnn		EQU	4
_BnnGetDigit	MOVE.L	BGDnn(A7),A0
		MOVE.L	(A0),D0			| return(*nn);
		RTS

		XDEF	_BnnNumDigits
BNDnn		EQU	4
BNDnl		EQU	8
_BnnNumDigits	MOVE.L	BNDnn(A7),A0
		MOVE.L	BNDnl(A7),D0
		LEA	0(A0,D0.L*4),A0		| nn = &nn[nl];
		DBF	D0,BND1			| if(nl--) goto BND1;
		MOVEQ	#1,D0
		RTS				| return(1);
BND1		TST.L	-(A0)
		BNE	BND3			| if(*(--nn) != 0) goto BND3;
		DBF	D0,BND1			| if(nl--) goto BND1;
		MOVEQ	#1,D0
		RTS				| return(1);
BND3		ADDQ.L	#1,D0
		RTS				| return(nl + 1);

		XDEF	_BnnNumLeadingZeroBitsInDigit
BLZd		EQU	4
_BnnNumLeadingZeroBitsInDigit
		BFFFO	BLZd(A7){#0:#32},D0
		RTS

		XDEF	_BnnDoesDigitFitInWord
BDFd		EQU	4
_BnnDoesDigitFitInWord		
		MOVEQ	#1,D0			| C_VERSION
		RTS

		XDEF	_BnnIsDigitZero
BDZd		EQU	4
_BnnIsDigitZero	CLR.L	D0
		TST.L	BDZd(A7)
		SEQ	D0
		RTS				| return(d == 0);

		XDEF	_BnnIsDigitNormalized
BDNd		EQU	4
_BnnIsDigitNormalized
		CLR.L	D0
		TST.W	BDNd(A7)
		SMI	D0
		RTS				| return(d < 0);

		XDEF	_BnnIsDigitOdd
BDOd		EQU	4
_BnnIsDigitOdd	CLR.L	D0
		MOVE	BDOd+2(A7),CCR
		SCS	D0
		RTS				| return(d & 1);

		XDEF	_BnnCompareDigits
BCDd1		EQU	4
BCDd2		EQU	8
_BnnCompareDigits
		MOVE.L	BCDd1(A7),D1
		CMP.L	BCDd2(A7),D1
		BHI	BCDsup			| if(d1 > d2) goto BCDsup;
		SNE	D0
		EXTB.L	D0
		RTS				| return(-(d1 < d2));
BCDsup		MOVEQ	#1,D0
		RTS				| return(1);

		XDEF	_BnnComplement
BCMnn		EQU	4
BCMnl		EQU	8
_BnnComplement	MOVE.L	BCMnn(A7),A0
		MOVE.L	BCMnl(A7),D0
		DBF	D0,BCM1			| if(nl--) goto BCM1;
		RTS				| return;
BCM1		NOT.L	(A0)+			| *(nn++) ^= -1;
		DBF	D0,BCM1			| if(nl--) goto BCM1;
		RTS				| return;

		XDEF	_BnnAndDigits
BADnn		EQU	4
BADd		EQU	8
_BnnAndDigits	MOVE.L	BADnn(A7),A0
		MOVE.L	BADd(A7),D0
		AND.L	D0,(A0)			| *n &= d;
		RTS				| return;

		XDEF	_BnnOrDigits
BODnn		EQU	4
BODd		EQU	8
_BnnOrDigits	MOVE.L	BODnn(A7),A0
		MOVE.L	BODd(A7),D0
		OR.L	D0,(A0)			| *n |= d;
		RTS				| return;

		XDEF	_BnnXorDigits
BXDnn		EQU	4
BXDd		EQU	8
_BnnXorDigits
		MOVE.L	BXDnn(A7),A0
		MOVE.L	BXDd(A7),D0
		EOR.L	D0,(A0)			| *n ^= d;
		RTS				| return;

		XDEF	_BnnShiftLeft
BSLmm		EQU	4
BSLml		EQU	8
BSLnbi		EQU	12
_BnnShiftLeft	CLR.L	D0			| res = 0;
		MOVE.L	BSLnbi(A7),D1
		BNE	BSL0			| if(nbi) goto BSL0;
		RTS				| return(res);
BSL0		MOVE.L	BSLmm(A7),A0
		MOVEM.L	D2-D5,-(A7)		| Save 4 registers
		MOVE.L	BSLml+16(A7),D2
		MOVEQ	#32,D3			| rnbi = BN_DIGIT_SIZE;
		SUB.L	D1,D3			| rnbi -= nbi;
		DBF	D2,BSL1			| if(ml--) goto BSL1;
		MOVEM.L	(A7)+,D2-D5		| Restore 4 registers
		RTS				| return(res);
BSL1		MOVE.L	(A0),D4			| save = *mm;
		MOVE.L	D4,D5			| X = save;
		LSL.L	D1,D5			| X <<= nbi;
		OR.L	D0,D5			| X |= res;
		MOVE.L	D5,(A0)+			| *(mm++) = X;
		MOVE.L	D4,D0			| res = save;
		LSR.L	D3,D0			| res >>= rnbi;
		DBF	D2,BSL1			| if(ml--) goto BSL1;
		MOVEM.L	(A7)+,D2-D5		| Restore 4 registers
		RTS				| return(res);

		XDEF	_BnnShiftRight
BSRmm		EQU	4
BSRml		EQU	8
BSRnbi		EQU	12
_BnnShiftRight	CLR.L	D0			| res = 0;
		MOVE.L	BSRnbi(A7),D1
		BNE	BSR0			| if(nbi) goto BSR0;
		RTS				| return(res);
BSR0		MOVE.L	BSRmm(A7),A0
		MOVEM.L	D2-D5,-(A7)		| Save 4 registers
		MOVE.L	BSRml+16(A7),D2
		LEA	0(A0,D2.L*4),A0		| mm = &mm[ml];
		MOVEQ	#32,D3			| lnbi = BN_DIGIT_SIZE;
		SUB.L	D1,D3			| lnbi -= nbi;
		DBF	D2,BSR1			| if(ml--) goto BSR1;
		MOVEM.L	(A7)+,D2-D5		| Restore 4 registers
		RTS				| return(res);
BSR1		MOVE.L	-(A0),D4			| save = *(--mm);
		MOVE.L	D4,D5			| X = save;
		LSR.L	D1,D5			| X >>= nbi;
		OR.L	D0,D5			| X |= res;
		MOVE.L	D5,(A0)			| *mm = X;
		MOVE.L	D4,D0			| res = save;
		LSL.L	D3,D0			| res <<= lnbi;
BSR2		DBF	D2,BSR1			| if(ml--) goto BSR1;
		MOVEM.L	(A7)+,D2-D5		| Restore 4 registers
		RTS				| return(res);

		XDEF	_BnnAddCarry
BACnn		EQU	4
BACnl		EQU	8
BACcar		EQU	12
_BnnAddCarry	MOVE.L	BACcar(A7),D0		|
		BEQ	BAC2			| if(car == 0) return(car);
		MOVE.L	BACnl(A7),D0		|
		BEQ	BAC3			| if(nl == 0) return(1);
		MOVE.L	BACnn(A7),A0
		SUBQ.L	#1,D0			| nl--;
BAC1		ADDQ.L	#1,(A0)+		| ++(*nn++);
		DBCC	D0,BAC1			| if(Carry || nl--) goto BAC1
		SCS	D0
		NEG.B	D0
		EXTB.L	D0
BAC2		RTS				| return(Carry)
BAC3		MOVEQ	#1,D0
		RTS				| return(1);
		
		XDEF	_BnnAdd
BADDmm		EQU	4
BADDml		EQU	8
BADDnn		EQU	12
BADDnl		EQU	16
BADDcar		EQU	20
_BnnAdd		MOVE.L	BADDmm(A7),A0
		MOVE.L	BADDnn(A7),A1
		MOVE.L	BADDnl(A7),D1
		SUB.L	D1,BADDml(A7)		| ml -= nl;
		TST.L	D1
		BNE	BADD1			| if(nl) goto BADD1
		TST.L	BADDcar(A7)		||
		BNE	BADD7			| if(car) goto BADD7
		CLR.L	D0
		RTS				| return(0);
BADD1		SUBQ.L	#1,D1			| nl--;
		MOVE.L	BADDcar(A7),D0
		NEG.B	D0			| /* Bit No 4 */
		MOVE	D0,CCR			| X = car;
		MOVE.L	D2,-(A7)		||| Save register.
BADDX		MOVE.L	(A1)+,D0
		MOVE.L	(A0),D2
		ADDX.L	D0,D2			| N = *mm + *(nn++) + X
		MOVE.L	D2,(A0)+		| X = N >> 32; *(mn++) = N;
		DBF	D1,BADDX		| if(nl--) goto BADDX
		MOVE.L	(A7)+,D2		||| Restore register.
		MOVE	CCR,D0
		AND.W	#0x10,D0
		BNE	BADD7			| if(X) goto BADD7;
		CLR.L	D0			| return(0);
		RTS
BADD7		MOVE.L	BADDml(A7),D0
		BEQ	BADD9			| if(ml == 0) return(1);
		SUBQ.L	#1,D0			| ml--;
BADD8		ADDQ.L	#1,(A0)+			| ++(*mm++);
		DBCC	D0,BADD8		| if(Carry || ml--) goto BADD8
		SCS	D0
		NEG.B	D0
		EXTB.L	D0
		RTS				| return(Carry)
BADD9		MOVEQ	#1,D0
		RTS				| return(1);

		XDEF	_BnnSubtractBorrow
BSBnn		EQU	4
BSBnl		EQU	8
BSBcar		EQU	12
_BnnSubtractBorrow
		MOVE.L	BSBcar(A7),D0
		BNE	BSB2			| if(car) return(car);
		MOVE.L	BSBnl(A7),D0
		BEQ	BSB3			| if(nl == 0) return(0);
		MOVE.L	BSBnn(A7),A0
		SUBQ.L	#1,D0			| nl--;
BSB1		SUBQ.L	#1,(A0)+			| (*nn++)--;
		DBCC	D0,BSB1			| if(Carry || nl--) goto BSB1
		SCC	D0
		NEG.B	D0
		EXTB.L	D0
BSB2		RTS				| return(Carry)
BSB3		MOVEQ	#0,D0
		RTS				| return(0);

		XDEF	_BnnSubtract
BSmm		EQU	4
BSml		EQU	8
BSnn		EQU	12
BSnl		EQU	16
BScar		EQU	20
_BnnSubtract	MOVE.L	BSmm(A7),A0
		MOVE.L	BSnn(A7),A1
		MOVE.L	BSnl(A7),D1
		SUB.L	D1,BSml(A7)		| ml -= nl;
		TST.L	D1
		BNE	BS1			| if(nl) goto BS1
		TST.L	BScar(A7)
		BEQ	BS7			| if(!car) goto BS7
		MOVEQ	#1,D0
		RTS				| return(1);
BS1		SUBQ.L	#1,D1			| nl--;
		MOVE.L	BScar(A7),D0
		NEG.B	D0			| /* Bit No 4 */
		NOT.B	D0
		MOVE	D0,CCR			| X = ~car;
		MOVE.L	D2,-(A7)		||| Save register.
BSX		MOVE.L	(A1)+,D0
		MOVE.L	(A0),D2
		SUBX.L	D0,D2			| N = *mm - *(nn++) - X
		MOVE.L	D2,(A0)+		| X = N >> 32; *(mm++) = N;
		DBF	D1,BSX			| if(nl--) goto BSX
		MOVE.L	(A7)+,D2		||| Restore register.
		MOVE	CCR,D0
		AND.W	#0x10,D0
		BNE	BS7			| if(X) goto BS7;
		MOVEQ	#1,D0			| return(1);
		RTS
BS7		MOVE.L	BSml(A7),D1
		BEQ	BS9			| if(ml == 0) goto BS9;
		SUBQ.L	#1,D1			| ml--;
BS8		SUBQ.L	#1,(A0)+			| --(*m++);
		DBCC	D1,BS8			| if(Carry || ml--) goto BS8
		SCC	D0
		NEG.B	D0
		EXTB.L	D0
		RTS				| return(C)
BS9		CLR.L	D0
		RTS				| return(0);

		XDEF	_BnnMultiplyDigit
BMDpp		EQU	4
BMDpl		EQU	8
BMDmm		EQU	12
BMDml		EQU	16
BMDd		EQU	20
_BnnMultiplyDigit
		MOVE.L	BMDd(A7),D0
		BNE	BMD1			| if(d) goto BMD1;
		RTS				| return(0);
BMD1		CMP.L	#1,D0
		BNE	BMD2			| if(d != 1) goto BMD2;
		CLR.L	BMDd(A7)
		BRA	_BnnAdd			| BnnAdd(p,pl,m,ml,0);
BMD2		MOVE.L	BMDpp(A7),A0
		MOVE.L	BMDmm(A7),A1
		MOVE.L	BMDml(A7),D1
		SUB.L	D1,BMDpl(A7)		| pl -= ml;
		MOVEM.L	D2-D5,-(A7)		| Save 4 registers
		CLR.L	D2			| low = 0;
		CLR.L	D5
		BRA	BMD6			| goto BMD6;
BMD3		MOVE.L	(A1)+,D4		| X = *(mm++);
		MULU.L	D0,D3:D4		| X *= d;
		ADD.L	D2,D4			| X += low;
		ADDX.L	D5,D3			| X(hight) += Carry;
		ADD.L	(A0),D4			| X += *pp;
		ADDX.L	D5,D3			| X(hight) += Carry;
		MOVE.L	D4,(A0)+		| *(pp++) = X(low);
		MOVE.L	D3,D2			| low = X(hight);
BMD6		DBF	D1,BMD3			| if(ml--) goto BMD3;
		MOVE.L	D2,D0
		MOVEM.L	(A7)+,D2-D5		| Restore 4 registers
		ADD.L	D0,(A0)+		| *(pp++) += low;
		BCS	BMD7			| if(Carry) goto BMD7;
		CLR.L	D0
		RTS				| return(0);
BMD7		MOVE.L	BMDpl(A7),D0
		SUBQ.L	#1,D0			| pl--;
		BEQ	BMD10			| if(!pl) goto BM10;
		SUBQ.L	#1,D0			| pl--;
BMD8		ADDQ.L	#1,(A0)+		| ++(*pp++);
BMD9		DBCC	D0,BMD8			| if(Carry || pl--) goto BMD8
		SCS	D0
		NEG.B	D0
		EXTB.L	D0
		RTS				| return(Carry);
BMD10		MOVEQ	#1,D0
		RTS				| return(1);

		XDEF	_BnnDivideDigit
BDDqq		EQU	12
BDDnn		EQU	16
BDDnl		EQU	20
BDDd		EQU	24
_BnnDivideDigit	MOVEM.L	D2-D3,-(A7)		| Save 2 registers
		MOVE.L	BDDqq(A7),A1
		MOVE.L	BDDnn(A7),A0
		MOVE.L	BDDnl(A7),D0
		MOVE.L	BDDd(A7),D1
		LEA	0(A0,D0.L*4),A0		| nn = &nn[nl];
		SUBQ.L	#1,D0			| nl--;
		LEA	0(A1,D0.l*4),A1		| qq = &qq[nl];
		MOVE.L	-(A0),D2		|| X(hight) = *(--nn);
		BRA	BDD2			| goto BDD2;
BDD1		MOVE.L	-(A0),D3		| X(low) = *(--nn);
		DIVU.L	D1,D2:D3		| X(low) = X / d;
						| X(hight) = X % d;
		MOVE.L	D3,-(A1)		| *(--qq) = X(low);
BDD2		DBF	D0,BDD1			| if(nl--) goto BDD1;
		MOVE.L	D2,D0			|| return(X(hight));
		MOVEM.L	(A7)+,D2-D3		| Restore 2 registers
		RTS
