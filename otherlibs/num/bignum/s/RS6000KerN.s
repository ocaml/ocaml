# Copyright     Digital Equipment Corporation & INRIA     1988, 1989
#
#               KerN for the RS6000
#               [Bepaul]
#
# La plupart du code est celui ge'ne're' par le compilo C (Cha^peau!)
#
		.set	RES,3
		.set	CA1,3
		.set	CA2,4
		.set	CA3,5
		.set	CA4,6
		.set	CA5,7
		.set	X4,7
		.set	X3,8
		.set	X2,9
		.set	X1,10

		.set	NN1,CA1
		.set	MM1,CA1
		.set	D1,CA1
		.set	NN2,CA2
		.set	NL2,CA2
		.set	ML2,CA2
		.set	D2,CA2
		.set	NN3,CA3
		.set	NL3,CA3
		.set	NL4,CA4

		.toc
T.bignum_dat:	.tc	bignum_dat[TC],bignum_dat[RW]
		.csect	bignum_dat[RW]
		.csect	bignum_txt[PR]

		.globl	.BnnSetToZero	# BnnSetToZero(nn, nl)
.BnnSetToZero:	cmpi	0,NL2,0		# if(nl <= 0) return;
		bler
		mtctr	NL2		# ctr = nl;
		lil	X1,0		# cte = 0;
		ai	NN1,NN1,-4	# nn--;
BSTZ1:		stu	X1,4(NN1)	# *(++nn) = cte;
		bdn	BSTZ1		# if(--ctr != 0) goto BSTZ1;
		br			# return;

		.globl	.BnnAssign	# BnnAssign(mm, nn, nl)
.BnnAssign:	cmpi	0,NL3,0		# if(nl <= 0) return;
		bler
		mtctr	NL3		# ctr = nl;
		cmpl	0,MM1,NN2	# if(mm >= nn) goto BAG2;
		bge	BAG2
		ai	MM1,MM1,-4	# mm--;
		ai	NN2,NN2,-4	# nn--;
BAG1:		lu	X1,4(NN2)	# X = *(++nn);
		stu	X1,4(MM1)	# *(++mm) = X;
		bdn	BAG1		# if(--ctr != 0) goto BAG1;
		br
BAG2:		beqr			# if(mm == nn) return;
		sli	X1,NL3,2
		a	NN2,NN2,X1	# nn += nl;
		a	MM1,MM1,X1	# mm += nl;
BAG3:		lu	X1,-4(NN2)	# X = *(--nn);
		stu	X1,-4(MM1)	# *(--mm) = X;
		bdn	BAG3		# (if(--ctr != 0) goto BAG3;
		br			# return;

		.globl	.BnnSetDigit	# BnnSetDigit(nn, d)
.BnnSetDigit:	st	D2,0(NN1)
		br

		.globl	.BnnGetDigit	# BnnGetDigit(nn)
.BnnGetDigit:	l	RES,0(NN1)
		br

		.globl	.BnnNumDigits	# BnnNumDigits(nn, nl)
.BnnNumDigits:	cmpi	0,NL2,0		# if(nl <= 0) return(1);
		ble	BND2
		sli	X1,NL2,2
		a	NN1,NN1,X1	# nn += nl;
		mtctr	NL2		# ctr = nl;
BND1:		lu	X1,-4(NN1)	# X = *(--nn);
		cmpi	0,X1,0		# if(X != 0) goto BND3
		bne	BND3
		bdn	BND1		# if(--ctr != 0) goto BND1;
BND2:		lil	RES,1		# return(1);
		br
BND3:		mfctr	RES		# return(ctr);
		br

		.globl	.BnnNumLeadingZeroBitsInDigit	# (d)
.BnnNumLeadingZeroBitsInDigit:
		cntlz	RES,D1		# Yeah!
		br

		.globl	.BnnDoesDigitFitInWord	# (d)
.BnnDoesDigitFitInWord:
		lil	RES,1		# return(1);
		br

		.globl	.BnnIsDigitZero	# BnnIsDigitZero(d)
.BnnIsDigitZero:	# Use the fact that nabs(d) >=0 <=> d == 0
		nabs	RES,D1
		rlinm	RES,RES,1,31,31	# sign in the lowest bit.
		xoril	RES,RES,1	# get the inverse.
		br

		.globl	.BnnIsDigitNormalized	# (d)
.BnnIsDigitNormalized:
		rlinm	RES,D1,1,31,31	# sign in the lowest bit.
		br

		.globl	.BnnIsDigitOdd	# BnnIsDigitOdd(d)
.BnnIsDigitOdd:	rlinm	RES,D1,0,31,31	# only the lowest bit.
		br

		.globl	.BnnCompareDigits	# BnnCompareDigits(d1, d2)
.BnnCompareDigits:
		cmpl	0,D1,D2		# if(d1 == d2) return(0);
		beq	BSD0
		bgt	BCDsup		# if(d1 > d2) return(1);
		lil	RES,-1		# return(-1);
BSDret:		br
BCDsup:		lil	RES,1		# return(1);
		br
BSD0:		lil	RES,0		# return(0);
		br

		.globl	.BnnComplement	#.BnnComplement(nn, nl)
.BnnComplement:	cmpi	0,NL2,0		# if(nl <= 0) return;
		bler
		ai	NN1,NN1,-4	# nn--;
		mtctr	NL2		# ctr = nl;
BCM1:		l	X1,4(NN1)	# X = nn[1];
		sfi	X1,X1,-1	# X ^= -1;
		stu	X1,4(NN1)	# *++nn = X;
		bdn	BCM1		# if(--ctr > 0) goto BCM1
		br			# return;

		.globl	.BnnAndDigits	# BnnAndDigits(nn, d)
.BnnAndDigits:	l	X1,0(NN1)	# X = *nn;
		and	X1,X1,D2	# X &= d;
		st	X1,0(NN1)	# *nn = X;
		br

		.globl	.BnnOrDigits	# BnnOrDigits(nn, d)
.BnnOrDigits:	l	X1,0(NN1)	# X = *nn;
		or	X1,X1,D2	# X |= d;
		st	X1,0(NN1)	# *nn = X;
		br

		.globl	.BnnXorDigits	# BnnXorDigits(nn, d)
.BnnXorDigits:	l	X1,0(NN1)	# X = *nn;
		xor	X1,X1,D2	# X ^= d;
		st	X1,0(NN1)	# *nn = X;
		br

		.globl	.BnnShiftLeft	# BnnShiftLeft(mm, ml, nbits)
# here and in the next funxtion we use the fact that MM1 == RES.
	.set NBI,CA3; .set SMM,X1; .set RNB,X2; .set SX,X3; .set SY,ML2
.BnnShiftLeft:	oril	SMM,MM1,0
		lil	RES,0		# res = 0;
		cmpi	0,NBI,0		# if(nbits == 0) return(res);
		beqr
		cmpi	0,ML2,0		# if(ml <= 0) return(res);
		bler
		sfi	RNB,NBI,32	# rnbits = 32 - nbits;
		ai	SMM,SMM,-4	# mm--;
		mtctr	ML2		# ctr = ml;
BSL1:		l	SX,4(SMM)	# X = mm[1];
		sl	SY,SX,NBI	# Y = X << nbits;
		or	SY,SY,RES	# Y |= res;
		stu	SY,4(SMM)	# *(++mm) = Y;
		sr	RES,SX,RNB	# res = X >> rnbits;
		bdn	BSL1		# if(--ctr > 0) goto BSL1
		br			# return(res);

		.globl	.BnnShiftRight	# BnnShiftRight(mm, ml, nbits)
.BnnShiftRight:	sli	X1,ML2,2	# mm += ml;
		a	SMM,MM1,X1
		lil	RES,0		# res = 0;
		cmpi	0,NBI,0		# if(nbits == 0) return(res);
		beqr
		cmpi	0,ML2,0		# if(ml <= 0) return(res);
		bler
		sfi	RNB,NBI,32	# rnbits = 32 - nbits;
		mtctr	ML2		# ctr = ml;
BSR1:		lu	SX,-4(SMM)	# X = *(--mm);
		sr	SY,SX,NBI	# Y = X >> nbits;
		or	SY,SY,RES	# Y |= res;
		st	SY,0(SMM)	# *(mm) = Y;
		sl	RES,SX,RNB	# res = X << rnbits;
		bdn	BSR1		# if(--ctr > 0) goto BSR1
		br			# return(res);

		.globl	.BnnAddCarry	# BnnAddCarry(nn, nl, carryin)
	.set CARRY,CA3			# also for BnnSubtractBorrow
.BnnAddCarry:	cmpi	0,CARRY,0	# if(carryin == 0) return(0);
		beq	BAC3
		cmpi	0,NL2,0		# if(nl == 0) return(1);
		beq	BAC2
		ai	NN1,NN1,-4	# nn--;
		mtctr	NL2		# ctr = nl;
BAC1:		l	X1,4(NN1)	# X = nn[1];
		ai.	X1,X1,1		# X++;
		stu	X1,4(NN1)	# *(++nn) = X;
		bne	BAC3		# if(X != 0) return(0);
		bdn	BAC1		# if(--ctr > 0) goto BAC1
BAC2:		lil	RES,1		# return(1);
		br
BAC3:		lil	RES,0		# return(0);
		br

		.globl	.BnnAdd		# BnnAdd(mm, ml, nn, nl, carryin)
	.set CARRYIN,CA5		# also for BnnSubtract.
.BnnAdd:	sf	ML2,NL4,ML2	# ml -= nl;
		ai	NN3,NN3,-4	# nn--;
		ai	MM1,MM1,-4	# mm--; carry = 1;
		cmpi	0,NL4,0		# if(nl == 0) goto BADD2;
		beq	BADD2
		mtctr	NL4		# ctr = nl;
		cmpi	0,CARRYIN,0	# if(carryin) goto BADD1;
		bne	BADD1
		ai	X1,X1,0		# carry = 0;
BADD1:		lu	X2,4(NN3)	# Y = *(++nn);
		l	X1,4(MM1)	# X = mm[1];
		ae	X1,X1,X2	# X = X + Y + carry; carry = ??
		stu	X1,4(MM1)	# *(++mm) = X;
		bdn	BADD1		# if(--ctr > 0) goto BADD1
		lil	X2,0
		ae	CARRYIN,X2,X2	# carryin = carry;
BADD2:		cmpi	0,CARRYIN,0	# if(carryin == 0) return(0);
		beq	BADD5
		cmpi	0,ML2,0		# if(ml == 0) return(1);
		beq	BADD4
		mtctr	ML2		# ctr = ml;
BADD3:		l	X1,4(MM1)	# X = mm[1];
		ai.	X1,X1,1		# X++;
		stu	X1,4(MM1)	# *(++mm) = X;
		bne	BADD5		# if(X != 0) return(0);
		bdn	BADD3		# if(--ctr > 0) goto BADD3;
BADD4:		lil	RES,1		# return(1);
		br
BADD5:		lil	RES,0		# return(0);
		br

		.globl	.BnnSubtractBorrow	# (nn, nl, carryin)
.BnnSubtractBorrow:
		cmpi	0,CARRY,1	# if(carryin == 1) return(1);
		beq	BSB3
		cmpi	0,NL2,0		# if(nl == 0) return(0);
		beq	BSB2
		ai	NN1,NN1,-4	# nn--;
		mtctr	NL2		# ctr = nl;
BSB1:		l	X1,4(NN1)	# X = nn[1];
		si	X2,X1,1		# Y= X-1;
		stu	X2,4(NN1)	# *(++nn) = Y;
		cmpi	0,X1,0
		bne	BSB3		# if(X != 0) return(1);
		bdn	BSB1		# if(--ctr > 0) goto BSB1
BSB2:		lil	RES,0		# return(0);
		br
BSB3:		lil	RES,1		# return(1);
		br

		.globl	.BnnSubtract	# BnnSubtract(mm, ml, nn, nl, carryin)
.BnnSubtract:	sf	ML2,NL4,ML2	# ml -= nl;
		ai	NN3,NN3,-4	# nn--;
		ai	MM1,MM1,-4	# mm--; carry = 1;
		cmpi	0,NL4,0		# if(nl == 0) goto BS2
		beq	BS2
		mtctr	NL4		# ctr = nl;
		cmpi	0,CARRYIN,0	# if(carryin) goto BS1
		bne	BS1
		ai	X1,X1,0		# carry = 0;
BS1:		lu	X2,4(NN3)	# Y = *(++nn);
		l	X1,4(MM1)	# X = mm[1];
		sfe	X1,X2,X1	# X = X - (Y + carry); carry = ??
		stu	X1,4(MM1)	# *(++mm) = X;
		bdn	BS1		# if(--ctr > 0) goto BS1
		lil	CA5,0
		ae	CA5,CA5,CA5	# carryin = carry;
BS2:		cmpi	0,CA5,1		# if(carryin == 0) return(1);
		beq	BS5
		cmpi	0,ML2,0		# if(ml == 0) return(0);
		beq	BS4
		mtctr	ML2		# ctr = ml;
BS3:		l	X1,4(MM1)	# X = mm[1];
		si	X2,X1,1		# Y= X-1;
		stu	X2,4(MM1)	# *(++mm) = Y;
		cmpi	0,X1,0		# if(X != 0) return(1);
		bne	BS5
		bdn	BS3		# if(--ctr > 0) goto BS3
BS4:		lil	RES,0		# return(0);
		br
BS5:		lil	RES,1		# return(1);
		br

		.globl	.BnnMultiplyDigit # BnnMultiplyDigit(pp, pl, mm, ml, d)
	.set PP,CA1; .set PL,CA2; .set MM,CA3; .set ML,CA4; .set D,CA5
	.set LOW,X1; .set HIGHT,X2; .set OHIGHT,X3
.BnnMultiplyDigit:
		cmpi	0,D,0		# if(d == 0) return(0);
		beq	BMD7
BMD1:		cmpi	0,D,1		# if(d != 1) goto BMD2;
		bne	BMD2
		lil	CA5,0		# return(BnnAdd(pp, pl, mm, ml, 0));
		b	.BnnAdd
BMD2:		sf	PL,ML,PL	# pl -= ml;
		ai	MM,MM,-4	# mm--;
		ai	PP,PP,-4	# pp--;
		cmpi	0,ML,0		# if(ml == 0) return(0);
		beq	BMD7
		mtctr	ML		# ctr = ml;
		lil	OHIGHT,0	# OldHight = 0;
		cmpi	0,D,0		# if(D < 0) goto BMD8;
		blt	BMD8
BMD3:		lu	LOW,4(MM)	# Low = mm[1];
		mul	HIGHT,LOW,D	# Hight:MQ = Low*d
		cmpi	0,LOW,0		# if(Low>=0) pas de correction.
		bge	BMD4
		a	HIGHT,HIGHT,D	# Correction multiplication signe'.
BMD4:		mfmq	LOW		# Low = MQ;
		a	LOW,LOW,OHIGHT	# Low += OldHight;
		aze	HIGHT,HIGHT	# Hight += carry;
		l	OHIGHT,4(PP)	# *++pp += Low;
		a	LOW,LOW,OHIGHT
		stu	LOW,4(PP)
		aze	OHIGHT,HIGHT	# OldHight = Hight + carry;
		bdn	BMD3		# if(--ctr > 0) goto BMD3;
BMD40:		l	LOW,4(PP)	# *(++pp) += OldHight;
		a	LOW,LOW,OHIGHT
		stu	LOW,4(PP)
		lil	LOW,0		# if(carry == 0) return(0);
		aze.	LOW,LOW
		beq	BMD7
		si.	PL,PL,1		# pl--;
		ble	BMD6		# if(pl <= 0) return(1);
		mtctr	PL		# ctr = pl;
BMD5:		l	X1,4(PP)	# X = pp[1];
		ai.	X1,X1,1		# X++;
		stu	X1,4(PP)	# *(++pp) = X;
		bne	BMD7		# if(X != 0) return(0);
		bdn	BMD5		# if(--ctr > 0) goto BMD5;
BMD6:		lil	RES,1		# return(1);
		br
BMD7:		lil	RES,0		# return(0);
		br
		
BMD8:		lu	LOW,4(MM)	# Low = mm[1];
		mul	HIGHT,LOW,D	# Hight:MQ = Low*d
		a	HIGHT,HIGHT,LOW # Correction pour d<0...
		cmpi	0,LOW,0		# if(Low>=0) pas de correction.
		bge	BMD9
		a	HIGHT,HIGHT,D	# Correction multiplication signe'.
BMD9:		mfmq	LOW		# Low = MQ;
		a	LOW,LOW,OHIGHT	# Low += OldHight;
		aze	HIGHT,HIGHT	# Hight += carry;
		l	OHIGHT,4(PP)	# *pp += Low;
		a	LOW,LOW,OHIGHT
		stu	LOW,4(PP)
		aze	OHIGHT,HIGHT	# OldHight = Hight + carry;
		bdn	BMD8		# if(--ctr > 0) goto BMD8;
		b	BMD40		# goto BMD40;

		.globl	.BnnDivideDigit	# BnnDivideDigit(qq, nn, nl, d)
	.set QQ,CA1; .set NN,CA2; .set NL,CA3; .set DD,CA4
	.set SQQ,X1; .set R,CA1; .set Q,X2; .set NLOW,X2; .set DQ,X3
	.set BITS,X4; .set AUX,CA3
.BnnDivideDigit:
		sli	X1,NL,2
		a	NN,NN,X1	# nn = &nn[nl];
		a	SQQ,QQ,X1	# qq = &qq[nl];
		si	SQQ,SQQ,4	# qq--;
		lu	R,-4(NN)	# R = *(--nn);
		si.	NL,NL,1		# nl--;
		bler			# if(nl <= 0) return(R);
		mtctr	NL		# ctr = nl;
		sri	DQ,DD,1		# D'= D / 2;
		cmpi	0,DD,0		# if(D<0) goto BDDinf;
		blt	BDDinf
# D > 0
BDDsup:		lu	NLOW,-4(NN)	# Low = *(--nn);
		cmpl	0,R,DQ		# if (R < D') goto BDDsupinf;
		blt	BDDsupinf
		andil.	BITS,NLOW,1	# bits = Low & 1;
		sri	NLOW,NLOW,1	# Low >>= 1;
		andil.	AUX,R,1		# aux = R & 1;
		sri	R,R,1		# R >>= 1;
		sli	AUX,AUX,31	# Low |= aux << 31;
		or	NLOW,NLOW,AUX
		mtmq	NLOW		# MQ = Low;
		div	Q,R,DD		# Q=R:MQ/D; MQ=R:MQ%D;
		mfmq	R		# R=MQ;
		sli	R,R,1		# R <<= 1;
		sli	Q,Q,1		# Q <<= 1;
		a	R,R,BITS	# R+=bits;
		cmpl	0,R,DD		# si R<D => ok
		blt	BDDsup1
		ai	Q,Q,1		# Q++;
		sf	R,DD,R		# R-=D;
BDDsup1:	stu	Q,-4(SQQ)	# *(--qq)=Q;
		bdn	BDDsup		# if(--ctr > 0) goto BDDsup;
		br			# return(R);
BDDsupinf:	mtmq	NLOW		# MQ = XL;
		div	Q,R,DD		# Q=R:MQ/D; MQ=R:MQ%D;
		mfmq	R		# R=MQ;
		stu	Q,-4(SQQ)	# *(--qq)=Q;
		bdn	BDDsup		# if(--ctr > 0) goto BDDsup;
		br			# return(R);

# D < 0
BDDinf:		lu	NLOW,-4(NN)	# Low = *(--nn);
		andil.	BITS,NLOW,7	# bits = Low & 7;
		sri	NLOW,NLOW,3	# Low >>= 3;
		andil.	AUX,R,7		# aux = R & 7;
		sri	R,R,3		# R >>= 3;
		sli	AUX,AUX,29	# Low |= aux << 29;
		or	NLOW,NLOW,AUX
		mtmq	NLOW		# MQ = Low;
		div	Q,R,DQ		# Q=R:MQ/D'; MQ=R:MQ%D';
		mfmq	R		# R=MQ
		sli	R,R,1		# R *= 2;
		andil.	AUX,DD,1	# if((D & 1) == 0) rien a retrancher;
		cmpli	0,AUX,0
		beq	BDDi4
# R <- R - Q
		cmpl	0,R,Q		# On teste avant de faire la diff.
		blt	BDDi3		# la diff est < 0
		sf	R,Q,R		# la diff est > 0
		b	BDDi4
BDDi3:		sf	R,Q,R		# On met a` jour
		si	Q,Q,1		# Q--;
		a	R,R,DD		# R += D;
# R <- 2R; Q <- 2Q;
BDDi4:		cmpl	0,R,DQ		# On teste avant de faire la mult.
		blt	BDDi41		# Ca va passer..
		bne	BDDi40		# Ca va casser...
		cmpli	0,AUX,0		# d0 = 1 ca passe...
		bne	BDDi41
BDDi40:		sli	Q,Q,1		# Q *= 2;
		sli	R,R,1		# R *= 2;
		ai	Q,Q,1		# Q++;
		sf	R,DD,R		# R -= D;
		b	BDDi5
BDDi41:		sli	Q,Q,1		# Q *= 2;
		sli	R,R,1		# R *= 2;
# R <- 2R; Q <- 2Q;
BDDi5:		cmpl	0,R,DQ		# On teste avant de faire la mult.
		blt	BDDi51		# Ca va passer..
		bne	BDDi50		# Ca va casser...
		cmpli	0,AUX,0		# d0 = 1 ca passe...
		bne	BDDi51
BDDi50:		sli	Q,Q,1		# Q *= 2;
		sli	R,R,1		# R *= 2;
		ai	Q,Q,1		# Q++;
		sf	R,DD,R		# R -= D;
		b	BDDi6
BDDi51:		sli	Q,Q,1		# Q *= 2;
		sli	R,R,1		# R *= 2;
# R += bits;
BDDi6:		sf	AUX,BITS,DD	# pour tester sans de'bordement..
		cmpl	0,R,AUX
		blt	BDDi61		# Ca va passer..
		ai	Q,Q,1		# Q++;
		sf	R,DD,R		# R -= D;
BDDi61:		a	R,R,BITS	# R += bits;
		stu	Q,-4(SQQ)	# *(--qq)=Q;
		bdn	BDDinf		# if(--ctr > 0) goto BDDinf;
		br
