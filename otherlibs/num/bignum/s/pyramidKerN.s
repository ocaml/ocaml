# Copyright     Digital Equipment Corporation & INRIA     1988, 1989
#
#		KerN for Pyramid Architecture
#		Bernard Paul Serpette
#
		.text	0

	.globl	_BnnSetToZero
_BnnSetToZero:	subw	$1,pr1			# nl--;
		blt	BSTZ2			# if(nl < 0) return;
BSTZ1:		movw	$0,(pr0)		# *nn = 0;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--;
		bge	BSTZ1			# if(nl >= 0) goto BSTZ1;
BSTZ2:		ret				# return;

	.globl	_BnnAssign
_BnnAssign:	ucmpw	pr1,pr0
		bgt	BAG3			# if(mm > nn) goto BAG3;
		subw	$1,pr2			# nl--;
		bge	BAG2			# if(nl >= 0) goto BAG2;
		ret
BAG1:		addw	$4,pr0			# mm++;
		addw	$4,pr1			# nn++;
BAG2:		movw	(pr1),(pr0)		# *mm = *nn;
		subw	$1,pr2			# nl--;
		bge	BAG1			# if(nl >= 0) goto BAG1;
		ret

BAG3:		mova	(pr1)[pr2*0x4],pr1	# nn += nl;
		mova	(pr0)[pr2*0x4],pr0	# mm += nl;
		subw	$1,pr2			# nl--;
		blt	BAG5			# if(nl < 0) return;
BAG4:		subw	$4,pr0			# mm--;
		subw	$4,pr1			# nn--;
		movw	(pr1),(pr0)		# *mm = *nn;
		subw	$1,pr2			# nl--;
		bge	BAG4			# if(nl >= 0) goto BAG4;
BAG5:		ret

	.globl	_BnnSetDigit
_BnnSetDigit:	movw	pr1,(pr0)		# *nn = d;
		ret

	.globl	_BnnGetDigit
_BnnGetDigit:	movw	(pr0),pr0		# return(*nn);
		ret

	.globl	_BnnNumDigits
_BnnNumDigits:
		mova	(pr0)[pr1*0x4],pr0      # nn += nl;
		br	BND2
BND1:		subw	$4,pr0			# nn--;
		mtstw	(pr0),pr2
		bne	BND3			# if(*nn) goto BND3
		subw	$1,pr1			# nl--;
BND2:		mtstw	pr1,pr2
		bne	BND1			# if(nl) goto BND1;
		movw	$1,pr0			# return(1);
		ret
BND3:		movw	pr1,pr0			# return(nl);
		ret

	.globl	_BnnNumLeadingZeroBitsInDigit
_BnnNumLeadingZeroBitsInDigit:
		movw	$0,pr1			# p = 0;
		mtstw	pr0,pr0
		bne	BLZ2			# if(!d) goto BLZ2;
		movw	$32,pr0			# return(32);
		ret
BLZ1:		addw	$1,pr1			# p++;
		lshlw	$1,pr0			# d <<= 1;
BLZ2:		mtstw	pr0,pr0
		bgt	BLZ1			# if(d > 0) goto BLZ1;
		movw	pr1,pr0			# return(p);
		ret

	.globl	_BnnDoesDigitFitInWord
_BnnDoesDigitFitInWord:
		movw	$1,pr0			# return(1);
		ret

	.globl	_BnnIsDigitZero
_BnnIsDigitZero:
		mtstw	pr0,pr0			# set NZVC flags
		mpsw	pr0			# mov NZVC flags in register
		andw	$4,pr0			# return(Z);
		ret

	.globl	_BnnIsDigitNormalized
_BnnIsDigitNormalized:
		mtstw	pr0,pr0			# set NZVC flags
		mpsw	pr0			# mov NZVC flags in register
		andw	$8,pr0			# return(N);
		ret

	.globl	_BnnIsDigitOdd
_BnnIsDigitOdd:
		andw	$1,pr0			# return(d & 1);
		ret

	.globl	_BnnCompareDigits
_BnnCompareDigits:
		ucmpw	pr1,pr0
		bgt	BCDsup
		bne	BCDinf
		movw	$0,pr0
		ret
BCDinf:		movw	$-1,pr0
		ret
BCDsup:		movw	$1,pr0
		ret

	.globl	_BnnComplement
_BnnComplement:
		subw	$1,pr1			# nl--;
		blt	BCM2			# if(nl < 0) goto BCM2
BCM1:		mcomw	(pr0),pr2		# tmp = *nn ^ -1;
		movw	pr2,(pr0)		# *nn = tmp;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--;
		bge	BCM1			# if(nl >= 0) goto BCM1;
BCM2:		ret

	.globl	_BnnAndDigits
_BnnAndDigits:	andw    (pr0),pr1		# d &= *nn;
		movw    pr1,(pr0)		# *nn = d;
		ret

	.globl	_BnnOrDigits
_BnnOrDigits:	orw	(pr0),pr1		# d |= *nn;
		movw	pr1,(pr0)		# *nn = d;
		ret

	.globl	_BnnXorDigits
_BnnXorDigits:	xorw	(pr0),pr1		# d ^= *nn;
		movw	pr1,(pr0)		# *nn = d;
		ret

	.globl	_BnnShiftLeft
_BnnShiftLeft:	movw	$0,lr1			# res = 0;
		mtstw	pr2,pr2
		beq	BSL2			# if(!nbi) return(res);
		movw	$32,lr2			# rnbi = 32;
		subw	pr2,lr2			# rnbi -= nbi;
		subw	$1,pr1			# ml--;
		blt	BSL2			# if(ml < 0) return(res);
BSL1:		movw	(pr0),lr0		# save = *mm;
		movw	lr0,pr3			# X = save;
		lshlw	pr2,pr3			# X <<= nbi;
		orw	lr1,pr3			# X |= res;
		movw	pr3,(pr0)		# *mm = X;
		addw	$4,pr0			# mm++;
		movw	lr0,lr1			# res = save;
		lshrw	lr2,lr1			# res >>= rnbi;
		subw	$1,pr1			# ml--;
		bge	BSL1			# if(ml >= 0) goto BSL1;
BSL2:		movw	lr1,pr0			# return(res);
		ret

	.globl	_BnnShiftRight
_BnnShiftRight:	movw	$0,lr1			# res = 0;
		mtstw	pr2,pr2
		beq	BSR2			# if(!nbi) return(res);
		mova	(pr0)[pr1*0x4],pr0      # mm += ml;
		movw	$32,lr2			# lnbi = 32;
		subw	pr2,lr2			# lnbi -= nbi;
		subw	$1,pr1			# ml--;
		blt	BSR2			# if(ml < 0) return(res);
BSR1:		subw	$4,pr0			# mm--;
		movw	(pr0),lr0		# save = *mm;
		movw	lr0,pr3			# X = save;
		lshrw	pr2,pr3			# X >>= nbi;
		orw	lr1,pr3			# X |= res;
		movw	pr3,(pr0)		# *mm = X;
		movw	lr0,lr1			# res = save;
		lshlw	lr2,lr1			# res <<= lnbi;
		subw	$1,pr1			# ml--;
		bge	BSR1			# if(ml >= 0) goto BSR1;
BSR2:		movw	lr1,pr0			# return(res);
		ret

	.globl	_BnnAddCarry
_BnnAddCarry:	mtstw	pr2,pr2
		beq	BAC3			# if(!carryin) return(0);
		mtstw	pr1,pr1
		beq	BAC2			# if(!nl) return(1);
		subw	$1,pr1			# nl--;
BAC1:		icmpw	$0,(pr0)		# Z = (++(nn) == 0);
		bne	BAC3			# if(!Z) goto BAC3;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--
		bge	BAC1			# if(nl >= 0) goto BAC1;
BAC2:		movw	$1,pr0			# return(1);
		ret
BAC3:		movw	$0,pr0			# return(0);
		ret

	.globl	_BnnAdd
_BnnAdd:	subw	pr3,pr1			# ml -= nl;
		mtstw	pr3,pr3
		beq	BADD5			# if(!nl) goto BADD5;
BADD1:		subw	$1,pr3			# nl--;
BADDX:		movw	(pr0),pr5		# X1 = *mm
		bicpsw	$1
		bispsw	pr4			# Set the carry C;
		addwc	(pr2),pr5		# X1 += *nn + C;
		mpsw	pr4
		andw	$1,pr4			# get the carry C;
		movw	pr5,(pr0)		# *mm = X1;
		addw	$4,pr0			# mm++;
		addw	$4,pr2			# nn++;
		subw	$1,pr3			# nl--;
		bge	BADDX			# if(nl >= 0) goto BADDX;
BADD5:		mtstw	pr4,pr4
		bne	BADD7			# if(car) goto BADD7;
BADD6:		movw	$0,pr0			# return(0);
		ret
BADD7:		mtstw	pr1,pr1
		beq	BADD9			# if(!ml) return(1);
		subw	$1,pr1			# ml--;
BADD8:		icmpw	$0,(pr0)		# Z = (++(mm) == 0);
		bne	BADD6			# if(!Z) goto BADD6;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--
		bge	BADD8			# if(nl >= 0) goto BADD8;
BADD9:		movw	$1,pr0			# return(1);
		ret

	.globl	_BnnSubtractBorrow
_BnnSubtractBorrow:
		mtstw	pr2,pr2
		bne	BSB3			# if(carryin) return(1);
		mtstw	pr1,pr1
		beq	BSB2			# if(!nl) return(1);
		subw	$1,pr1			# nl--;
BSB1:		dcmpw	$-1,(pr0)		# Z = (--(nn) == -1);
		bne	BSB3			# if(!Z) goto BSB3;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--
		bge	BSB1			# if(nl >= 0) goto BSB1;
BSB2:		movw	$0,pr0			# return(0);
		ret
BSB3:		movw	$1,pr0			# return(1);
		ret


	.globl	_BnnSubtract
_BnnSubtract:	subw	pr3,pr1			# ml -= nl;
		mtstw	pr3,pr3
		beq	BS5			# if(!nl) goto BS5;
BS1:		subw	$1,pr3			# nl--;
BSX:		movw	(pr0),pr5		# X1 = *mm
		bicpsw	$1
		bispsw	pr4			# Set the carry C;
		subwb	(pr2),pr5		# X1 -= *nn + C;
		mpsw	pr4
		andw	$1,pr4			# get the carry C;
		movw	pr5,(pr0)		# *mm = X1;
		addw	$4,pr0			# mm++;
		addw	$4,pr2			# nn++;
		subw	$1,pr3			# nl--;
		bge	BSX			# if(nl >= 0) goto BSX;
BS5:		mtstw	pr4,pr4
		beq	BS7			# if(!car) goto BS7;
BS6:		movw	$1,pr0			# return(1);
		ret
BS7:		mtstw	pr1,pr1
		beq	BS9			# if(!ml) return(1);
		subw	$1,pr1			# ml--;
BS8:		dcmpw	$-1,(pr0)		# Z = (--(mm) == -1);
		bne	BS6			# if(!Z) goto BS6;
		addw	$4,pr0			# nn++;
		subw	$1,pr1			# nl--
		bge	BS8			# if(nl >= 0) goto BS8;
BS9:		movw	$0,pr0			# return(0);
		ret

	.globl	_BnnMultiplyDigit		# (pp, pl, mm, ml, d)
_BnnMultiplyDigit:
		mtstw	pr4,pr4
		bne	BMD1			# if(!d) return(0);
		movw	$0,pr0
		ret
BMD1:		ucmpw	$1,pr4
		bne	BMD2			# if(d != 1) goto BMD2;
		movw	$0,pr4
		br	_BnnAdd			# BnnAdd(p,pl,m,ml,0);
BMD2:		subw	pr3,pr1			# pl -= ml;
		movw	$0,pr8			# Un zero.
		movw	pr8,pr7			# low = 0;
		br	BMD4
BMD3:		subw	$1,pr3			# pl--;
		movw	(pr2),pr6		# X = *mm;
		addw	$4,pr2			# mm++;
		uemul	pr4,pr5			# X *= d;
		addw	pr7,pr6			# X += low;
		addwc	pr8,pr5			# X(hight) += Carry;
		addw	(pr0),pr6		# X += *pp;
		addwc	pr8,pr5			# X(hight) += Carry;
		movw	pr6,(pr0)		# *pp = X(low);
		addw	$4,pr0			# pp++;
		movw	pr5,pr7			# low = X(Hight);
BMD4:		mtstw	pr3,pr3
		bne	BMD3			# if(ml) goto BMD3;
		addw	(pr0),pr7		# low += *pp;
		movw	pr7,(pr0)		# *pp = low;
		bcs	BMD7			# if(Carry) goto BMD7;
BMD6:		movw	$0,pr0			# return(0);
		ret
BMD7:		addw	$4,pr0			# pp++;
		subw	$1,pr1			# pl--;
		beq	BMD10			# if(!pl) return(1);
		subw	$1,pr1			# pl--;
BMD8:		icmpw	$0,(pr0)		# Z = (++(*pp) == 0)
		bne	BMD6			# if(!!Z) goto BADD6;
		addw	$4,pr0			# pp++;
		subw	$1,pr1			# pl--
		bge	BMD8			# if(pl >= 0) goto BADD8;
BMD10:		movw	$1,pr0			# return(1);
		ret

# The 64 bits/32 bits unsigned division, like in Vaxes, must be simulated
#by a 64/32 signed division:
#
#N = D*Q + R
#D = 2D' + d0
#Cas 1: 0 <= D < 2^31
#------
#	Sous-cas 1: N < D'*2^32	-> Calcul direct signe'
#	-----------
#
#	Sous-cas 2: N >= D'*2^32
#	-----------
#		N = 2N' + n0
#		N' = DQ' + R'			(0 <= R' < D)
#		N  = 2DQ' + 2R' + n0		(0 <= 2R' + n0 < 2D)
#			Si 2R' + n0 < D
#				Q = 2Q' et R = 2R' + n0
#			sinon	Q = 2Q' + 1 et R = 2R' + n0 - D
#
#Cas 2: 2^31 <= D < 2^32
#------
#	N = 8N' + 4n2 + 2n1 + n0
#	N' = D'Q' + R'					(0 <= R' <= D' - 1)
#	N = 8D'Q' + 8R' + 4n2 + 2n1 + n0
#	N = 4DQ' + 8R' + 4n2 + 2n1 + n0 - 4Q'd0
#	N = 4DQ' + 2(2(2R' + n2 - Q'd0) + n1) + n0	(0 <= 2R' + n2 < D)
#	    Q' < 2^31 <= D
#	    -D <= R1 = 2R' + n2 - Q'd0 < D
#	Si d0 = 1 et -D < R1 < 0
#	    Q1 = Q' - 1; R1 = R1 + D
#	N = 4Q1D + 2(2R1 + n1) + n0
#	Q0 = 2Q1; R0 = 2R1 + n1
#	Si R2 >= D
#	    Q0 = Q0 + 1; R2 = R2 - D
#	N = 2Q0 + 2R0 + n0
#	Q = 2Q0; R = 2R0 + n0
#	Si R >= d
#	    Q = Q + 1; R = R - D
	.globl	_BnnDivideDigit			# (qq, nn, nl, d)
_BnnDivideDigit:
		subw	$1,pr2			# nl--;
		mova	(pr1)[pr2*0x4],pr1      # nn += nl;
		mova	(pr0)[pr2*0x4],pr0      # qq += nl;
		movw	(pr1),pr4		# N(Hight) = *nn;
		movw	pr3,pr6
		lshrw	$1,pr6			# D' = D >> 1;
		mtstw	pr3,pr3
		bge	BDD2
		movw	pr3,lr5
		andw	$1,lr5
		movw	$1,lr6			# lr6 <- 0x1FFFFFFF
		lshlw	$29,lr6			# pour le
		subw	$1,lr6			# shift arithme'tique
		br	BDD5
BDD1:		subw	$4,pr1			# nn--;
		movw	(pr1),pr5		# N(low) = *nn;
		ucmpw	pr6,pr4
		blt	BDD11			# if(N < D'*2^32) goto BDD11;
		movw	pr5,lr0
		andw	$1,lr0			# n0 = N & 1;
		ashrl	$1,pr4			# N = N' = N / 2;
		ediv	pr3,pr4			# Q = N' / D
						# R = N' % D
		lshlw	$1,pr4			# Q = 2Q
		lshlw	$1,pr5			# R = 2R;
		addw	lr0,pr5			# R = R + n0
		ucmpw	pr3,pr5
		blt	BDD12			# if(R < D) goto BDD12;
		addw	$1,pr4			# Q = Q + 1;
		subw	pr3,pr5			# R = R - D;
		br	BDD12			# goto BDD12
BDD11:		ediv	pr3,pr4			# N(Hight) = N / d;
						# N(low) = N % d;
BDD12:		subw	$4,pr0			# qq--;
		movw	pr4,(pr0)		# *qq = X(low);
		movw	pr5,pr4
BDD2:		subw	$1,pr2
		bge	BDD1
		movw	pr4,pr0
		ret

BDD3:		subw	$4,pr1			# nn--;
		movw	(pr1),pr5		# N(low) = *nn;
		movw	pr5,lr0
		andw	$1,lr0			# lr0 = n0 = N & 1;
		movw	pr5,lr1
		andw	$2,lr1			# lr1 = 2n1 = N & 2;
		movw	pr5,lr2
		andw	$4,lr2			# lr2 = 4n2 = N & 4;
		ashrl	$3,pr4			# N = N' = N / 8;
		andw	lr6,pr4	    		# shift arithme'tique!!
		ediv	pr6,pr4			# Q' = N' / D';
						# R' = N' % D';
		addw	pr5,pr5			# R1 = 2 * R'; Q1 = Q';
		mtstw	lr5,lr5
		beq	BDD33			# if(d0 == 0) goto BDD33;
		ucmpw	pr4,pr5
		bge	BDD32			# if(R1 >= Q') goto BDD32;
		subw	pr4,pr5			# R1 = R1 - Q'
		subw	$1,pr4			# Q1 = Q1 - 1;
		addw	pr3,pr5			# R1 = R1 + D;
		br	BDD33
BDD32:		subw	pr4,pr5			# R1 = R1 - Q'
BDD33:		addw	pr4,pr4			# Q0 = 2 * Q1;
		addw	pr5,pr5			# R0 = 2 * R1;
		bcs	BDD4
		ucmpw	pr3,pr5
		blt	BDD40			# if(R0 < D) goto BDD40;
BDD4:		addw	$1,pr4			# Q0 = Q0 + 1;
		subw	pr3,pr5			# R0 = R0 - D
BDD40:		addw	pr4,pr4			# Q = 2 * Q0;
		addw	pr5,pr5			# R = 2 * R0;
		bcs	BDD41
		ucmpw	pr3,pr5
		blt	BDD42			# if(R < D) goto BDD42;
BDD41:		addw	$1,pr4			# Q = Q + 1;
		subw	pr3,pr5			# R = R - D;
BDD42:		addw	lr2,pr5
		addw	lr1,pr5
		addw	lr0,pr5			# R = R + lr2 + lr1 + lr0;
		ucmpw	pr3,pr5
		blt	BDD43			# if(R < D) goto BDD43
		addw	$1,pr4			# Q = Q + 1;
		subw	pr3,pr5			# R = R - D;
BDD43:		subw	$4,pr0			# qq--;
		movw	pr4,(pr0)		# *qq = X(low);
		movw	pr5,pr4
BDD5:		subw	$1,pr2
		bge	BDD3
		movw	pr4,pr0
		ret

