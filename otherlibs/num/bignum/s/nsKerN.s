# Copyright     Digital Equipment Corporation & INRIA     1988, 1989
#
#		KerN for NS32032
#		Francis Dupont
#
		.text

		.globl _BnnSetToZero
		.align 2
_BnnSetToZero:			.set	BSTZnn,4
				.set	BSTZnl,8
		movd	BSTZnn(sp),r0
		movd	BSTZnl(sp),r1
		acbd	0,r1,BSTZ1		# ?? test a 0 + rapide ??
		ret	0
BSTZ1:		movqd	0,0(r0)			# *nn = 0;
		addqd	4,r0			# nn++;
		acbd	-1,r1,BSTZ1		# if!(--nl) goto BSTZ1;
		ret	0

		.globl _BnnAssign
		.align 2
_BnnAssign:			.set	BAGmm,4
				.set	BAGnn,8
				.set	BAGnl,12
		movd	BAGnl(sp),r0
		movd	BAGnn(sp),r1
		movd	BAGmm(sp),r2
		cmpd	r2,r1
		bge	BAG1			# if(mm >= nn) goto BAG1;
		movsd				# bcopy(nn, mm, 4*nl);
		ret	0
BAG1:		addr	r2[r0:d],r2		# mm = &mm[nl];
		addr	r1[r0:d],r1		# nn = &nn[nl];
		addqd	-4,r2			# mm--;
		addqd	-4,r1			# nn--;
		movsd	b			# revbcopy(nn, mm, 4*nl);
		ret	0

		.globl _BnnSetDigit
		.align 2
_BnnSetDigit:			.set	BSDnn,4
				.set	BSDd,8
		movd	BSDd(sp),0(BSDnn(sp))	# *nn = d;
		ret	0

		.globl _BnnGetDigit
		.align 2
_BnnGetDigit:			.set	BGDnn,4
		movd	0(BGDnn(sp)),r0		# return(*nn);
		ret	0

		.globl _BnnNumDigits
		.align 2
_BnnNumDigits:			.set	BNDnn,4
				.set	BNDnl,8
		movd	BNDnl(sp),r0
		cmpqd	0,r0
		beq	BND2			# if(nl == 0) return(1);
		addr	0(BNDnn(sp))[r0:d],r1	# nn = &nn[nd];
BND1:		addqd	-4,r1			# --nn;
		cmpqd	0,0(r1)
		bne	BND3			# if(*nn != 0) return(nl);
		acbd	-1,r0,BND1		# if(!--nl) goto BND1;
BND2:		movqd	1,r0			# return(1);
BND3:		ret	0

		.globl _BnnNumLeadingZeroBitsInDigit
		.align 2
_BnnNumLeadingZeroBitsInDigit:	.set	BLZd,4
		movd	BLZd(sp),r1
		movd	31,r0			# ret = 31;
BLZ1:		tbitd	r0,r1
		bfs	BLZ2			# if(d & 2^ret) goto BLZ2;
		addqd	-1,r0
		bcs	BLZ1			# if(--ret) goto BLZ1;
BLZ2:		negd	r0,r0
		addd	31,r0			# return(31 - ret);
		ret	0

		.globl _BnnDoesDigitFitInWord
		.align 2
_BnnDoesDigitFitInWord:		.set	BDFd,4
		movqd	1,r0			# return(1);
		ret	0

		.globl _BnnIsDigitZero
		.align 2
_BnnIsDigitZero:		.set	BDZd,4
		cmpqd	0,BDZd(sp)		# return(!d);
		seqd	r0
		ret	0

		.globl _BnnIsDigitNormalized
		.align 2
_BnnIsDigitNormalized:		.set	BDNd,4
		tbitd	31,BDNd(sp)		# return(d & 2^31);
		sfsd	r0
		ret	0

		.globl _BnnIsDigitOdd
		.align 2
_BnnIsDigitOdd:			.set	BDOd,4
		movqd	1,r0			# return(d & 1);
		andd	BDOd(sp),r0
		ret	0

		.globl _BnnCompareDigits
		.align 2
_BnnCompareDigits:		.set	BCDd1,4
				.set	BCDd2,8
		cmpd	BCDd1(sp),BCDd2(sp)
		bhs	BCD1			# if(d1 >= d2)
		movqd	-1,r0			# return(-1);
		ret	0
BCD1:		sned	r0			# return(d1 != d2);
		ret	0

		.globl _BnnComplement
		.align 2
_BnnComplement:			.set	BCMnn,4
				.set	BCMnl,8
		movd	BCMnl(sp),r1
		cmpqd	0,r1
		beq	BCM2			# if(nl == 0) return;
		movd	BCMnn(sp),r0
BCM1:		comd	0(r0),0(r0)		# *nn ^= -1;
		addqd	4,r0			# nn++;
		acbd	-1,r1,BCM1		# if(!--nl) goto BCM1;
BCM2:		ret	0

		.globl _BnnAndDigits
		.align 2
_BnnAndDigits:			.set	BADnn,4
				.set	BADd,8
		andd	BADd(sp),0(BADnn(sp))	# *nn &= d;
		ret	0

		.globl _BnnOrDigits
		.align 2
_BnnOrDigits:			.set	BODnn,4
				.set	BODd,8
		ord	BODd(sp),0(BODnn(sp))	# *nn |= d;
		ret	0

		.globl _BnnXorDigits
		.align 2
_BnnXorDigits:			.set	BXDnn,4
				.set	BXDd,8
		xord	BXDd(sp),0(BXDnn(sp))	# *nn ^= d;
		ret	0

		.globl _BnnShiftLeft
		.align 2
_BnnShiftLeft:			.set	BSLmm,8
				.set	BSLml,12
				.set	BSLnbi,16
		enter	[r3,r4,r5,r6],0
		movqd	0,r0			# res = 0;
		movd	BSLnbi(fp),r5
		cmpqd	0,r5
		beq	BSL2			# if(nbi == 0) return(res);
		movd	BSLml(fp),r3
		cmpqd	0,r3
		beq	BSL2			# if(ml == 0) return(res);
		movd	r5,r6
		subd	32,r6			# rnbi = nbi - BN_DIGIT_SIZE;
		movd	BSLmm(fp),r2
BSL1:		movd	0(r2),r1		# save = *mm;
		movd	r1,r4			# X = save;
		lshd	r5,r4			# X <<= nbi;
		ord	r0,r4			# X |= res;
		movd	r4,0(r2)		# *mm = X;
		addqd	4,r2			# mm++;
		movd	r1,r0			# res = save;
		lshd	r6,r0			# res <<= rnbi;
		acbd	-1,r3,BSL1		# if(!--nl) goto BSL1;
BSL2:		exit	[r3,r4,r5,r6]
		ret	0

		.globl _BnnShiftRight
		.align 2
_BnnShiftRight:			.set	BSRmm,8
				.set	BSRml,12
				.set	BSRnbi,16
		enter	[r3,r4,r5,r6],0
		movqd	0,r0			# res = 0;
		movd	BSRnbi(fp),r1
		cmpqd	0,r1			# if(nbi == 0) return(res);
		beq	BSR2
		movd	BSRml(fp),r3
		cmpqd	0,r3
		beq	BSR2			# if(ml == 0) return(res);
		addr	@32,r6
		subd	r1,r6			# rnbi = BN_DIGIT_SIZE - nbi;
		negd	r1,r5			# nbi = - nbi;
		addr	0(BSRmm(fp))[r3:d],r2	# mm = &mm[ml];
BSR1:		addqd	-4,r2			# mm--;
		movd	0(r2),r1		# save = *mm;
		movd	r1,r4			# X = save;
		lshd	r5,r4			# X <<= nbi;
		ord	r0,r4			# X |= res
		movd	r4,0(r2)		# *mm = X;
		movd	r1,r0			# res = save;
		lshd	r6,r0			# res <<= rnbi;
		acbd	-1,r3,BSR1		# if(!--nl) goto BSR1;
BSR2:		exit	[r3,r4,r5,r6]
		ret	0

		.globl _BnnAddCarry
		.align 2
_BnnAddCarry:			.set	BACnn,4
				.set	BACnl,8
				.set	BACcar,12
		cmpqd	0,BACcar(sp)
		beq	BAC3			# if(car == 0) return(0);
		movd	BACnl(sp),r0
		cmpqd	0,r0			# if(nl = 0) return(1);
		beq	BAC2
		movd	BACnn(sp),r1
BAC1:		addqd	1,0(r1)			# ++(*nn);
		bcc	BAC3			# if(!Carry) return(0);
		addqd	4,r1			# nn++;
		acbd	-1,r0,BAC1		# if(!--nl) goto BAC1;
BAC2:		movqd	1,r0			# return(1);
		ret	0
BAC3:		movqd	0,r0			# return(0);
		ret	0

		.globl _BnnAdd
		.align 2
_BnnAdd:			.set	BADDmm,8
				.set	BADDml,12
				.set	BADDnn,16
				.set	BADDnl,20
				.set	BADDcar,24
		enter	[r3,r4,r5],0
		movd	BADDnl(fp),r4
		movd	BADDcar(fp),r1
		movd	BADDmm(fp),r2
		movd	BADDnn(fp),r3
		movd	BADDml(fp),r5
		subd	r4,r5			# ml -= nl
BADD1:		cmpqd	0,r4
		beq	BADD4			# if(nl == 0) goto BADD4;
		addqd	-1,r4			# nl--;
		addd	0(r2),r1		# car += *mm;
		bcc	BADD2			# if(!Carry) goto BADD2;
		movd	0(r3),0(r2)		# *mm = *nn;
		addqd	4,r3			# nn++;
		addqd	4,r2			# mm++;
		movqd	1,r1			# car = 1
		br	BADD1			# goto BADD1
BADD2:		movd	0(r3),r0		# save = *nn;
		addqd	4,r3			# nn++;
		addd	r0,r1			# car += save;
		movd	r1,0(r2)		# *mm = car;
		addqd	4,r2			# mm++;
		cmpd	r1,r0
		slod	r1			# car = (car < save) ? 1 : 0;
		br	BADD1			# goto BADD1;

BADD4:		cmpqd	0,r1			# if (car == 0) return(0);
		beq	BADD8
		cmpqd	0,r5			# if (ml == 0) return(1);
		beq	BADD9
BADD5:		addqd	1,0(r2)			# ++(*mm);
		bcc	BADD8			# if(Carry) return(0):
		addqd	4,r2			# mm++;
		acbd	-1,r5,BADD5		# if(!--ml) goto BADD5;
BADD9:		movqd	1,r0			# return(1);
		exit	[r3,r4,r5]
		ret	0
BADD8:		movqd	0,r0			# return(0);
		exit	[r3,r4,r5]
		ret	0

		.globl _BnnSubtractBorrow
		.align 2
_BnnSubtractBorrow:		.set	BSBnn,4
				.set	BSBnl,8
				.set	BSBcar,12
		cmpqd	1,BSBcar(sp)
		beq	BSB3			# if(car == 1) return(1);
		movd	BSBnl(sp),r0
		cmpqd	0,r0
		beq	BSB2			# if(nl == 0) return(0);
		movd	BSBnn(sp),r1
BSB1:		addqd	-1,0(r1)		# (*nn)--;
		bcs	BSB3			# if(Carry) return(1);
		addqd	4,r1			# nn++;
		acbd	-1,r0,BSB1		# if(!--nl) goto BSB1;
BSB2:		ret	0			# return(nl);
BSB3:		movqd	1,r0			# return(1);
		ret	0


		.globl _BnnSubtract
		.align 2
_BnnSubtract:			.set	BSmm,8
				.set	BSml,12
				.set	BSnn,16
				.set	BSnl,20
				.set	BScar,24
		enter	[r3,r4,r5,r6],0
		movd	BSmm(fp),r4
		movd	BSml(fp),r6
		movd	BSnn(fp),r3
		movd	BSnl(fp),r5
		movd	BScar(fp),r1
		subd	r5,r6			# ml -= nl;
BS1:		cmpqd	0,r5
		beq	BS4			# if (nl == 0) goto BS4;
		addqd	-1,r5			# nl--;
		addd	0(r4),r1		# car += *mm;
		bcc	BS2			# if(!Carry) goto BS2;
		comd	0(r3),0(r4)		# *mm = ~*nn
		addqd	4,r3			# nn++
		addqd	4,r4			# mm++
		movqd	1,r1			# car = 1;
		br	BS1			# goto BS1;
BS2:		comd	0(r3),r0		# save = *nn;
		addqd	4,r3			# nn++;
		addd	r0,r1			# car += save;
		movd	r1,0(r4)		# *mm = car;
		addqd	4,r4			# mm++;
		cmpd	r1,r0
		slod	r1			# car = (car < save) ? 1 : 0;
		br	BS1			# goto BS1;

BS4:		cmpqd	1,r1
		beq	BS8			# if(car == 1) return(1);
		cmpqd	0,r6
		beq	BS9			# if(ml != 0) return(0);
BS5:		addqd	-1,0(r4)		# (*mm)--;
		bcs	BS8			# if(Carry) return(1);
		addqd	4,r4			# mm++;
		acbd	-1,r6,BS5		# if(!--ml) goto BS5;
BS9:		movqd	0,r0			# return(0);
		exit	[r3,r4,r5,r6]
		ret	0
BS8:		movqd	1,r0			# return(1);
		exit	[r3,r4,r5,r6]
		ret	0

		.globl _BnnMultiplyDigit
		.align 2
_BnnMultiplyDigit:		.set	BMDpp,8
				.set	BMDpl,12
				.set	BMDmm,16
				.set	BMDml,20
				.set	BMDd,24
		enter	[r3,r4,r5,r6,r7],0
		movd	BMDd(fp),r0
		cmpqd	0,r0
		beq	BMD10			# if(d == 0) return(0);
		cmpqd	1,r0
		bne	BMD1			# if(d != 1) goto BMD1;
		exit	[r3,r4,r5,r6,r7]
		movqd	0,20(sp)
		br	_BnnAdd			# BnAdd(pp,pl,mm,ml,0);
BMD1:		movqd	0,r7			# c = 0;
		movd	BMDpp(fp),r4
		movd	BMDml(fp),r5
		movd	BMDpl(fp),r6
		subd	r5,r6			# pl -= ml;
		cmpqd	0,r5
		beq	BMD7			# if(ml == 0) goto BMD7;
		movd	BMDmm(fp),r1
BMD2:		movd	0(r1),r2		# save = *mm;
		addqd	4,r1			# mm++;
		meid	r0,r2			# X = d * save;
		addd	r7,r2			# X += c;
		bcc	BMD3			# if(Carry) XH++;
		addqd	1,r3
BMD3:		addd	r2,0(r4)		# *pp += XL;
		bcc	BMD4			# if(Carry) XH++;
		addqd	1,r3
BMD4:		addqd	4,r4			# pp++;
		movd	r3,r7			# c = XL;
		acbd	-1,r5,BMD2		# if(!--ml) goto BMD2;
BMD7:		addd	r7,0(r4)		# *pp += c;
		bcc	BMD10			# if(!Carry) return(0);
		addqd	4,r4			# pp++;
		addqd	-1,r6			# pl--;
		cmpqd	0,r6
		beq	BMD11			# if (pl == 0) goto BMD11;
BMD8:		addqd	1,0(r4)			# ++(*p);
		bcc	BMD10			# if(!Carry) return(0);
		addqd	4,r4			# pp++;
		acbd	-1,r6,BMD8		# if(!--pl) goto BMD8;
BMD11:		movqd	1,r0			# return(1);
		exit	[r3,r4,r5,r6,r7]
		ret	0
BMD10:		movqd	0,r0			# return(0);
		exit	[r3,r4,r5,r6,r7]
		ret	0

		.globl _BnnDivideDigit
		.align 2
_BnnDivideDigit:		.set	BDDqq,8
				.set	BDDnn,12
				.set	BDDnl,16
				.set	BDDd,20
		enter	[r3,r4,r5],0
		movd	BDDd(fp),r2
		movd	BDDnl(fp),r3
		addr	0(BDDnn(fp))[r3:d],r4	# nn = &nn[nl];
		addqd	-1,r3			# nl--;
		addr	0(BDDqq(fp))[r3:d],r5	# qq = &qq[nl];
		addqd	-4,r4			# nn--;
		movd	0(r4),r1		# Xhig = *nn;
		cmpqd	0,r3
		beq	BDD2			# if(nl == 0) return(Xhig);
BDD1:		addqd	-4,r4			# --nn;
		addqd	-4,r5			# --qq;
		movd	0(r4),r0		# Xlow = *nn;
		deid	r2,r0			# Xlow = X % c;
						# Xhig = X / c;
		movd	r1,0(r5)		# *qq = Xhig;
		movd	r0,r1			# Xhig = Xlow;
		acbd	-1,r3,BDD1		# if(!--nl) goto BDD1;
		exit	[r3,r4,r5]		# return(Xlow);
		ret	0
BDD2:		movd	r1,r0			# return(Xlow);
		exit	[r3,r4,r5]
		ret	0
