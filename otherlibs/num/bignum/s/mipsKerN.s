 # Copyright     Digital Equipment Corporation & INRIA     1988, 1989
 # Last modified_on Tue Jul 31 17:48:45 GMT+2:00 1990 by shand
 #      modified_on Fri Mar  2 16:53:50 GMT+1:00 1990 by herve
 #
 #		KerN for Mips
 #		Paul Zimmermann & Robert Ehrlich & Bernard Paul Serpette
 #		& Mark Shand
 #
	.text	
	.align	2
	.globl	BnnSetToZero
	.ent	BnnSetToZero			# (nn nl)
BnnSetToZero:
	.frame	$sp, 0, $31
		sll	$9,$5,2			# nl <<= 2;
		beq	$5,$0,BSTZ2		# if(nl == 0) goto BSTZ2;
		andi	$8,$9,0x1c
		lw	$10,BSTZTable($8)
		addu	$9,$4			# nl += nn;
		addu	$4,$8
		j	$10
BSTZE8:
BSTZLoop:	addu	$4,32			# nn++;
		sw	$0,-32($4)		# *nn = 0;
BSTZE7:		sw	$0,-28($4)
BSTZE6:		sw	$0,-24($4)
BSTZE5:		sw	$0,-20($4)
BSTZE4:		sw	$0,-16($4)		# *nn = 0;
BSTZE3:		sw	$0,-12($4)
BSTZE2:		sw	$0,-8($4)
BSTZE1:		sw	$0,-4($4)
		bne	$4,$9,BSTZLoop		# if(nn != nl) goto BSTZLoop;
BSTZ2:		j	$31			# return;
		.rdata
BSTZTable:
		.word	BSTZE8
		.word	BSTZE1
		.word	BSTZE2
		.word	BSTZE3
		.word	BSTZE4
		.word	BSTZE5
		.word	BSTZE6
		.word	BSTZE7
		.text
	.end	BnnSetToZero

	.align	2
	.globl	BnnAssign
	.ent	BnnAssign			# (mm nn nl)
BnnAssign:
	.frame	$sp, 0, $31
		ble	$4,$5,BAG2		# if(mm <= nn) goto BAG2;
		sll	$12,$6,2		# X = nl << 2;
		addu	$4,$12			# mm += X;
		addu	$5,$12			# nn += X;
		b	BAG4			# goto BAG4;
BAG1:		lw	$12,($5)		# X = *(nn);
		sw	$12,($4)		# *(mm) = X
		addu	$4,4			# mm++;
		addu	$5,4			# nn++;
		subu	$6,1			# nl--;
BAG2:		bnez	$6,BAG1			# if(nl) goto BAG1;
		j	$31			# return;
BAG3:		subu	$4,4			# mm--;
		subu	$5,4			# nn--;
		lw	$12,($5)		# X = *(nn);
		sw	$12,($4)		# *(mm) = X;
		subu	$6,1			# nl--;
BAG4:		bnez	$6,BAG3			# if(nl) goto BAG3;
		j	$31			# return;
	.end	BnAssign

	.align	2
	.globl	BnnSetDigit
	.ent	BnnSetDigit			# (nn d)
BnnSetDigit:
		sw	$5,0($4)		# *nn = d;
		j	$31			# return;
	.end	BnnSetDigit

	.align	2
	.globl	BnnGetDigit
	.ent	BnnGetDigit			# (nn)
BnnGetDigit:
		lw	$2,0($4)		# return(*nn);
		j	$31
	.end	BnnGetDigit

	.align	2
	.globl	BnnNumDigits
	.ent	BnnNumDigits			# (nn nl)
BnnNumDigits:
	.frame	$sp, 0, $31
		sll	$12,$5,2
		addu	$4,$12			# nn = &nn[nl];
		b	BND2			# goto BND2;
BND1:		subu	$5,1			# nl--;
		subu	$4,4			# nn--;
		lw	$12,0($4)		# X = *nn;
		bnez	$12,BND3		# if(X) goto BND3;
BND2:		bnez	$5,BND1			# if(nl) goto BND1;
		li	$2,1			# return(1);
		j	$31
BND3:		addu	$2,$5,1			# return(nl);
		j	$31
	.end	BnnNumDigits

	.align	2
	.globl	BnnNumLeadingZeroBitsInDigit
	.ent	BnnNumLeadingZeroBitsInDigit	# (d)
BnnNumLeadingZeroBitsInDigit:
	.frame	$sp, 0, $31
		move	$2,$0			# p = 0;
		bne	$4,0,BLZ2		# if(!d) goto BLZ2;
		li	$2,32			# return(32);
		j	$31
BLZ1:		addu	$2,1			# p++;
		sll	$4,1			# d <<= 1;
BLZ2:		bgtz	$4,BLZ1			# while (d>0) goto BLZ1
		j	$31			# return(p);
	.end	BnnNumLeadingZeroBitsInDigit

	.align	2
	.globl	BnnDoesDigitFitInWord
	.ent	BnnDoesDigitFitInWord		# (d)
BnnDoesDigitFitInWord:
	.frame	$sp, 0, $31
		li	$2,1			# return(1);
		j	$31
	.end	BnnDoesDigitFitInWord

	.align	2
	.globl	BnnIsDigitZero
	.ent	BnnIsDigitZero			# (d)
BnnIsDigitZero:
	.frame	$sp, 0, $31
		seq	$2,$4,0			# return(d == 0);
		j	$31
	.end	BnnIsDigitZero

	.align	2
	.globl	BnnIsDigitNormalized
	.ent	BnnIsDigitNormalized		# (d)
BnnIsDigitNormalized:
	.frame	$sp, 0, $31
		slt	$2,$4,$0		# return(d < 0);
		j	$31
	.end	BnnIsDigitNormalized

	.align	2
	.globl	BnnIsDigitOdd
	.ent	BnnIsDigitOdd			# (d)
BnnIsDigitOdd:
	.frame	$sp, 0, $31
		and	$2,$4,1			# return(d & 1);
		j	$31
	.end	BnnIsDigitOdd

	.align	2
	.globl	BnnCompareDigits
	.ent	BnnCompareDigits		# (d1 d2)
BnnCompareDigits:
	.frame	$sp, 0, $31
 # 254	    return ((d1 > d2) - (d1 < d2));
		sltu	$8,$5,$4		# t0 = (d2 < d1);
		sltu	$9,$4,$5		# t1 = (d1 < d2);
		sub	$2,$8,$9		# return t0-t1;
		j	$31
	.end	BnnCompareDigits

	.align	2
	.globl	BnnComplement
	.ent	BnnComplement			# (nn nl)
BnnComplement:
	.frame	$sp, 0, $31
		sll	$8,$5,2			# bytes = nl*4;
		beq	$5,$0,BCM2		# if(nl == 0) goto BCM2;
		add	$8,$4			# lim = nn+bytes;
BCM1:
		lw	$14,0($4)		# X = *nn;
		nor	$14,$0			# X ^= -1;
		sw	$14,0($4)		# *nn = X
		addu	$4,4			# nn++;
		bne	$8,$4,BCM1		# if(nl != 0) goto BCM1;
BCM2:		j	$31			# return;
	.end	BnnComplement

	.align	2
	.globl	BnnAndDigits
	.ent	BnnAndDigits			# (nn d)
BnnAndDigits:
	.frame	$sp, 0, $31
		lw	$14,0($4)		# X = *nn;
		and	$14,$5			# X &= d;
		sw	$14,0($4)		# *nn = X;
		j	$31			# return;
	.end	BnnAndDigits

	.align	2
	.globl	BnnOrDigits
	.ent	BnnOrDigits			# (nn d)
BnnOrDigits:
	.frame	$sp, 0, $31
		lw	$14,0($4)		# X = *nn;
		or	$14,$5			# X |= d;
		sw	$14,0($4)		# *nn = X;
		j	$31			# return;
	.end	BnnOrDigits

	.align	2
	.globl	BnnXorDigits
	.ent	BnnXorDigits			# (nn d)
BnnXorDigits:
	.frame	$sp, 0, $31
		lw	$14,0($4)		# X = *nn;
		xor	$14,$5			# X ^= d;
		sw	$14,0($4)		# *nn = X;
		j	$31			# return;
	.end	BnnXorDigits

	.align	2
	.globl	BnnShiftLeft
	.ent	BnnShiftLeft			# (mm ml nbi)
BnnShiftLeft:
	.frame	$sp, 0, $31
		move	$2,$0			# res = 0;
		beq	$6,0,BSL2		# if(nbi == 0) goto BSL2;
		li	$14,32			# rnbi = 32;
		subu	$14,$6			# rnbi -= nbi;
		beq	$5,0,BSL2		# if(ml == 0) goto BSL2;
		sll	$15,$5,2		# bytes = 4*ml;
		addu	$15,$4			# lim = mm+size;
BSL1:
		lw	$25,0($4)		# save = *mm;
		sll	$24,$25,$6		# X = save << nbi;
		or	$24,$2			# X |= res;
		sw	$24,0($4)		# *mm = X;
		addu	$4,4			# mm++;
		srl	$2,$25,$14		# res = save >> rnbi;
		bne	$4,$15,BSL1		# if(mm != lim) goto BSL1;
BSL2:		j	$31			# return(res);
	.end	BnnShiftLeft

	.align	2
	.globl	BnnShiftRight
	.ent	BnnShiftRight			# (mm ml nbi)
BnnShiftRight:
	.frame	$sp, 0, $31
		move	$2,$0			# res = 0;
		beq	$6,0,BSR2		# if(nbi == 0) goto BSR2;
		sll	$14,$5,2		# bytes = ml*4;
		beq	$5,0,BSR2		# if(ml == 0) goto BSR2
		addu	$15,$4,$14		# lim = mm; mm += bytes;
		li	$14,32			# lnbi = 32;
		subu	$14,$6			# lnbi -= nbi;
BSR1:
		subu	$15,4			# mm--;
		lw	$25,0($15)		# save = *mm;
		srl	$24,$25,$6		# X = save >> nbi;
		or	$24,$2			# X |= res
		sw	$24,0($15)		# *mm = X;
		sll	$2,$25,$14		# res = save << lnbi;
		bne	$15,$4,BSR1		# if(mm != lim) goto BSR1;
BSR2:		j	$31			# return(res);
	.end	BnnShiftRight

	.align	2
	.globl	BnnAddCarry
	.ent	BnnAddCarry			# (nn nl car)
BnnAddCarry:
	.frame	$sp, 0, $31
		beq	$6,0,BAC3		# if(car == 0) return(0);
		beq	$5,0,BAC2		# if(nl == 0) return(1);
BAC1:		subu	$5,1			# nl--;
		lw	$9,0($4)		# X = *nn;
		addu	$9,1			# X++;
		sw	$9,0($4)		# *nn = X;
		addu	$4,4			# nn++;
		bne	$9,$0,BAC3		# if(X) goto BAC3;
		bne	$5,$0,BAC1		# if(nl) goto BAC1;
BAC2:		li	$2,1			# return(1);
		j	$31
BAC3:		li	$2,0			# return(0);
		j	$31
	.end	BnnAddCarry

	.align	2
	.globl	BnnAdd
	.ent	BnnAdd				# (mm ml nn nl car)
BnnAdd:
	.frame	$sp, 0, $31
		lw	$2, 16($sp)		# c = carryin;
		subu	$5,$7			# ml -= nl;
		bne	$7,$0,BADD1		# if(nl) goto BADD1;
		bne	$2,$0,BADD2		# if(c) goto BADD2;
BADD0:		j	$31			# return(c)
BADD1a:		# carry, save == 0
		#  hence (*nn == 0 && carry == 0) || (*nn == -1 && carry == 1)
		# in either case, *mm++ += 0; carry is preserved
		addu	$4,4			# mm++;
		beq	$7,$0,BADD2
BADD1:		subu	$7,1			# nl--;
		lw	$15,0($6)		# save = *nn;
		addu	$6,4			# nn++;
		addu	$15,$2			# save += c;
		beq	$15,$0,BADD1a		# if (save == 0);
		# no carry
		lw	$10,0($4)		# X = *mm;
		addu	$4,4			# mm++;
		addu	$10,$15			# X += save;
		sw	$10,-4($4)		# mm[-1] = X
		sltu	$2,$10,$15		# c = (X < save);
		bne	$7,$0,BADD1		# if(nl) goto BADD1;

BADD2:		beq	$5,0,BADD0		# if(ml == 0) return(c);
		beq	$2,0,BADD0		# if(c == 0) return(0);
BADD3:		subu	$5,1			# ml--;
		lw	$9,0($4)		# X = *mm;
		addu	$9,1			# X++;
		sw	$9,0($4)		# *mm = X;
		addu	$4,4			# mm++;
		bne	$9,$0,BADD4		# if(X) return(0);
		bne	$5,$0,BADD3		# if(ml) goto BADD3;
		j	$31			# return(1);
BADD4:		move	$2,$0			# return(0)
		j	$31
	.end	BnnAdd

	.align	2
	.globl	BnnSubtractBorrow
	.ent	BnnSubtractBorrow		# (nn nl car)
BnnSubtractBorrow:
	.frame	$sp, 0, $31
		bne	$6,0,BSB3		# if(car) return(1);
		beq	$5,0,BSB2		# if(nl == 0) return(0);
BSB1:		subu	$5,1			# nl--;
		lw	$9,0($4)		# X = *nn;
		subu	$10,$9,1		# Y = X - 1;
		sw	$10,0($4)		# *nn = Y;
		addu	$4,4			# nn++;
		bne	$9,$0,BSB3		# if(X) return(1);
		bne	$5,$0,BSB1		# if(nl) goto BSB1;
BSB2:		li	$2,0			# return(0);
		j	$31
BSB3:		li	$2,1			# return(1);
		j	$31
	.end	BnnSubtractBorrow

	.align	2
	.globl	BnnSubtract
	.ent	BnnSubtract 2			# (mm ml nn nl car)
BnnSubtract:
	.frame	$sp, 0, $31
		subu	$5,$7			# ml -= nl;
		lw	$2, 16($sp)		# car;
		xor	$14,$2,1		# c = !car
		bne	$7,$0,BS1		# if(nl) goto BS1;
		bne	$2,$0,BS0		# if(!c) goto BS0
		bne	$5,$0,BSB1		# if (ml != 0) goto Borrow
BS0:		j	$31			# $r2 == 1; return(1)
BS1a:		# sub == 0
		#  hence (*nn == 0 && carry == 0) || (*nn == -1 && carry == 1)
		# in either case, *mm++ -= 0; carry is preserved
		addu	$4,4
		beq	$7,$0,BS2
BS1:		subu	$7,1			# nl--;
		lw	$12,0($6)		# sub = *nn;
		addu	$6,4			# nn++;
		addu	$12,$14			# sub += c;
		beq	$12,$0,BS1a		# if(sub == 0) goto BS1a
		lw	$15,0($4)		# X = *mm
		addu	$4,4			# mm++;
		subu	$10,$15,$12		# Y = X-sub (sub != 0)
		sw	$10,-4($4)		# *mm = Y
		sltu	$14,$15,$10		# c = (Y > X) (note: X != Y)
		bne	$7,$0,BS1		# if(nl) goto BS1;

BS2:		beq	$14,$0,BS3		# if (!c) return (!c)
		bne	$5,$0,BSB1		# if (ml != 0) goto Borrow
BS3:		xor	$2,$14,1		# return(!c);
		j	$31
	.end	BnnSubtract

	.align	2
	.globl	BnnMultiplyDigit
	.ent	BnnMultiplyDigit		# (pp pl mm ml d)
BnnMultiplyDigit:
	.frame	$sp, 0, $31
		lw	$8, 16($sp)		# d;
		move	$9,$0			# low = 0;
		li	$2,1			# load 1 for comparison
		beq	$8,0,BMD7		# if(d == 0) return(0);
		move	$10,$0			# carry1 = 0
		bne	$8,$2,BMDFastLinkage	# if(d!=1)goto BMDFastLinkage;
		sw	$0, 16($sp)
		b	BnnAdd			# BnnAdd(pp, pl, mm, ml, 0);

		# FastLinkage entry point takes 5th parameter in r8
		# and two extra parameters in r9,r10 which must add to
		# less than 2^32 and that are added to pp[0]
		# used from BnnMultiply squaring code.
BMDFastLinkage:
		subu	$5,$7			# pl -= ml;
		move	$11,$0			# inc = 0
		# move	$15,$0	save = 0;	 logically needed, but use is
		#				 such that we can optimize out
		beq	$7,$0,BMD6		# if(ml==0) goto BMD6;
		sll	$7,$7,2			# ml *= 4;
		addu	$7,$7,$6		# ml = &mm[ml]
BMD3:		lw	$13,0($6)		# X = *mm;
		addu	$6,4			# mm++;
		multu	$13,$8			# HI-LO = X * d;
		sltu	$12,$15,$11		# carry2 = (save < inc)
		lw	$15,0($4)		# save = *pp;
		addu	$9,$10			# low += carry1;
		addu	$9,$12			# low += carry2;
		addu	$15,$15,$9		# save = save + low;
		sltu	$10,$15,$9		# carry1 = (save < low)
		addu	$4,4			# pp++;
		mflo	$11			# inc = LO;
		mfhi	$9			# low = HI;
		addu	$15,$11			# save += inc;
		sw	$15,-4($4)		# *pp = save;
		bne	$7,$6,BMD3		# if(mm != ml) goto BMD3;
BMD6:		sltu	$12,$15,$11		# carry2 = (save < inc)
		lw	$15,($4)		# save = *pp;
		addu	$9,$10			# low += carry1;
		addu	$9,$12			# low += carry2;
		addu	$9,$15			# low += save;
		sw	$9,0($4)		# *pp = low;
		addu	$4,4			# pp++;
		bltu	$9,$15,BMD8		# if(low < save) goto BMD8;
BMD7:		move	$2, $0			# return(0);
		j	$31
BMD8:		subu	$5,1			# pl--;
		beq	$5,0,BMD10		# if(ml == 0) return(1);
BMD9:		subu	$5,1			# pl--;
		lw	$9,0($4)		# X = *pp;
		addu	$9,1			# X++;
		sw	$9,0($4)		# *pp = X;
		addu	$4,4			# pp++;
		bne	$9,$0,BMD7		# if(X) return(0);
		bne	$5,$0,BMD9		# if(pl) goto BMD9;
BMD10:		li	$2,1			# return(1);
		j	$31
	.end	BnnMultiplyDigit

	.align	2
	.globl	BnnDivideDigit
	.ent	BnnDivideDigit			# (qq nn nl d)
BnnDivideDigit:
	.frame	$sp, 0, $31
		move	$11,$31
		move	$10,$4
		move	$9,$5
		move	$8,$6
		move	$4,$7		# k = BnnNumLeadingZeroBitsInDigit(d);
		jal	BnnNumLeadingZeroBitsInDigit
		move	$6,$2
		beq	$6,$0,BDD1		# if(k == 0) goto BDD1;
		move	$4,$9
		move	$5,$8
		jal	BnnShiftLeft		# BnnShiftLeft(nn, nl, k);
		lw	$31,0($10)		# first_qq = *qq;
		move	$5,$8			# o_nl = nl;
		sll	$7,$6			# d <<= k;
BDD1:		sll	$3,$8,2
		addu	$9,$3			# nn = &nn[nl];
		subu	$8,1			# nl--;
		subu	$3,4
		addu	$10,$3			# qq = &qq[nl];
		srl	$25,$7,16		# ch = HIGH(d);
		and	$2,$7,65535		# cl = LOW(d);
		subu	$9,4			# nn--;
		lw	$13,0($9)		# rl = *nn;
		beq	$8,0,BDDx		# if(nl == 0) goto BDDx;
BDD2:		subu	$8,1			# nl--;
		move	$12,$13			# rh = rl;
		subu	$9,4			# nn--;
		lw	$13,0($9)		# rl = *nn;
		divu	$14,$12,$25		# qa = rh/ch;
		multu	$2,$14			# HI-LO = cl * qa;
		mflo	$24			# pl = LO;
		multu	$25,$14			# HI-LO = ch * qa;
		mflo	$15			# ph = LO;
		srl	$3,$24,16		# X = HIGH(pl);
		addu	$15,$3			# ph += X;
		sll	$24,16			# pl = L2H(pl);
		bgtu	$15,$12,BDD84		# if(ph > rh) goto BDD84;
		bne	$15,$12,BDD88		# if(ph != rh) goto BDD88;
		bleu	$24,$13,BDD88		# if(pl <= rl) goto BDD88;
BDD84:		sll	$3,$2,16		# X = L2H(cl);
BDD85:		subu	$14,1			# qa--;
		bleu	$3,$24,BDD86		# if(X <= pl) goto BDD86;
		subu	$15,1			# ph--;
BDD86:		subu	$24,$3			# pl -= X;
		subu	$15,$25			# ph -= ch;
		bgtu	$15,$12,BDD85		# if(ph > rh) goto BDD85;
		bne	$15,$12,BDD88		# if(ph != rh) goto BDD88;
		bgtu	$24,$13,BDD85		# if(pl > rl) goto BDD85;
BDD88:		bleu	$24,$13,BDD89		# if(pl <= rl) goto BDD89;
		subu	$12,1			# rh--;
BDD89:		subu	$13,$24			# rl -= pl;
		subu	$12,$15			# rh -= ph;
		subu	$10,4			# qq--;
		sll	$3,$14,16		# X = L2H(qa);
		sw	$3,0($10)		# *qq = X;
		sll	$3,$12,16		# X = L2H(rh);
		srl	$14,$13,16		# qa = HIGH(rl);
		or	$14,$3			# qa |= X;
		divu	$14,$14,$25		# qa /= ch;
		multu	$2,$14			# HI-LO = cl * qa;
		mflo	$24			# pl = LO;
		multu	$25,$14			# HI-LO = ch * qa;
		mflo	$15			# ph = LO;
		srl	$3,$24,16		# X = HIGH(pl);
		addu	$15,$3			# ph += X;
		and	$24,65535		# pl = LOW(pl);
		and	$3,$15,65535		# X = LOW(ph);
		sll	$3,16			# X = L2H(X)
		or	$24,$3			# pl |= X;
		srl	$15,16			# ph = HIGH(ph);
		bgtu	$15,$12,BDD41		# if(ph > rh) goto BDD841;
		bne	$15,$12,BDD44		# if(ph != rh) goto BDD44;
		bleu	$24,$13,BDD44		# if(pl <= rl) goto BDD44;
BDD41:		subu	$14,1			# qa--;
		bleu	$7,$24,BDD42		# if(d <= pl) goto BDD42;
		subu	$15,1			# ph--;
BDD42:		subu	$24,$7			# pl -= d;
		bgtu	$15,$12,BDD41		# if(ph > rh) goto BDD841;
		bne	$15,$12,BDD44		# if(ph == rh) goto BDD44;
		bgtu	$24,$13,BDD41		# if(pl > rl) goto BDD41;
BDD44:		subu	$13,$24			# rl -= pl;
		lw	$3,0($10)		# X = *qq;
		or	$3,$14			# X |= qa
		sw	$3,0($10)		# *qq = X;
		bne	$8,0,BDD2
BDDx:		beq	$6,0,BDD46		# if(k = 0) goto BDD46;
		bleu	$10,$9,BDD45		# if(qq < nn) goto BDD45;
		sll	$3,$5,2
		addu	$3,$9			# X = &nn[o_nl];
		bleu	$3,$10,BDD45		# if(X <= qq) goto BDD45;
		subu	$5,$10,$9		# o_nl = qq - nn;
		srl	$5,2			# o_nl >>= 2;
		lw	$8,0($10)		# X = *qq;
		sw	$31,0($10)		# *qq = first_qq;
		addu	$5,1			# o_nl++;
		move	$4,$9			# BnnShiftRight(nn, o_nl, k);
		jal	BnnShiftRight
		sw	$8,0($10)		# X = *qq;
		srl	$2,$13,$6		# return(rl >> k);
		j	$11
BDD45:		bne	$10,$9,BDD451		# if(qq == nn) goto BDD451;
		subu	$5,1			# o_nl--;
		sll	$5,2
		addu	$9,$5			# nn = &nn[o_nl];
		li	$5,1			# o_nl = 1;
BDD451:		move	$4,$9			# BnnShiftRight(nn, o_nl, k);
		jal	BnnShiftRight
BDD46:		srl	$2,$13,$6		# return(rl >> k);
		j	$11
	.end	BnnDivideDigit

 #############################################################################
 # Karatsuba Multiplication for Mips.
 # Mark Shand & Jean Vuillemin, May 1989.
 #
 # Basic operation is to compute: (a1.B + a0) * (b1.B + b0)
 #                                B is the base; a1,a0,b1,b0 <= B-1
 # We compute PL = a0.b0
 #            PM = (a1-a0).(b0-b1)
 #            PH = a1.b1
 # Then:
 #            (a1.B + a0) * (b1.B + b0) = PL + B.(PM+PL+PH) + B.B.PH
 #
 # Overall operation is BigNum mm * d0_d1.
 # Each cycle computes m0_m1 * d0_d1
 # to avoid underflow in (a1-a0) and (b0-b1) and the
 # extra adds that it would entail, the main loop is
 # broken into four variants:
 #    BM2DLLoop     d0 >= d1, m0 <= m1
 #    BM2DNLLoop    d0 >= d1, m0 > m1
 #    BM2DHLoop     d0 < d1, m0 >= m1
 #    BM2DNHLoop    d0 < d1, m0 < m1
 # mm is assumed to be even length.
 #
 # The code within the loops is written on the assumption of an
 # infinite supply of registers.  Each name is used in a single
 # assignment.  Name are then assigned to the finite set of registers
 # based on an analysis of lifetime of each name--this is the purpose
 # of the "defines" at the start of the routine.
 
	.align	2
	.globl	BnnMultiply2Digit
	.globl	BnnM2DFastLink
#define	c0 $2		/* low carry */
#define	 tb1 $2
#define	 tc1 $2
#define	 tj1 $2
#define	 tn1 $2
#define	 tq1 $2
#define	 tz1 $2
#define	 tA2 $2
#define	c1 $3		/* high carry */
#define	 th2 $3
#define	 ti2 $3
#define	 pH3 $3
#define	 tx3 $3
#define	 ty3 $3
#define	ss $4
#define	sl $5
#define	mm $6
#define	ml $7
#define	mlim $7
#define	d0 $8
#define	d1 $9
#define	ds $10		/* d0+d1 mod base */
#define	t_z $11
#define	 tC3 $11
#define	 s0 $11
#define	 ta0 $11
#define	 td0 $11
#define	 te1 $11
#define	 tf1 $11
#define	 s1 $11
#define	 to2 $11
#define	 tp2 $11
#define	 ts2 $11
#define	 pM1 $11
#define	m0 $12
#define	 ms $12		/* b0+b1 mod base */
#define	 tr2 $12 
#define	 tu3 $12
#define	 tv3 $12
#define	pL0 $13
#define	 tg1 $13
#define	 tk2 $13
#define	 tm2 $13
#define	 tt2 $13
#define	 tw2 $13
#define	t_1 $14
#define	 pL1 $14
#define	 pH2 $14
#define	 pM2 $14
#define	 tB2 $14
#define	m1 $15
#define	 borrow $15
 # Special "friends" entry point--allows fast non-standard procedure linkage.
 # Permits passing d0:d1 in r8-r9 and a low-order 64-bit integer in r2-r3
 # that is added to final result.
 # Used from BnnMultiply and most highly optimized version of PRL's RSA
 # implemenatation.
	.ent	BnnM2DFastLink
BnnM2DFastLink:
	.frame	$sp, 0, $31
	subu	sl,ml
	blez	ml,BM2D6
	lw	m0,0(mm)
	b	BnnM2DFLAux
	.end	BnnM2DFastLink			# (ss sl mm ml d0, d1)

	.ent	BnnMultiply2Digit		# (ss sl mm ml d0, d1)
BnnMultiply2Digit:
	.frame	$sp, 0, $31

	.set	noreorder
		lw	d0, 16($sp)		# d0;
		lw	d1, 20($sp)		# d1;
		li	c0,0
		li	c1,0
		blez	ml,BM2D6		# if(ml <= 0) goto end_loop;
		# BDSLOT
		subu	t_1,d0,1		# t_1 = d0-1
	.set	reorder
		or	t_z,d0,d1		# t_z = (d0 | d1)
		beq	t_z,0,BM2D7		# if(d0.d1 == 0)
						#	return(0);
		lw	m0,0(mm)
		or	t_1,d1		# t_1 = (d0-1)|d1
		subu	sl,ml			# sl -= ml;
		beq	t_1,0,BM2DADD0		# if(d0.d1 != 1)
						#     BnnAdd(pp, pl, mm, ml, 0);
	.set	noreorder
BnnM2DFLAux:
		 multu	d0,m0
#define EnableOddLength 1
#ifdef EnableOddLength
#define	t_odd $15
#define	t_a $15
#define	t_b $14
#define	t_c $15
#define	t_d $15
#define	t_e $14
#define	t_f $13
#define	t_g $15
		# the ifdef'ed code handles case when length of mm is odd.
		and	t_odd,ml,1
		sll	mlim,ml,2		# ml *= 4;
		beq	t_odd,$0,BM2DmlEven
		addu	mlim,mlim,mm		# mlim = mm+ml;
		lw	s0,0(ss)
		addu	mm,4
		addu	ss,4
		mflo	t_a
		mfhi	t_b
		addu	s0,t_a,s0
		sltu	t_c,s0,t_a
		multu	d1,m0
		lw	m0,0(mm)
		addu	t_d,t_c,t_b
		mflo	t_e
		mfhi	t_f
		addu	c0,t_e,t_d
		sltu	t_g,c0,t_e
		multu	d0,m0
		addu	c1,t_g,t_f
		beq	mm,mlim,BM2D6
		# BDSLOT
		sw	s0,-4(ss)

BM2DmlEven:
#else EnableOddLength
		sll	mlim,ml,2		# ml *= 4;
		addu	mlim,mlim,mm		# mlim = mm+ml;
#endif EnableOddLength
		lw	m1,4(mm)		# ml *= 4;
		bltu	d0,d1,BM2DHighBig	# expands to 2 instructions
		# BDSLOT
		nop
		bltu	m1,m0,BM2DLNeg	# expands to 2 instructions
		# BDSLOT
		subu	ds,d0,d1
		b	BM2DLPEntry
		# BDSLOT
		lw	s0,0(ss)
		
BM2DLLoop:
		lw	m0,0(mm)
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		 multu	m0,d0
		addu	ss,8
		sltu	tA2,tz1,pM1
		addu	tB2,pM2,tA2		# tB2 = pM2 + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	c0,tB2,tw2		# c0 = pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		lw	m1,4(mm)
		sltu	tC3,c0,tB2
		bltu	m1,m0,BM2DLNeg	# expands to 2 instructions
		# BDSLOT
		addu	c1,ty3,tC3
		
BM2DLPos:
		lw	s0,0(ss)
BM2DLPEntry:
		subu	ms,m1,m0
		addu	ta0,s0,c0		# ta0 = (s0+c0)%B
		 mfhi	pL1
		 mflo	pL0
		sltu	tb1,ta0,c0
		addu	tc1,pL1,tb1		# tc1 = pL1 + (s0+c0)/B
		 multu	m1,d1
		addu	td0,pL0,ta0		# td0 = (pL0+s0+c0)%B
		sw	td0,0(ss)		#  (pL0+s0+c0)%B -> ss[0] FIN
		sltu	te1,td0,pL0
		addu	tf1,tc1,te1		# tf1 = pL1 + (pL0+s0+c0)/B
		addu	tg1,pL0,c1		# tg1 = (pL0+c1)%B
		sltu	th2,tg1,c1
		addu	ti2,pL1,th2		# ti2 = pL1 + (pL0+c1)/B
		addu	tj1,tg1,tf1		# tj1 = (pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tk2,tj1,tg1
		lw	s1,4(ss)
		addu	tm2,ti2,tk2		# tm2 = pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B
		 mfhi	pH3
		 mflo	pH2
		addu	tn1,tj1,s1		# tn1 = (s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	to2,tn1,s1
		 multu	ms,ds
		addu	tp2,pH3,to2		# tp2 = pH3 + (s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tq1,pH2,tn1		# tq1 = (pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tr2,tq1,pH2
		addu	ts2,tp2,tr2		# ts2 = pH3 + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tt2,pH2,tm2		# tt2 = (pH2+pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B
		sltu	tu3,tt2,pH2
		addu	tv3,pH3,tu3		# tv3 = pH3 + (pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	tw2,ts2,tt2		# tw2 = pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tx3,tw2,ts2
		addu	ty3,tv3,tx3		# ty3 = pH3 + (pH3+(pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	mm,8
		 mflo	pM1
		 mfhi	pM2
		bne	mlim,mm,BM2DLLoop	# if(mm!=mlim) goto BM2DLLoop;
		# BDSLOT
		addu	tz1,pM1,tq1		# tz1 = (pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		
	.set	reorder
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		sltu	tA2,tz1,pM1
		addu	tB2,pM2,tA2		# tB2 = pM2 + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	c0,tB2,tw2		# c0 = pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tC3,c0,tB2
		addu	c1,ty3,tC3
		b	BM2D6
	.set	noreorder
		
BM2DNLLoop:
		lw	m0,0(mm)
		subu	tz1,tq1,pM1		# tz1 = (-pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		 multu	m0,d0
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		addu	tB2,pM2,borrow
		sltu	tC3,tw2,tB2
		lw	m1,4(mm)
		subu	c0,tw2,tB2		# c0 = -pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		bgeu	m1,m0,BM2DLPos	# expands to 2 instructions
		# BDSLOT
		subu	c1,ty3,tC3
		
BM2DLNeg:
		lw	s0,0(ss)
		subu	ms,m0,m1
		addu	ta0,s0,c0		# ta0 = (s0+c0)%B
		 mfhi	pL1
		 mflo	pL0
		sltu	tb1,ta0,c0
		addu	tc1,pL1,tb1		# tc1 = pL1 + (s0+c0)/B
		 multu	m1,d1
		addu	td0,pL0,ta0		# td0 = (pL0+s0+c0)%B
		sw	td0,0(ss)		#  (pL0+s0+c0)%B -> ss[0] FIN
		sltu	te1,td0,pL0
		addu	tf1,tc1,te1		# tf1 = pL1 + (pL0+s0+c0)/B
		addu	tg1,pL0,c1		# tg1 = (pL0+c1)%B
		sltu	th2,tg1,c1
		addu	ti2,pL1,th2		# ti2 = pL1 + (pL0+c1)/B
		addu	tj1,tg1,tf1		# tj1 = (pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tk2,tj1,tg1
		lw	s1,4(ss)
		addu	tm2,ti2,tk2		# tm2 = pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B
		 mfhi	pH3
		 mflo	pH2
		addu	tn1,tj1,s1		# tn1 = (s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	to2,tn1,s1
		 multu	ms,ds
		addu	tp2,pH3,to2		# tp2 = pH3 + (s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tq1,pH2,tn1		# tq1 = (pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tr2,tq1,pH2
		addu	ts2,tp2,tr2		# ts2 = pH3 + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tt2,pH2,tm2		# tt2 = (pH2+pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B
		sltu	tu3,tt2,pH2
		addu	tv3,pH3,tu3		# tv3 = pH3 + (pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	tw2,ts2,tt2		# tw2 = pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tx3,tw2,ts2
		addu	ty3,tv3,tx3		# ty3 = pH3 + (pH3+(pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		# Subtract ds
		# sltu	borrow,tw2,ds
		# subu	tw2,ds
		# subu	ty3,borrow
		# End Subtract
		addu	mm,8
		 mflo	pM1
		 mfhi	pM2
		bne	mlim,mm,BM2DNLLoop	# if(mm!=mlim) goto BM2DNLLoop;
		# BDSLOT
		sltu	borrow,tq1,pM1
		
	.set	reorder
		subu	tz1,tq1,pM1		# tz1 = (-pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		addu	tB2,pM2,borrow
		sltu	tC3,tw2,tB2
		subu	c0,tw2,tB2		# c0 = -pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		subu	c1,ty3,tC3
		b	BM2D6
	.set	noreorder
BM2DHighBig:
		bltu	m0,m1,BM2DHNeg	# expands to 2 instructions
		subu	ds,d1,d0
		# BDSLOT
		b	BM2DHEntry
		# BDSLOT
		lw	s0,0(ss)
		
BM2DHLoop:
		lw	m0,0(mm)
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		 multu	m0,d0
		addu	ss,8
		sltu	tA2,tz1,pM1
		addu	tB2,pM2,tA2		# tB2 = pM2 + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	c0,tB2,tw2		# c0 = pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		lw	m1,4(mm)
		sltu	tC3,c0,tB2
		bltu	m0,m1,BM2DHNeg	# expands to 2 instructions
		# BDSLOT
		addu	c1,ty3,tC3
		
BM2DHPos:
		lw	s0,0(ss)
BM2DHEntry:
		subu	ms,m0,m1
		addu	ta0,s0,c0		# ta0 = (s0+c0)%B
		 mfhi	pL1
		 mflo	pL0
		sltu	tb1,ta0,c0
		addu	tc1,pL1,tb1		# tc1 = pL1 + (s0+c0)/B
		 multu	m1,d1
		addu	td0,pL0,ta0		# td0 = (pL0+s0+c0)%B
		sw	td0,0(ss)		#  (pL0+s0+c0)%B -> ss[0] FIN
		sltu	te1,td0,pL0
		addu	tf1,tc1,te1		# tf1 = pL1 + (pL0+s0+c0)/B
		addu	tg1,pL0,c1		# tg1 = (pL0+c1)%B
		sltu	th2,tg1,c1
		addu	ti2,pL1,th2		# ti2 = pL1 + (pL0+c1)/B
		addu	tj1,tg1,tf1		# tj1 = (pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tk2,tj1,tg1
		lw	s1,4(ss)
		addu	tm2,ti2,tk2		# tm2 = pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B
		 mfhi	pH3
		 mflo	pH2
		addu	tn1,tj1,s1		# tn1 = (s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	to2,tn1,s1
		 multu	ms,ds
		addu	tp2,pH3,to2		# tp2 = pH3 + (s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tq1,pH2,tn1		# tq1 = (pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tr2,tq1,pH2
		addu	ts2,tp2,tr2		# ts2 = pH3 + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tt2,pH2,tm2		# tt2 = (pH2+pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B
		sltu	tu3,tt2,pH2
		addu	tv3,pH3,tu3		# tv3 = pH3 + (pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	tw2,ts2,tt2		# tw2 = pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tx3,tw2,ts2
		addu	ty3,tv3,tx3		# ty3 = pH3 + (pH3+(pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	mm,8
		 mflo	pM1
		 mfhi	pM2
		bne	mlim,mm,BM2DHLoop	# if(mm!=mlim) goto BM2DHLoop;
		# BDSLOT
		addu	tz1,pM1,tq1		# tz1 = (pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		
	.set	reorder
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		sltu	tA2,tz1,pM1
		addu	tB2,pM2,tA2		# tB2 = pM2 + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	c0,tB2,tw2		# c0 = pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tC3,c0,tB2
		addu	c1,ty3,tC3
		b	BM2D6
	.set	noreorder

BM2DNHLoop:
		lw	m0,0(mm)
		subu	tz1,tq1,pM1		# tz1 = (-pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		 multu	m0,d0
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		addu	tB2,pM2,borrow
		sltu	tC3,tw2,tB2
		lw	m1,4(mm)
		subu	c0,tw2,tB2		# c0 = -pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		bgeu	m0,m1,BM2DHPos	# expands to 2 instructions
		# BDSLOT
		subu	c1,ty3,tC3
		
BM2DHNeg:
		lw	s0,0(ss)
		subu	ms,m1,m0
		addu	ta0,s0,c0		# ta0 = (s0+c0)%B
		 mfhi	pL1
		 mflo	pL0
		sltu	tb1,ta0,c0
		addu	tc1,pL1,tb1		# tc1 = pL1 + (s0+c0)/B
		 multu	m1,d1
		addu	td0,pL0,ta0		# td0 = (pL0+s0+c0)%B
		sw	td0,0(ss)		#  (pL0+s0+c0)%B -> ss[0] FIN
		sltu	te1,td0,pL0
		addu	tf1,tc1,te1		# tf1 = pL1 + (pL0+s0+c0)/B
		addu	tg1,pL0,c1		# tg1 = (pL0+c1)%B
		sltu	th2,tg1,c1
		addu	ti2,pL1,th2		# ti2 = pL1 + (pL0+c1)/B
		addu	tj1,tg1,tf1		# tj1 = (pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tk2,tj1,tg1
		lw	s1,4(ss)
		addu	tm2,ti2,tk2		# tm2 = pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B
		 mfhi	pH3
		 mflo	pH2
		addu	tn1,tj1,s1		# tn1 = (s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	to2,tn1,s1
		 multu	ms,ds
		addu	tp2,pH3,to2		# tp2 = pH3 + (s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tq1,pH2,tn1		# tq1 = (pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sltu	tr2,tq1,pH2
		addu	ts2,tp2,tr2		# ts2 = pH3 + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		addu	tt2,pH2,tm2		# tt2 = (pH2+pL1 + (pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B
		sltu	tu3,tt2,pH2
		addu	tv3,pH3,tu3		# tv3 = pH3 + (pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		addu	tw2,ts2,tt2		# tw2 = pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		sltu	tx3,tw2,ts2
		addu	ty3,tv3,tx3		# ty3 = pH3 + (pH3+(pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B)/B+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)/B
		# Subtract ds
		# sltu	borrow,tw2,ds
		# subu	tw2,ds
		# subu	ty3,borrow
		# End Subtract
		addu	mm,8
		 mflo	pM1
		 mfhi	pM2
		bne	mlim,mm,BM2DNHLoop	# if(mm!=mlim) goto BM2DHLoop;
		# BDSLOT
		sltu	borrow,tq1,pM1
		
	.set	reorder
		subu	tz1,tq1,pM1		# tz1 = (-pM1+pH2+s1+pL1+pL0+c1 + (pL0+s0+c0)/B)%B
		sw	tz1,4(ss)		#  (pM1+pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B -> ss[1] FIN
		addu	ss,8
		addu	tB2,pM2,borrow
		sltu	tC3,tw2,tB2
		subu	c0,tw2,tB2		# c0 = -pM2+pH3+(pH2+pL1+(pL1+(pL0+s0+c0)/B+pL0+c1)/B)%B + (pM1+(pH2+s1+pL1+pL0+c1+(pL0+s0+c0)/B)%B+pH2+s1+(pL1+pL0+c1+(pL0+s0+c0)/B)%B)/B
		subu	c1,ty3,tC3
		# b	BM2D6

BM2D6:		
		lw	s0,0(ss)		# s0 = *ss;
		addu	c0,s0			# c0 = (c0+s0)%B
		sltu	$12,c0,s0		# r = (c0+s0)/B
		lw	s1,4(ss)		# 
		sw	c0,0(ss)		# *ss = c0;
		addu	c1,s1			# c1 = (c1+s1)%B
		sltu	c0,c1,s1		# c0 = (c1+s1)/B
		addu	c1,$12			# c1 = (c1+s1+(c0+s0)/B)%B
		sltu	$12,c1,$12		# r = ((c1+s1)%B+(c0+s0)/B)/B
		sw	c1,4(ss)
		addu	c0,$12			# c0 = (c1+s1+(c0+s0)/B)/B
		addu	ss,8			# ss+=2;
		bne	c0,0,BM2D8		# if(c0) goto BM2D8;
BM2D7:		li	$2,0			# return(0);
		j	$31
BM2D8:		subu	$5,2			# sl-=2;
		blez	$5,BM2D10		# if(sl <= 0) return(1);
BM2D9:		subu	$5,1			# pl--;
		lw	$9,0($4)		# X = *pp;
		addu	$9,1			# X++;
		sw	$9,0($4)		# *ss = X;
		bne	$9,$0,BM2D7		# if(X) return(0);
		addu	$4,4			# ss++;
		bgtz	$5,BM2D9		# if(sl > 0) goto BM2D9;
BM2D10:		li	$2,1			# return(1);
		j	$31

 #==============================================================================

BM2DADD0:	li	c0,0
BM2DADD1:	subu	$7,1			# nl--;
		lw	$15,0($4)		# save = *mm;
		addu	$4,4			# mm++;
		addu	$15,$2			# save += c;
		sltu	$14,$15,$2		# c' = (save < c);
		lw	$10,0($6)		# X = *nn;
		addu	$6,4			# nn++;
		addu	$10,$15			# X += save;
		sw	$10,-4($4)		# mm[-1] = X
		sltu	$15,$10,$15		# save = (X < save);
		addu	$2,$15,$14		# c = c' + save;
		bne	$7,$0,BM2DADD1		# if(nl) goto BM2DADD1;

		beq	$5,0,BM2D10		# if(ml == 0) return(c);
		beq	$2,0,BM2DADD3		# if(c == 0) return(0);
BM2DADD2:	subu	$5,1			# ml--;
		lw	$9,0($4)		# X = *mm;
		addu	$9,1			# X++;
		sw	$9,0($4)		# *mm = X;
		addu	$4,4			# mm++;
		bne	$9,$0,BM2DADD3		# if(X) return(0);
		bne	$5,$0,BM2DADD2		# if(ml) goto BM2DADD2;
		j	$31			# return(1);
BM2DADD3:	move	$2,$0			# return(0)
		j	$31
#undef	c0
#undef	 tb1
#undef	 tc1
#undef	 tj1
#undef	 tn1
#undef	 tq1
#undef	 tz1
#undef	 tA2
#undef	c1
#undef	 th2
#undef	 ti2
#undef	 pH3
#undef	 tx3
#undef	 ty3
#undef	ss
#undef	sl
#undef	mm
#undef	ml
#undef	mlim
#undef	d0
#undef	d1
#undef	ds
#undef	t_z
#undef	 tC3
#undef	 s0
#undef	 ta0
#undef	 td0
#undef	 te1
#undef	 tf1
#undef	 s1
#undef	 to2
#undef	 tp2
#undef	 ts2
#undef	 pM1
#undef	m0
#undef	 ms
#undef	 tr2 
#undef	 tu3 
#undef	 tv3 
#undef	pL0 
#undef	 tg1 
#undef	 tk2 
#undef	 tm2 
#undef	 tt2 
#undef	 tw2 
#undef	t_1 
#undef	 pL1 
#undef	 pH2 
#undef	 pM2 
#undef	 tB2 
#undef	m1 
#undef	 borrow 
#ifdef EnableOddLength
#undef	t_odd 
#undef	t_a 
#undef	t_b 
#undef	t_c 
#undef	t_d 
#undef	t_e 
#undef	t_f 
#undef	t_g 
#endif EnableOddLength
	.end	BnnMultiply2Digit

	.align	2
	.globl	BnnMultiply
	#.loc	2 40
 #  40	{
	.ent	BnnMultiply 2
BnnMultiply:
	subu	$sp, 56
	sw	$31, 52($sp)
	sw	$22, 48($sp)
	sd	$20, 40($sp)
	sd	$18, 32($sp)
	sd	$16, 24($sp)
	.mask	0x807F0000, -4
	.frame	$sp, 56, $31
	move	$17, $4
	move	$18, $5
	move	$21, $6
	move	$22, $7
	lw	$16, 72($sp)
	lw	$19, 76($sp)
	#.loc	2 74
 #  74	   if (nl & 1)
	and	$14, $19, 1
	bne	$6, $16, $37
	move	$20, $0
	beq	$7, $19, $38
	#.loc	2 73
 #  73	   c = 0;
$37:
	move	$20, $0
	bne	$14, $0, $32
	b	$33
$32:
	#.loc	2 76
 #  75	   {
 #  76	      c += BnnMultiplyDigit (pp, pl, mm, ml, *nn);
	move	$4, $17
	move	$5, $18
	move	$6, $21
	move	$7, $22
	lw	$15, 0($16)
	sw	$15, 16($sp)
	jal	BnnMultiplyDigit
	move	$20, $2
	#.loc	2 77
 #  77	      pp++, nn++, nl--, pl--;
	addu	$17, $17, 4
	addu	$16, $16, 4
	addu	$19, $19, -1
	addu	$18, $18, -1
	#.loc	2 78
 #  78	   }
$33:
	#.loc	2 79
 #  79	   if ((ml & 1) && nl)
	and	$24, $22, 1
	beq	$24, $0, $34
	beq	$19, 0, $34
	#.loc	2 81
 #  80	   {
 #  81	      c += BnnMultiplyDigit (pp, pl, nn, nl, *mm);
	move	$4, $17
	move	$5, $18
	move	$6, $16
	move	$7, $19
	lw	$25, 0($21)
	sw	$25, 16($sp)
	jal	BnnMultiplyDigit
	addu	$20, $20, $2
	#.loc	2 82
 #  82	      pp++, mm++, ml--, pl--;
	addu	$17, $17, 4
	addu	$21, $21, 4
	addu	$22, $22, -1
	addu	$18, $18, -1
	#.loc	2 83
 #  83	   }
$34:
	#.loc	2 84
 #  84	   while (nl > 0)
	bleu	$19, 0, $36
$35:
	#.loc	2 86
 #  85	   {
 #  86	      c += BnnMultiply2Digit (pp, pl, mm, ml, nn[0], nn[1]);
	move	$4, $17
	move	$5, $18
	move	$6, $21
	move	$7, $22
	lw	$8, 0($16)
	lw	$9, 4($16)
	li	$2, 0
	li	$3, 0
	jal	BnnM2DFastLink
	addu	$20, $20, $2
	#.loc	2 87
 #  87	      pp += 2, nn += 2, nl -= 2, pl -= 2;
	addu	$17, $17, 8
	addu	$16, $16, 8
	addu	$19, $19, -2
	addu	$18, $18, -2
	#.loc	2 88
 #  88	   }
	#.loc	2 88
	bgtu	$19, 0, $35
$36:
	#.loc	2 89
 #  89	   return c;
	move	$2, $20
	ld	$16, 24($sp)
	ld	$18, 32($sp)
	ld	$20, 40($sp)
	lw	$22, 48($sp)
	lw	$31, 52($sp)
	addu	$sp, 56
	j	$31
$38:
	# We no longer need r21, r22 since nn == mm && nl == ml
	li	$21, 0
	beq	$14, $0, $40	# if ((nl&1) == 0) goto $40
	lw	$21, 0($16)	# r10 = d = *nn
	 multu	$21, $21	# d*d
	lw	$12, 0($17)	# r12 = *pp
	addu	$16, 4		# nn++
	addu	$8, $21, $21	# d2 = 2*d
	addu	$17, 8		# pp += 2
	mflo	$13		# d*d % 2^32
	addu	$13, $12	# r13 = new pp[0] = (*pp + d*d) % 2^32
	sltu	$10, $13, $12	# r10 = carry = (*pp + d*d) / 2^32
	mfhi	$9		# r9 = save = d*d / 2^32
	subu	$4, $17, 4	# arg1 = pp-1
	subu	$5, $18, 1	# arg2 = pl-1
	subu	$18, 2		# pl -= 2
	subu	$19, 1		# nl--
	move	$6, $16		# arg3 = nn
	move	$7, $19
	sw	$13, -8($17)
	jal	BMDFastLinkage	# BnnMultiplyDigit(r4,r5,r6,r7,r8)+(r9+r10)%2^32
	addu	$20, $2
	sra	$21,31
$40:
 #  84	   while (nl > 0)
	bleu	$19, 0, $42
$41:
 #  85	   {
 # compute d0:d1*d0:d1+p0:p1+c0:c1 -> p0:p1:c0:c1 with maximal overlap of
 # single cycle instruction with multiplier operation.
 #
 # observe a*b+c+d <= 2^64-1 for all a,b,c,d < 2^32
 #    we can exploit this property to minimize carry tests
 # Accordingly, computation can be organized as follows:
 #    d0*d0 -> l0:l1    d0*d1 -> m0:m1    d1*d1 -> h0:h1
 #
 #    c0     c1     L1     M1
 #    p0     p1     M0     N1
 #    l0:l1  m0:m1  m0:m1  h0:h1
 #    =====  =====  =====  =====
 #    L0:L1  M0:M1  N0:N1  H0:H1
 # -> P0            P1     C0:C1
 #
	lw	$8, 0($16)
	lw	$15, 4($16)
	multu	$8, $8		# d0*d0
	and	$2, $8, $21	# c0 = d0*sgn(n[-1])
	and	$3, $15, $21	# c1 = d1*sgn(n[-1])
	slt	$22, $21, $0	# r22 = n[-1] < 0
	sra	$21, $15, 31
	lw	$10, 0($17)	# r10 = p0
	lw	$11, 4($17)	# r11 = p1
	addu	$17, 16		# pp += 4
	addu	$10, $2		# r10 = L(p0+c0)
	sltu	$2, $10, $2	# r2 = H(p0+c0)
	addu	$11, $3		# r11 = L(p1+c1)
	sltu	$3, $11, $3	# r3 = H(p1+c1)
	# enough computation to prevent a stall
	mflo	$12		# l0
	mfhi	$13		# l1
	addu	$10, $12	# r10 = L0 = L(p0+c0+l0)
	sw	$10,-16($17)	# pp[-4] = L0
	multu	$8, $15		# d0*d1
	addu	$16, 8		# nn += 2
	sltu	$12, $10, $12	# r12 = H(L(p0+c0)+l0)
				# r12+r2 = H(p0+c0+l0)
	addu	$12, $13	# assert r12 == 0 || r2 == 0
	addu	$12, $2		# r12 = L1 = l1+H(p0+c0+l0)
	# Free: 2,9,10,13,14; Used: r11:r3 = p1+c1 r8=d0 r15=d1 r12=L1
	slt	$14, $8, $0	# r14 = n[0] < 0
	addu	$8, $8		# r8 = L(2*d0)
	addu	$8, $22		# r8 = L(2*d0+(n[-1] < 0))
	addu	$9, $15, $15	# r9 = L(2*d1)
	addu	$9, $14		# r9 = L(2*d1+(d0 < 0))
	subu	$18, 4		# pl -= 4
	subu	$19, 2		# nl -= 2
	# enough computation to prevent a stall
	mflo	$10		# m0
	mfhi	$14		# m1
	addu	$11, $10	# r11 = M0 = L(p1+c1+m0)
	sltu	$13, $11, $10	# r13 = H(L(p1+c1)+m0)
				# r13+r3 = H(p0+c0+l0)
	multu	$15, $15	# d1*d1
	addu	$13, $14	# assert before r11 == 0 || r3 == 0
	addu	$13, $3		# r13 = M1 = m1+H(p1+c1+m0)
	# Free: 2,3,15; Used: r8:r9 = 2*d0:d1 r10=m0 r11=M0 r12=L1 r13=M1 r14=m1
	addu	$10, $11	# r10 = L(m0+M0)
	sltu	$11, $10, $11	# r11 = H(m0+M0)
	addu	$10, $12	# r10 = N0 = L(M0+m0+L1)
	sw	$10, -12($17)	# pp[-3] = N0
	sltu	$12, $10, $12	# r12 = H(L(m0+M0)+L1)
				# r12+r11 = H(M0+m0+L1)
	addu	$14, $11	# assert r11 == 0 || r12 == 0
	addu	$14, $12	# r14 = N1 = m1+H(M0+m0+L1)
	addu	$14, $13	# r14 = L(M1+N1)
	sltu	$13, $14, $13	# r13 = H(M1+N1)
	# enough computation to prevent a stall
	mflo	$10		# h0
	mfhi	$11		# h1
	addu	$2, $10, $14	# c0 = L(M1+N1+h0)
	sltu	$14, $2, $14	# r14 = H(L(M1+N1)+h0)
				# r14+r13 = H(M1+N1+h0)
	addu	$3, $11, $14	# assert r14 == 0 || r13 == 0
	addu	$3, $13		# c1 = H(M1+N1+h0)
	addu	$4, $17, -8	# arg1 = pp-2
	addu	$5, $18, 2	# arg2 = pl+2
	move	$6, $16		# arg3 = nn
	move	$7, $19		# arg4 = nl
	jal	BnnM2DFastLink
	addu	$20, $20, $2
 #  88	   }
	bgtu	$19, 0, $41
$42:
 #  89	   return c;
	move	$2, $20
	ld	$16, 24($sp)
	ld	$18, 32($sp)
	ld	$20, 40($sp)
	lw	$22, 48($sp)
	lw	$31, 52($sp)
	addu	$sp, 56
	j	$31
	.end	BnnMultiply
