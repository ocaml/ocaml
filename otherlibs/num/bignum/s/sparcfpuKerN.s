! Copyright     Digital Equipment Corporation     1991
! Last modified_on Fri Jan 25 23:11:58 GMT+1:00 1991 by shand
!
!    KerN for SPARC
!    Mark Shand
!
!    Implementation notes:
!
!        Initial implementations of sparc offer very limited support for
!        integer multiplication, so BnnMultiplyDigit is based on
!        double precision floating point multiplies that compute
!        a 16x32->48 bit result without round-off.  Performance is
!        not great, but is about twice as good as using the integer
!        multiply primitives directly.
!
!        BnnDivideDigit uses the unmodified assembly code produced
!        by cc -O2 KerN.c
!
	.seg	"text"			! [internal]
	.proc	16
	.global	_BnnSetToZero
_BnnSetToZero:
	deccc	%o1
	bneg	LBSZ3			! is zero
	andcc	1,%o1,%o2
	be	LBSZ2			! is odd
	nop
	dec	4,%o0	
LBSZ1:					! [internal]
	inc	8,%o0
	st	%g0,[%o0-4]
LBSZ2:
	deccc	2,%o1
	bpos	LBSZ1
	st	%g0,[%o0]
LBSZ3:
	retl
	nop				! [internal]
!
!
	.proc	16
	.global	_BnnAssign
_BnnAssign:
	cmp	%o0,%o1
	bgt,a	LBAG2			! if(mm >= nn) goto LBAG2
	tst	%o2
	be	LBAGX
	tst	%o2
	be	LBAGX			! if(nl==0) return
	nop
LBAG1:
	ld	[%o1],%o3
	inc	4,%o1
	st	%o3,[%o0]
	deccc	%o2
	bgt	LBAG1
	inc	4,%o0
LBAGX:
	retl
	nop
LBAG2:
	be	LBAGX			! if(nl==0) return
	sll	%o2,2,%o3		! nl <<= 2
	add	%o1,%o3,%o1		! nn += nl
	add	%o0,%o3,%o0		! mm += nl
LBAG3:
	dec	4,%o1
	ld	[%o1],%o3		! %o3 = *--nn
	dec	4,%o0
	deccc	%o2
	bgt	LBAG3
	st	%o3,[%o0]		! *--mm = %o3
	retl
	nop
!
!
	.proc	16
	.global	_BnnSetDigit
_BnnSetDigit:
	retl
	st	%o1,[%o0]
!
!
	.proc	14
	.global	_BnnGetDigit
_BnnGetDigit:
	retl
	ld	[%o0],%o0
!
!
	.proc	14
	.global	_BnnNumDigits
_BnnNumDigits:
	tst	%o1
	sll	%o1,2,%o3
	be	LBND2
	add	%o0,%o3,%o4
	dec	4,%o4
LBND1:
	ld	[%o4],%o2
	tst	%o2
	bne	LBND2
	deccc	%o1
	bne,a	LBND1
	dec	4,%o4
LBND2:
	retl
	add	1,%o1,%o0
!
!
	.proc	14
	.global	_BnnNumLeadingZeroBitsInDigit
_BnnNumLeadingZeroBitsInDigit:
	addcc	%o0,%g0,%o5		! %o5 = d
	be	LBLZX			! if(!d) goto BLZX
	sethi	%hi(0xffff0000),%o1	! mask = 0xffff0000
	mov	1,%o0			! p = 1
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ1
	sll	%o1,8,%o1
	 sll	%o5,16,%o5
	 or	16,%o0,%o0
LBLZ1:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ2
	sll	%o1,4,%o1
	 sll	%o5,8,%o5
	 or	8,%o0,%o0
LBLZ2:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ3
	sll	%o1,2,%o1
	 sll	%o5,4,%o5
	 or	4,%o0,%o0
LBLZ3:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ4
	nop
	 sll	%o5,2,%o5
	 or	2,%o0,%o0
LBLZ4:
	srl	%o5,31,%o5		! %o5 = (d & 0x80000000) != 0
	retl
	xor	%o0,%o5,%o0
LBLZX:
	retl
	mov	32,%o0
	.proc	4
	.global	_BnnDoesDigitFitInWord
_BnnDoesDigitFitInWord:
	retl
	mov	1,%o0
	.proc	4
	.global	_BnnIsDigitZero
_BnnIsDigitZero:
	tst	%o0
	bne,a	LBDZ0
	mov	0,%o1
	mov	1,%o1
LBDZ0:
	retl
	add	%g0,%o1,%o0
	.proc	4
	.global	_BnnIsDigitNormalized
_BnnIsDigitNormalized:
	retl
	srl	%o0,31,%o0
	.proc	4
	.global	_BnnIsDigitOdd
_BnnIsDigitOdd:
	retl
	and	%o0,1,%o0
	.proc	4
	.global	_BnnCompareDigits
_BnnCompareDigits:
	cmp	%o0,%o1
	bleu	LBCD1
	mov	-1,%o0
	retl
	mov	1,%o0
LBCD1:					! [internal]
	be,a	LBCD2
	mov	0,%o0
LBCD2:
	retl
	nop				! [internal]
	.proc	16
	.global	_BnnComplement
_BnnComplement:
	deccc	%o1
	bneg	LE129
	nop
LY11:					! [internal]
	ld	[%o0],%o2
	xor	%o2,-1,%o2
	st	%o2,[%o0]
	deccc	%o1
	bpos	LY11
	inc	4,%o0
LE129:
	retl
	nop				! [internal]
	.proc	16
	.global	_BnnAndDigits
_BnnAndDigits:
	ld	[%o0],%o2
	and	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	16
	.global	_BnnOrDigits
_BnnOrDigits:
	ld	[%o0],%o2
	or	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	16
	.global	_BnnXorDigits
_BnnXorDigits:
	ld	[%o0],%o2
	xor	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	14
	.global	_BnnShiftLeft
_BnnShiftLeft:
	tst	%o2
	be	L77105
	mov	0,%o4
	deccc	%o1
	mov	32,%o3
	bneg	L77105
	sub	%o3,%o2,%o3
LY12:					! [internal]
	ld	[%o0],%o5
	sll	%o5,%o2,%g1
	or	%g1,%o4,%g1
	st	%g1,[%o0]
	deccc	%o1
	srl	%o5,%o3,%o4
	bpos	LY12
	inc	4,%o0
L77105:
	retl
	add	%g0,%o4,%o0
	.proc	14
	.global	_BnnShiftRight
_BnnShiftRight:
	tst	%o2
	be	L77114
	mov	0,%o4
	sll	%o1,2,%g1
	deccc	%o1
	mov	32,%o3
	add	%o0,%g1,%o0
	bneg	L77114
	sub	%o3,%o2,%o3
LY13:					! [internal]
	dec	4,%o0
	ld	[%o0],%o5
	srl	%o5,%o2,%g2
	or	%g2,%o4,%g2
	deccc	%o1
	sll	%o5,%o3,%o4
	bpos	LY13
	st	%g2,[%o0]
L77114:
	retl
	add	%g0,%o4,%o0
	.proc	14
	.global	_BnnAddCarry		! (mm, ml, car)
_BnnAddCarry:
	tst	%o2
	be	LBACX0			! if(car == 0) return(0);
	tst	%o1
	be	LBACX1			! if(nl == 0) return(1);
	nop
LBACL:
	ld	[%o0],%o3
	inccc	%o3
	bcc	LBACX0
	st	%o3,[%o0]
	deccc	%o1
	bgt	LBACL
	inc	4,%o0
LBACX1:
	retl
	mov	1,%o0
LBACX0:
	retl
	mov	0,%o0
	.proc	14
	.global	_BnnAdd			! (mm ml nn nl car)
_BnnAdd:
	sub	%o1,%o3,%o1		! ml -= nl
	tst	%o3
	be,a	_BnnAddCarry		! if (nl == 0) %o2 = car; goto AddCarry
	mov	%o4,%o2
LBAD1:
	ld	[%o2],%o5		! o5 = *nn
	addcc	-1,%o4,%g0		! set C = carin
	ld	[%o0],%o4		! o4 = *mm
	inc	4,%o2
	addxcc	%o5,%o4,%o5		! o5 = *mm + *nn, C = carout
	addx	%g0,%g0,%o4		! o4 = carout
	st	%o5,[%o0]
	deccc	%o3
	bne	LBAD1
	inc	4,%o0
	b	_BnnAddCarry
	mov	%o4,%o2
	.proc	14
	.global	_BnnSubtractBorrow	! (mm, ml, car)
_BnnSubtractBorrow:
	tst	%o2
	bne	LSBBX1			! if(car == 1) return(1);
	tst	%o1
	be	LSBBX0			! if(nl == 0) return(0);
	nop
LSBBL:
	ld	[%o0],%o3
	deccc	%o3
	bcc	LSBBX1
	st	%o3,[%o0]
	deccc	%o1
	bgt	LSBBL
	inc	4,%o0
LSBBX0:
	retl
	mov	0,%o0
LSBBX1:
	retl
	mov	1,%o0
	.proc	14
	.global	_BnnSubtract		! (mm ml nn nl car)
_BnnSubtract:
	sub	%o1,%o3,%o1		! ml -= nl
	tst	%o3
	be,a	_BnnSubtractBorrow	! if (nl == 0) %o2 = car; goto SubBorrow
	mov	%o4,%o2
LSUB1:
	ld	[%o2],%o5		! o5 = *nn
	deccc	%o4			! set C = carin
	ld	[%o0],%o4		! o4 = *mm
	inc	4,%o2
	subxcc	%o4,%o5,%o5		! o5 = *mm + *nn, C = carout
	mov	1,%o4
	subx	%o4,%g0,%o4		! o4 = carout
	st	%o5,[%o0]
	deccc	%o3
	bne	LSUB1
	inc	4,%o0
	b	_BnnSubtractBorrow
	mov	%o4,%o2
	.proc	14
	.global	_BnnMultiplyDigit	! (pp pl mm ml d)
! Assembler version of BnnMultiplyDigit is derived from the
! following code.
!
! BigNumCarry
! BnnMultiplyDigit(pp, pl, mm, ml, d)
! register BigNum 	pp, mm;
!          int 		pl, ml; 
!          BigNumDigit	d;
! {
!     register double fd, lowAlignR;
!     register BigNumDigit carry = 0;
! 
!     fd = (double) d;
!     lowAlignR = (65536.0*65536.0*65536.0*16.0);
! 
!     pl -= ml;
! 
!     while (ml--)
!     {
!         BigNumDigit md, pd;
!         register BigNumDigit tmp0, tmp1;
!         register double fmh, fml;
!         double fmlxd, fmhxd;
! 
!         md = *mm++;
!         pd = *pp;
!         fml = (double) (int) (md & 0xffff);
!         fmh = (double) (int) (md >> 16);
!         fmlxd = fd*fml + lowAlignR;
!         fmhxd = fd*fmh + lowAlignR;
!         pd += carry;
!         carry = (pd < carry);
!         tmp0 = ((unsigned long *)(&fmlxd))[1];
!         carry += (((unsigned long *)(&fmlxd))[0]) &0xffff;
!         if ((pd += tmp0) < tmp0) carry++;
!         tmp0 = ((unsigned long *)(&fmhxd))[1];
!         tmp1 = tmp0 << 16;
!         if ((pd += tmp1) < tmp1) carry++;
!         carry += (tmp0 >> 16);
!         carry += (((unsigned long *)(&fmhxd))[0]) << 16;
!         /* assert carry:pd = d*md + *pp + carry(in) */
!         *pp++ = pd;
!     }
! 
!     while (carry && pl--)
!     {
!         BigNumDigit pd;
! 
!         pd = *pp;
!         carry = (pd += carry) < carry;
!         *pp++ = pd;
!     }
!     return carry;
! }
_BnnMultiplyDigit:
!#PROLOGUE# 0
!#PROLOGUE# 1
	save	%sp,-120,%sp		! establish stack frame
	st	%i4,[%sp+LP61+32]	! mem = d
	ld	[%sp+LP61+32],%f0	! f0 = d
	fitod	%f0,%f26		! f26 = (double) d
	mov	0,%i5			! carry = 0
	tst	%i4			! if (i >= 0)
	bge	LBMD1			!     goto LBMD1
	sethi	%hi(L2pwr32),%o0
	ldd	[%o0+%lo(L2pwr32)],%f4	! f4 = 2^32
	faddd	%f26,%f4,%f26		! f26 += 2^32 
LBMD1:
	sethi	%hi(L2pwr52),%o1
	ldd	[%o1+%lo(L2pwr52)],%f24	! f24 = 2^52
	tst	%i3			! ml?
	be	LBMDExit		! if (ml == 0)
	sub	%i1,%i3,%i1		!     goto LBMDExit; pl -= ml
	st	%g0,[%sp+LP61+32]	! clr [%sp+LP61+32]
LBMDpxdLoop:
	ld	[%i2],%o4		! o4 = md = *mm
	sth	%o4,[%sp+LP61+34]	! o4 & 0xffff -> mem
	ld	[%sp+LP61+32],%f7	! f7 <- mem
	fitod	%f7,%f30		! fml = (double) (md & 0xffff)
	srl	%o4,16,%o4		! o4 = md >> 16
	st	%o4,[%sp+LP61+32]	! i4 -> mem
	! fitod + 8 cycles.  f30 ready on SS1
	fmuld	%f26,%f30,%f12		! f12 = fd * fml
	ld	[%sp+LP61+32],%f9	! f9 <- mem
	fitod	%f9,%f28		! fmh = (double) (md >> 16)
	ld	[%i0],%l7		! pd = l7 = *pp
	inc	4,%i2			! mm++
	inc	4,%i0			! pp++
	! fmuld + 10 fitod + 6.  f28 ready, mul/add unit available on SS1
	faddd	%f12,%f24,%f14		! f14 = f12 + 2^52
	fmuld	%f26,%f28,%f16		! f16 = fd * fmh
	addcc	%l7,%i5,%i4		! pd += carry{in}
	! 1 cycle stall of faddd
	st	%f15,[%fp-4]		! fmlxd[low] = f15
	! fmuld + 9.  f16 ready on SS1
	faddd	%f16,%f24,%f18		! f18 = f16 + 2^52
	st	%f14,[%fp-8]		! fmlxd[high] = f14
	ld	[%fp-4],%l7		! tmp0 = l7 = fmlxd[low]
	lduh	[%fp-6],%i5		! i5 = fmlxd[high] & 0xffff
	addx	%g0,%i5,%i5		! carry = (fmlxd[high] & 0xffff)+C
	addcc	%i4,%l7,%l7		! pd += tmp0
	st	%f18,[%fp-16]		! fmhxd = f18
	ld	[%fp-16],%o4		! o4 = fmhxd[high]
	st	%f19,[%fp-12]		! fmhxd = f18
	ld	[%fp-12],%o5		! o5 = fmhxd[low]
	sll	%o5,16,%l3		! l3 = fmhxd[low] << 16
	srl	%o5,16,%o5		! o5 = fmhxd[low] >> 16
	addx	%i5,%o5,%i5		! carry += (fmhxd[low] >> 16) + C
	addcc	%l7,%l3,%l7		! pd += fmhxd[low] << 16
	sll	%o4,16,%l3		! l3 = fmlxd[high] << 16
	addx	%i5,%l3,%i5		! carry += fmlxd[high] << 16 + C
	deccc	%i3			! ml--
	bne	LBMDpxdLoop		! if (ml > 0)
	st	%l7,[%i0-4]		!     goto LBMDpxdLoop; pp[-1] = pd
	tst	%i5
	be	LBMDExit		! if (!carry)
	nop				!     goto LBMDExit
LBMDacLoop:
	deccc	%i1
	blt	LBMDExit
	ld	[%i0],%i3
	addcc	%i3,%i5,%i3
	addxcc	%g0,%g0,%i5
	st	%i3,[%i0]
	bne	LBMDacLoop
	inc	4,%i0
LBMDExit:
	ret
	restore	%g0,%i5,%o0
LP61	=	64
	.seg	"data"			! [internal]
	.align	8
L2pwr32:
	.word	0x41f00000
	.word	0
	.align	8
L2pwr52:
	.word	0x43300000
	.word	0
	.seg	"text"
	.proc	14
	.global	_BnnDivideDigit
_BnnDivideDigit:
!#PROLOGUE# 0
!#PROLOGUE# 1
	save	%sp,-112,%sp
	call	_BnnNumLeadingZeroBitsInDigit,1
	mov	%i3,%o0
	mov	%o0,%o2
	tst	%o2
	be	L77225
	st	%o2,[%fp-8]
	ld	[%i0-4],%o4
	st	%i2,[%fp-16]
	st	%o4,[%fp-12]
	mov	%i2,%o1
	mov	%i1,%o0
	call	_BnnShiftLeft,3
	sll	%i3,%o2,%i3
L77225:
	sub	%i2,1,%l2
	sethi	%hi(0xffff),%o1		! [internal]
	or	%o1,%lo(0xffff),%o1	! [internal]
	sll	%i2,2,%l3
	add	%i1,%l3,%l3
	dec	4,%l3
	ld	[%l3],%i2
	and	%i3,%o1,%l1
	sll	%l2,2,%l4
	tst	%l2
	srl	%i3,16,%l6
	mov	%o1,%l0
	sll	%l1,16,%l7
	add	%i0,%l4,%l4
	be	L77249
	add	%l6,1,%l5
LY43:					! [internal]
	dec	4,%l3
	ld	[%l3],%i4
	mov	%i2,%i5
	mov	%i5,%o0
	call	.udiv,2
	mov	%l6,%o1
	mov	%o0,%i1
	mov	%l1,%o0
	call	.umul,2
	mov	%i1,%o1
	mov	%o0,%i2
	mov	%l6,%o0
	call	.umul,2
	mov	%i1,%o1
	srl	%i2,16,%i0
	add	%o0,%i0,%i0
	cmp	%i0,%i5
	dec	%l2
	bgu	L77232
	sll	%i2,16,%i2
	cmp	%i0,%i5
	bne	LY57
	cmp	%i2,%i4
LY54:					! [internal]
	bleu,a	LY57
	cmp	%i2,%i4
L77232:
	cmp	%l7,%i2
LY55:					! [internal]
	bleu	L77234
	dec	%i1
	sub	%i2,%l7,%i2
	b	L77228
	sub	%i0,%l5,%i0
LY56:					! [internal]
	ld	[%fp-4],%o3
	ld	[%fp+68],%i0
	ld	[%fp+80],%o1
	dec	4,%o0
	ld	[%o0],%o0
	sll	%o3,32,%o3
	call	.udiv,2
	or	%o3,%o0,%o0
	dec	4,%i0
	st	%o0,[%i0]
	ld	[%fp+76],%o0
	tst	%o0
	bne,a	LY56
	ld	[%fp+72],%o0
	b	L77259
	ld	[%fp-4],%i2
L77234:
	sub	%i0,%l6,%i0
	sub	%i2,%l7,%i2
L77228:
	cmp	%i0,%i5
	bgu,a	LY55
	cmp	%l7,%i2
	cmp	%i0,%i5
	be	LY54
	cmp	%i2,%i4
LY57:					! [internal]
	bleu	LY47
	sub	%i4,%i2,%i4
	inc	%i0
LY47:					! [internal]
	sub	%i5,%i0,%i5
	sll	%i5,16,%o0
	srl	%i4,16,%o7
	sll	%i1,16,%i1
	dec	4,%l4
	st	%i1,[%l4]
	mov	%l6,%o1
	or	%o0,%o7,%o0
	call	.udiv,2
	nop
	mov	%o0,%i1
	mov	%l1,%o0
	call	.umul,2
	mov	%i1,%o1
	mov	%o0,%i2
	mov	%l6,%o0
	call	.umul,2
	mov	%i1,%o1
	mov	%o0,%i0
	srl	%i2,16,%o0
	add	%i0,%o0,%i0
	and	%i0,%l0,%o2
	srl	%i0,16,%i0
	cmp	%i0,%i5
	sll	%o2,16,%o2
	and	%i2,%l0,%i2
	bgu	L77244
	or	%i2,%o2,%i2
	cmp	%i0,%i5
	bne,a	LY53
	ld	[%l4],%o1
	cmp	%i2,%i4
LY51:					! [internal]
	bleu,a	LY53
	ld	[%l4],%o1
L77244:
	cmp	%i3,%i2
LY52:					! [internal]
	bleu	L77246
	dec	%i1
	sub	%i2,%i3,%i2
	b	L77240
	dec	%i0
L77246:
	sub	%i2,%i3,%i2
L77240:
	cmp	%i0,%i5
	bgu,a	LY52
	cmp	%i3,%i2
	cmp	%i0,%i5
	be,a	LY51
	cmp	%i2,%i4
	ld	[%l4],%o1
LY53:					! [internal]
	tst	%l2
	or	%o1,%i1,%o1
	sub	%i4,%i2,%i2
	bne	LY43
	st	%o1,[%l4]
L77249:
	ld	[%fp-8],%o2
	tst	%o2
	be,a	LY50
	ld	[%fp-8],%o1
	cmp	%l4,%l3
	bleu,a	LY49
	cmp	%l4,%l3
	ld	[%fp-16],%o4
	sll	%o4,2,%o4
	add	%l3,%o4,%o4
	cmp	%l4,%o4
	bcc,a	LY49
	cmp	%l4,%l3
	sub	%l4,%l3,%i0
	sra	%i0,2,%i0
	mov	%i0,%o1
	call	_BnnShiftRight,3
	mov	%l3,%o0
	ld	[%fp-12],%o4
	dec	%i0
	sll	%i0,2,%i0
	b	L77258
	st	%o4,[%l3+%i0]
LY49:					! [internal]
	bne,a	LY48
	ld	[%fp-16],%o1
	ld	[%fp-16],%o0
	mov	1,%o1
	dec	%o0
	sll	%o0,2,%o0
	b	LY42
	add	%l3,%o0,%o0
LY48:					! [internal]
	mov	%l3,%o0
LY42:					! [internal]
	call	_BnnShiftRight,3
	ld	[%fp-8],%o2
L77258:
	ld	[%fp-8],%o1
LY50:					! [internal]
	srl	%i2,%o1,%i2
L77259:
	ret
	restore	%g0,%i2,%o0
	.seg	"data"			! [internal]
_copyright:
	.half	0x4028
	.half	0x2329
	.half	0x4b65
	.half	0x724e
	.half	0x2e63
	.half	0x3a20
	.half	0x636f
	.half	0x7079
	.half	0x7269
	.half	0x6768
	.half	0x7420
	.half	0x4469
	.half	0x6769
	.half	0x7461
	.half	0x6c20
	.half	0x4571
	.half	0x7569
	.half	0x706d
	.half	0x656e
	.half	0x7420
	.half	0x436f
	.half	0x7270
	.half	0x6f72
	.half	0x6174
	.half	0x696f
	.half	0x6e20
	.half	0x2620
	.half	0x494e
	.half	0x5249
	.half	0x4120
	.half	0x3139
	.half	0x3838
	.half	0x2c20
	.half	0x3139
	.half	0x3839
	.half	0xa00
