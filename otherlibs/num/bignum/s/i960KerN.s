/*
**	(c) Copyright 1989 Digital Equipment Corporation 
**
** Last modified_on Mon Apr  9 20:18:11 GMT+2:00 1990 by shand
** modified_on Tue Apr  3 19:48:27 GMT+2:00 1990 by sills@notok.enet.dec.com
**
**	KerN for 80960KA
**
**	Author: 	Glenn Sills
**	Date:		1.1
**	Version:	1
**
*/


/*
**	BnnSetToZero(nn, nl)
**	BigNum	nn;
**	int	nl;
**
**	Set all of the digits specified by nn length nl to zero
**
**	nn -> g0
**	nl -> g1
*/

.file "kern.s"
.text
.align 2 
.globl	_BnnSetToZero
.leafproc _BnnSetToZero

_BnnSetToZero:
	mov	g14,g7 		/* Preserve the return address    */
	ldconst 0,g14		/* Must always load g14 with 0 for*/
				/* branch and link procs          */

	cmpobe	0,g1,.bstzxt	/* if (!nl) return                */

	subo	1,g1,g1
	shlo	2,g1,g1		/* Do some pointer arithmetic    */
	addc	g0,g1,g1 	/* nl = nn + nl*byte_per_digit   */
				/* (I happen to know that        */
				/* bytes_per_digit is 4 on 960   */
	
.bstzlp:                        /* Do {                           */
	ldconst	0,g3		
	st	g3,(g0)		/*    *nn = 0                     */
	addc	4,g0,g0         /* } while (++nn <= nl)           */
	cmpoble	g0,g1,.bstzlp  

.bstzxt:
	bx	(g7)		/* Return voidly                  */

/*
**	void	BnnAssign(mm, nn, nl)
**	Bignum	mm, nn;
**	int	nl;
**
**	Copies nn to mm.  We copy from back to front to defend against
**	over lapping bignums.
**
**	mm -> g0
**	nn -> g1
**	nl -> g2
*/

.globl _BnnAssign
.leafproc _BnnAssign

_BnnAssign:
	mov	g14,g7 		/* Mandatory saving of g14	*/
	cmpo	0,g2
	mov	0,g14		
	be	.baexit
	subo	1,g2,g2		/* Prepare for some pointer     */
	cmpo	g0,g1
	shlo	2,g2,g2         /* Arithmetic                   */
	be	.baexit		/* if (mm == nn) exit           */
				/* if (mm >> nn)		*/	
	bg	.balast1	/* then				*/
				/*	Copy from last to first */

	addo	g2,g1,g2

.ba1last:
	ld	(g1),g3 
	addo	4,g1,g1		/*    *mm++ == *nn++		*/
	st	g3,(g0)	
	addo	4,g0,g0		
	cmpo	g1,g2
	ble	.ba1last	/* while (nl > 0)		*/
	bx	(g7)
	
.balast1:
	mov	g1,g4
	addo	g2,g0,g0	
	addo	g2,g1,g1	/* nn += nl                     */

.baloop:
	ld	(g1),g3		
	subo	4,g1,g1		/*	*mm-- == *nn--		*/
	st	g3,(g0)
	subo	4,g0,g0
	cmpo	g1,g4
	bge	.baloop

.baexit:
	bx	(g7)		/* Return voidly                  */


/*
**	void BnnSetDigit(nn,d)
**	BigNum nn;
**	int d;
**
**	Sets a single digit of N to the passed value
**
**	g0 -> nn
**	g1 -> d
**
*/

.globl _BnnSetDigit
.leafproc _BnnSetDigit
.align 2 

_BnnSetDigit:
	mov	g14,g7 		/* Mandatory saving of g14	*/
	ldconst	0,g14

	st	g1,(g0)		/* *nn = d 			*/
	bx	(g7)		/* Return ustedes		*/


/*
**	BigNumDigit BnnGetDigit (nn)
**	BigNum  nn;
**
**	Returns digit pointed to by nn
**
**	g0 -> nn
*/

.globl	_BnnGetDigit
.leafproc _BnnGetDigit
.align 2

_BnnGetDigit:
	mov	g14,g7
	ldconst	0,g14

	ld	(g0),g0
	bx	(g7)	


/*
**	BigNumLength	BnnNumDigits(nn, nl)
**	Bignum nn;
**	int nl;
**
**	Returns the total number of digits in nn not counting leading
**	zeros.
**
**	g0 -> nn
**	g1 -> nl
**
*/

.globl _BnnNumDigits
.leafproc _BnnNumDigits

_BnnNumDigits:
	mov	g14,g7
	ldconst	0,g14


.bndnot0:
	subo	1,g1,g2
	shlo	2,g2,g2
	addo 	g0,g2,g0

.bndloop:			
	cmpobe	0,g1,.bndret1		/* while (nl && *nn == 0) 	*/
	ld	(g0),g3			
	cmpobne	0,g3,.bndrett		/*	--nl;                   */
	subo	4,g0,g0
	subo	1,g1,g1
	b 	.bndloop

.bndret1:
	ldconst	1,g0			/* If nl == 0 return 1		*/	
	bx	(g7)	

.bndrett:
	mov	g1,g0
	bx	(g7)


/*
**	BigNumDigit	BnnNumLeadingZeroBitsInDigit(d)
**	BigNumDigit	d;
**
**	How many leading zero bits are there in the digit?  HUH???
**
**	g0 -> d;
**
*/

.globl	_BnnNumLeadingZeroBitsInDigit
.leafproc _BnnNumLeadingZeroBitsInDigit

_BnnNumLeadingZeroBitsInDigit:
	mov	g14,g7
	ldconst	0,g14

	scanbit	g0, g1
	bo	.bzidnz
	ldconst	32,g0
	bx	(g7)
	
.bzidnz:
	subo	g1,31,g0

	bx	(g7)

/*
**	Boolean BnnDoesDigitFitInWord(d)
**	BigNumDigit d;
**
**	Returns true if the digit d can fit in BNN_WORD_SIZE bits.
**	On the 80960, it always can.
**
**	g0 -> d
*/
.globl	_BnnDoesDigitFitInWord
.leafproc _BnnDoesDigitFitInWord

_BnnDoesDigitFitInWord:
	
	mov	g14,g7
	ldconst	0,g14

	ldconst	1,g0
	bx	(g7)


/*
**	Boolean BnnIsDigitZero (d)
**	BigNumDigit d;
**
**	Returns TRUE iff digit = 0.  We can do this!
**
**
**	g0 -> d
**
*/

.globl	_BnnIsDigitZero
.leafproc _BnnIsDigitZero

_BnnIsDigitZero:

	mov	g14,g7
	ldconst	0,g14

	cmpobne	0,g0, .bidz1
	ldconst	1,g0
	bx	(g7)
	
.bidz1:
	ldconst	0,g0
	bx	(g7)
	


/*
**	Boolean BnnIsDigitNormalized (d)
**	BigNumDigit d
**
**	 Returns TRUE iff Base/2 <= digit < Base
**	 i.e., if digit's leading bit is 1
**
**	g0 -> d
*/

.globl _BnnIsDigitNormalized
.leafproc _BnnIsDigitNormalized
	
_BnnIsDigitNormalized:

	mov	g14,g7
	ldconst	0,g14

	scanbit	g0,g0
	cmpobe	31,g0,.bidnt
	ldconst	0,g0
	bx	(g7)

.bidnt:	
	ldconst	1,g0
	bx	(g7)


/*
**	Boolean BnnIsDigitOdd (d)
**	BigNumDigit d;
**
**	Returns TRUE iff digit is odd
**
**	g0 -> d
*/

.globl _BnnIsDigitOdd
.leafproc _BnnIsDigitOdd

_BnnIsDigitOdd:
	mov	g14,g7
	ldconst 0, g14

	and	1, g0, g0
	bx	(g7)	


/*
**	BigNumCmp BnnCompareDigits (d1, d2)
**	BigNumDigit d1, d2
**
**	Compares digits and returns
**
**	BNN_GREATER	d1 >  d2
**	BNN_EQUAL	d1 == d2
**	BNN_LESS	d1 <  d2
**
**	g0 -> d1
**	g1 -> d2
*/

.globl	 _BnnCompareDigits
.leafproc _BnnCompareDigits

_BnnCompareDigits:
	
	mov	g14,g7
	ldconst	0,g14

	cmpobe	g0,g1,.bcdequal
	bg	.bcdgreater
	ldconst	-1,g0			/* BNN_LESS	*/
	bx	(g7)

.bcdequal:
	ldconst 0,g0			/* BNN_EQUAL	*/
	bx	(g7)

.bcdgreater:
	ldconst 1,g0			/*BNN_GREATER	*/
	bx	(g7)



/*
**	BnnComplement(nn, nl)
**	BigNum nn
**	int    nl
**
**	Complement nn and store result in nn
**
**	g0 -> nn
**	g1 -> nl
**
*/

.globl	_BnnComplement
.leafproc _BnnComplement

_BnnComplement:
	
	mov     g14,g7
	ldconst 0,g14

	cmpobe	0,g1,.bcexit

	subo	1,g1,g1
	shlo	2,g1,g1
	addo	g1,g0,g1

	ldconst	0xffffffff,g3
.bcloop:
	ld	(g0),g2
	xor	g3,g2,g2
	st	g2, (g0)
	addo	4,g0,g0
	cmpoble	g0,g1,.bcloop

.bcexit:
	bx	(g7)


/*
**	BnnAndDigits(n,d)
**	BigNum nn
**	BigNumDigit d
**
**	And the digit d with the first digit in n
**
**	g0 -> nn
**	g1 -> d
**
*/

.globl _BnnAndDigits
.leafproc _BnnAndDigits

_BnnAndDigits:
	mov 	g14,g7
	ldconst	0,g14

	ld	(g0),g2
	and	g1,g2,g2
	st	g2,(g0)

	bx	(g7)


/*
**	BnnOrDigits(n,d)
**	BigNum nn
**	BigNumDigit d
**
**	Returns the logical computation nn[0] |= d;
**
**	g0 -> nn
**	g1 -> d
**
*/

.globl	_BnnOrDigits
.leafproc _BnnOrDigits

_BnnOrDigits:
	mov	g14,g7
	ldconst	0,g14

	ld	(g0),g2
	or	g1,g2,g2
	st	g2,(g0)

	bx	(g7)


/*
**	void BnnXorDigits (n, d)
**	BigNum          n
**	BigNumDigit     d
**
**	Returns the logical computation n[0] XOR d in n[0]
**
**	g0 -> n
**	g1 -> d
**
*/

.globl _BnnXorDigits
.leafproc _BnnXorDigits

_BnnXorDigits:
        mov     g14,g7
        ldconst 0,g14
	
	ld	(g0),g2
	xor	g1,g2,g2
	st	g2,(g0)

	bx	(g7)


/*
**	BigNumDigit BnnShiftLeft (mm, ml, nbits)
**	BigNum mm
**	int    ml
**	int    nbits
**
** 	Shifts M left by "nbits", filling with 0s.
** 	Returns the leftmost "nbits" of M in a digit.
** 	Assumes 0 <= nbits < BNN_DIGIT_SIZE.
**
**	g0 -> mm
**	g1 -> ml
**	g2 -> nbits
*/

.globl	_BnnShiftLeft
.leafproc _BnnShiftLeft

_BnnShiftLeft:

        mov     g14,g7
        ldconst 0,g14
	cmpo	0,g1
	be	.bslexit0
	subo	1,g1,g1
	shlo	2,g1,g1
	addo	g1,g0,g1	/* nl += nn i.e. get the final address */
	mov	g0,g3		/* Save beginning of mm                */
	ldconst	0,g0		/* pre-load result with 0              */
	cmpo	0,g2
	be	.bslexit
	ldconst	32,g6		/* BNN_DIGIT_SIZE			*/
	subo	g2,g6,g6	

.blsloop:
	ld	(g3),g4		/* Access *mm				*/
	shlo	g2,g4,g5	/* *mm == (*mm << nbits)                */
	or	g5,g0,g5	/* or in remaining bits from last op    */
	st	g5,(g3)		/* save the stuff                       */
	shro	g6,g4,g0	/* Save the left over high bits		*/
				/* for the next time through the loop	*/
	addo	4,g3,g3		/* Increment to next address            */
	cmpi	g3,g1
	ble	.blsloop

.bslexit:
	bx	(g7)		/* Note that g0 holds bits that where   */
				/* Shifted out at the end               */

.bslexit0:
	mov	0,g0
	bx	(g7)
/*
**	BigNumDigit BnnShiftRight (mm, ml, nbits)
**	BigNum mm;
**	int    ml;
**	int    nbits;
**
**	 Shifts M right by "nbits", filling with 0s.
**	 Returns the rightmost "nbits" of M in a digit.
**	 Assumes 0 <= nbits < BNN_DIGIT_SIZE.
**
**	g0 -> mm
**	g1 -> ml
**	g2 -> nbits
**
**	Returns result in g0
**
*/
.globl	_BnnShiftRight
.leafproc _BnnShiftRight

_BnnShiftRight:
	mov	g14,g7
	ldconst	0,g14
	mov	g0,g3		/*Save mm in g3 and preload result	*/
	ldconst	0,g0

	cmpobe	0,g1,.bsrexit	/* If this is a zero length Bignum or  */
	cmpobe	0,g2,.bsrexit	/* there are no bits to shift, exit    */

	subo	1,g1,g1		/* Prepare for pointer arithmetic      */
	shlo	2,g1,g1
	addo	g3,g1,g1	/*Point to the last element in the array*/

	
	ldconst	32,g8		/* BNN_DIGIT_SIZE			*/
	subo	g2,g8,g6

.bsrloop:
	ld	(g1),g4		/* *mm = (*mm >> nbits)| leftover bits  */
				/* from the last time through the loop  */
	shro	g2,g4,g5
	or	g0,g5,g5
	st	g5,(g1)
	shlo	g6,g4,g0
	subo	4,g1,g1
	cmpobge	g1,g3,.bsrloop

.bsrexit:
	bx	(g7)		/* Bits shifted out are still in g0!  */


/*
**	BigNumCarry BnnAddCarry (nn, nl, carryin)
**	BigNum         nn;
**	int            nl;
**	BigNumCarry    carryin;
**
**	Performs the sum N + CarryIn => N.  Returns the CarryOut.
**
**	g0 -> nn
**	g1 -> nl
**	g2 -> carryin
**
**	Result is in g0
**
*/

.globl	_BnnAddCarry
.leafproc _BnnAddCarry

_BnnAddCarry:
	mov	g14,g7
	ldconst	0,g14

	cmpobe	0,g2,.bacexit0	/* If carry == 0 return 0 */
	cmpobe	0,g1,.bacexit1	/* If nl == 0 return 1    */

.bacloop:
	subo	1,g1,g1		/* --nl                   */
	ld	(g0),g3		/* g3= *nn                */
	addo	1,g3,g3		/* ++g3                   */
	st	g3,(g0)		/* *nn = g3               */
	addo	4,g0,g0		/* ++nn                   */
	cmpobne	0,g3,.bacexit0	/* if (g3) then return 0  */
	cmpibl	0,g1,.bacloop	/* If (nl) continue loop  */

.bacexit1:
	ldconst	1,g0
	bx	(g7)

.bacexit0:
	ldconst	0,g0
	bx	(g7)



	
/*
**	BigNumCarry BnnSubtractBorrow (nn, nl, carryin)
**	BigNum         nn;
**	int            nl;
**	BigNumCarry    carryin;
**
**	Performs the difference N + CarryIn - 1 => N.  Returns the CarryOut.
**
**	g0 -> nn
**	g1 -> nl
**	g2 -> carryin
*/

.globl	_BnnSubtractBorrow
.leafproc _BnnSubtractBorrow

_BnnSubtractBorrow:
	mov	g14,g7
	ldconst	0,g14

	cmpibe	1,g2,.bsbexit1	/* If Carry return 1	*/
	cmpobe	0,g1,.bsbexit0	/* If (!nl) return 0 	*/

.bsbloop:
	subi	1,g1,g1		/* --nl			*/
	ld	(g0),g3		/* g3 = *nn             */
	mov	g3,g5		/* g5 = *nn             */
	subo	1,g3,g3		/* --g3			*/
	st	g3,(g0)		/* *nn = g3		*/
	addo	4,g0,g0
	cmpobne	0,g5,.bsbexit1	
	cmpibl	0,g1,.bsbloop

.bsbexit0:
	ldconst 0,g0
	bx	(g7)

.bsbexit1:
	ldconst	1,g0
	bx	(g7)



/*
**	BigNumCarry BnnSubtract (mm, ml, nn, nl, carryin)
**	BigNum         mm, nn;
**	int            ml;
**	int            nl;
**	BigNumCarry    carryin;
**
**	Performs the difference M - N + CarryIn - 1 => M.
**	Returns the CarryOut.
**	Assumes Size(M) >= Size(N).
**
**	g0 -> mm
**	g1 -> ml
**	g2 -> nn
**	g3 -> nl
**	g4 -> carryin
**
*/

.globl	_BnnSubtract


_BnnSubtract:
	subo	g3,g1,g1
	cmpibe	0,g3,.bslpe	/* While (--nl >= 0)	*/

	ldconst	-1,r5
.bsloop:
	subi	1,g3,g3
	ld	(g0),g5		/* g5 = *mm		*/
	ld	(g2),g6		/* g6 = *nn		*/
	xor	r5,g6,g6	/* g6 = (*nn) ^ -1      */
	addo	g4,g5,g4	/* c += *mm             */
	cmpobge g4,g5,.bsgt	/* if (c < *mm) {       */
	mov	g6,g5		/* 	*mm = invn      */
	ldconst	1,g4		/*	c = 1           */
	b	.cleanup	/* }                    */
.bsgt:
	addo	g4,g6,g4	/* else { c += g6       */
	mov	g4,g5		/* *mm = c              */
	cmpobl	g4,g6, .bsset1	/* if (c < g6) then c=1 */
	ldconst	0,g4		/* else c = 0           */
	b	.cleanup	/* }                    */
.bsset1:
	ldconst	1,g4

.cleanup:	
	st	g5,(g0)		
	addo	4,g0,g0
	addo	4,g2,g2
	cmpibl	0,g3,.bsloop	/* While (--nl >= 0)	*/

.bslpe:
	mov	g4,g2
	lda	.bsexit,g14	
	bal	_BnnSubtractBorrow

.bsexit:
	ret
	

/*
**	BigNumCarry BnnMultiplyDigit (pp, pl, mm, ml, d)

**	BigNum         pp, mm;
**	int            pl, ml;
**	BigNumDigit    d;
**
**	 Performs the product:
**	 Q = P + M * d
**	 BB = BBase(P)
**	 Q mod BB => P
**	 Q div BB => CarryOut
**	 Returns the CarryOut.
**	 Assumes Size(P) >= Size(M) + 1.
**
**	
**	g0 -> pp
**	g1 -> pl
**	g2 -> mm
**	g3 -> ml
**	g4 -> d
*/

.globl	_BnnMultiplyDigit
	
_BnnMultiplyDigit:
	cmpo	0,g4
	be	.bmdexit0		/* if the multiplier is 0    */
	cmpo	1,g4
	be	.bmdbnnadd 
	subo	g3,g1,g1		/* pl -= ml		  */
	mov	0,g6			/* Carry = 0             */
	cmpo	0,g3
	mov	0,g7
	be	.bmdbye			/* While (m--)           */

.bmdlp1:
	ld	(g2),r3			/* r3 = *mm		*/
	subo	1,g3,g3
	ld	(g0),r4			/* r4 = *p		*/
					/* r5 = *(p++)		*/
	emul	g4,r3,r6		/* r6-r7 = *mm x d      */
	cmpo	1,0			/* Clear the carry bit	*/	
	addc	r4,r6,r6
	addc	0,r7,r7
	addc	r6,g6,g6
	addc	r7,g7,g7
	st	g6,(g0)			/* *p = C               */
	mov	g7,g6			/* c >> = BN_DIGIT_SIZE */
	addo	4,g0,g0
	mov	0,g7
	addo	4,g2,g2
	cmpo	0,g3
	bl	.bmdlp1			/* While (m--)           */

	cmpobl	0,g1, .bmdlp2
	mov	g6,g0
	ret

.bmdlp2:
	ld	(g0),r4
	cmpo	1,0
	addc	g6,r4,g6
	addc	0,g7,g7
	st	g6,(g0)
	mov	g7,g6
	subo	1,g1,g1
	mov	0,g7
	addo	4,g0,g0
	cmpobl	0,g1,.bmdlp2
	mov	g6,g0
	ret


.bmdbye:
	mov	0,g0
	ret

.bmdexit0:
	mov	0,g0			/* its a sure bet the result */
	ret

.bmdbnnadd: 
	mov	0,g4			/* Just add the 2 bignums    */
	call	_BnnAdd			/* of adding the 2           */
	ret


/*
**	BigNumDigit BnnDivideDigit (qq, nn, nl, d)
**	BigNum         qq, nn;
**	int            nl;
**	BigNumDigit    d;
**
** 	Performs the quotient: N div d => Q
** 	Returns R = N mod d
** 	Assumes leading digit of N < d, and d > 0.
**
**	g0 -> qq
**	g1 -> nn
**	g2 -> nl
**	g3 -> d
*/

.globl	_BnnDivideDigit
.leafproc _BnnDivideDigit

_BnnDivideDigit:
	mov	g14,g7		/* Do standard leafproc stuff	    */
	ldconst	0,g14
	cmpo	0,g2
	be	.bddret0 	/* Is this a Null length BIGNUM?    */
	cmpo	0,g3
	be	.bddret0	/* Is the divisor zero?             */

.bddndz:
	subo	1,g2,g2
	shlo	2,g2,g5
	addo	g1,g5,g1	/* nn += nl                         */
	subo	4,g5,g5		/* --nl                             */
	addo	g0,g5,g0        /* qq += nl                         */
	ld	(g1),g9         /* Preset remainder                 */
	subo	4,g1,g1         /* --nn                             */
	cmpo	0,g2
	be	.bddexit
.bddloop:
	subo	1,g2,g2		/*	--nl                        */
	ld	(g1),g8         /*      LSB of quad is next digit   */
	ediv	g3,g8,g8        /*      remainder =quad%d,          */
	st	g9,(g0)	        /*	*qq = quad/d                */
	subo	4,g0,g0         /*      --qq                        */
	subo	4,g1,g1         /*      --nn                        */
	cmpo	0,g2
	mov	g8,g9		
	bne	.bddloop   	/* }                                */

.bddexit:
	mov	g9,g0           /* Return (remainder)               */
	bx	(g7)

.bddret0:
	mov	0,g0
	bx	(g7)


/*
**	BigNumCarry BnnAdd (mm, ml, nn, nl, carryin)
**	BigNum         mm, nn;
**	int            ml;
**	int            nl;
**	BigNumCarry    carryin;
**
**	Performs the sum M + N + CarryIn => M.
**	Returns the CarryOut.  Assumes Size(M) >= Size(N).
**
**	g0 -> mm
**	g1 -> ml
**	g2 -> nn 
**	g3 -> nl
**	g4 -> caryin;
**
**	Result is in g0 and M (of course!)
*/

.text
.align 2
.globl	_BnnAdd

_BnnAdd:


	subo	g3,g1,g1	/* ml -= nl   				*/
	shlo	1,g4,g4
	cmpobe	0,g3,.bafni	/* if (!nl)                             */
	

.balp:
	modac	02,g4,g4	
	ld	(g0),g5		/* g5 = *mm                            */
	ld	(g2),g6		/* g6 = *nn                            */
	addc	g6,g5,g7	/* g7 = *m + *n                        */
	modac	00,00,g4	/* Save the carry bit                  */
	st	g7,(g0)		/* *m = g7                             */
	addo	4,g0,g0		/* ++m                                 */
	addo	4,g2,g2		/* ++n  			       */
	subi	1,g3,g3		/* --nl                                */
	cmpobl	0,g3,.balp

.bafni:
	shro	1,g4,g4
	and	01,g4,g2
	lda	.bazit,g14
	bal	_BnnAddCarry

.bazit:
	ret


