/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Mon Apr 15 18:51:35 GMT+2:00 1991 by herve */
/*      modified_on Fri Mar 30  3:29:17 GMT+2:00 1990 by shand */


/* bnDivide.c: a piece of the bignum kernel written in C */


		/***************************************/

#define BNNMACROS_OFF
#include "BigNum.h"

                        /*** copyright ***/

static char copyright[]="@(#)bnDivide.c: copyright Digital Equipment Corporation & INRIA 1988, 1989, 1990\n";


static divide (nn, nl, dd, dl)

	 BigNum		nn, dd;
register BigNumLength 	nl, dl;

/*
 * In-place division.
 *
 * Input (N has been EXTENDED by 1 PLACE; D is normalized):
 *	+-----------------------------------------------+----+
 *	|  			N			  EXT|
 *	+-----------------------------------------------+----+
 *
 *	+-------------------------------+
 *	|		D	       1|
 *	+-------------------------------+
 *
 * Output (in place of N):
 *	+-------------------------------+---------------+----+
 *	|		R	 	|	   Q	     |
 *	+-------------------------------+---------------+----+
 *
 * Assumes:
 *    N > D
 *    Size(N) > Size(D)
 *    last digit of N < last digit of D
 *    D is normalized (Base/2 <= last digit of D < Base)
 */

{
   register 	int		ni;
   		BigNumDigit 	DDigit, BaseMinus1, QApp, RApp;


   /* Initialize constants */
   BnnSetDigit (&BaseMinus1, 0);
   BnnComplement(&BaseMinus1, 1);

   /* Save the most significant digit of D */
   BnnAssign (&DDigit, dd+dl-1, 1);

   /* Replace D by Base - D */
   BnnComplement (dd, dl);
   BnnAddCarry (dd, dl, 1);

   /* For each digit of the divisor, from most significant to least: */
   nl += 1;
   ni = nl-dl;
   while (--ni >= 0) 
   {
      /* Compute the approximate quotient */
      nl--;

      /* If first digits of numerator and denominator are the same, */
      if (BnnCompareDigits (*(nn+nl), DDigit) == BN_EQ)
	 /* Use "Base - 1" for the approximate quotient */
	 BnnAssign (&QApp, &BaseMinus1, 1);
      else
	 /* Divide the first 2 digits of N by the first digit of D */
	 RApp = BnnDivideDigit (&QApp, nn+nl-1, 2, DDigit);

      /* Compute the remainder */
      BnnMultiplyDigit (nn+ni, dl+1, dd, dl, QApp);
      
      /* Correct the approximate quotient, in case it was too large */
      while (BnnCompareDigits (*(nn+nl), QApp) != BN_EQ)
      {
	 BnnSubtract (nn+ni, dl+1, dd, dl, 1);	/* Subtract D from N */
	 BnnSubtractBorrow (&QApp, 1, 0);  	/* Q -= 1 */
      }
   }

   /* Restore original D */
   BnnComplement (dd, dl);
   BnnAddCarry (dd, dl, 1);
}


		/***************************************/
/**/


void BnnDivide (nn, nl, dd, dl)

	 BigNum		nn, dd;
register BigNumLength 	nl, dl;

/*
 * Performs the quotient:
 *    N div D => high-order bits of N, starting at N[dl]
 *    N mod D => low-order dl bits of N
 *
 * Assumes 
 *    Size(N) > Size(D),
 *    last digit of N < last digit of D (if N > D).
 */

{
   BigNumDigit 	nshift;


   /* Take care of easy cases first */
   switch (BnnCompare (nn, nl, dd, dl))
   {
      case BN_LT:	/* n < d */
	 ;					/* N => R */
	 BnnSetToZero (nn+dl, nl-dl);		/* 0 => Q */
	 return;
      case BN_EQ:	/* n == d */
	 BnnSetToZero (nn, nl);			/* 0 => R */
	 BnnSetDigit (nn+dl, 1);		/* 1 => Q */
	 /* bug fixed Mon Apr 15 18:36:50 GMT+2:00 1991 by jch,
	    was BnnSetDigit (nn+nl-1, 1); */
	 return;
   }

   /* here: n > d */

   /* If divisor is just 1 digit, use a special divide */
   if (dl == 1)
      *nn = BnnDivideDigit (nn+1, nn, nl, *dd);        /* note: nn+1 = nn+dl */
   /* Otherwise, divide one digit at a time */
   else
   {
      /* Normalize */
      nshift = BnnNumLeadingZeroBitsInDigit (*(dd+dl-1));
      BnnShiftLeft (dd, dl, nshift);
      BnnShiftLeft (nn, nl, nshift);

      /* Divide */
      divide (nn, nl-1, dd, dl);

      /* Unnormalize */
      BnnShiftRight (dd, dl, nshift);
      BnnShiftRight (nn, dl, nshift); 
      /* note: unnormalize N <=> unnormalize R (with R < D) */
   }
}
