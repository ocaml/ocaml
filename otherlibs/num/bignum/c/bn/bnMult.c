/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Tue Oct  9 10:43:48 GMT+1:00 1990 by herve */
/*      modified_on Fri Mar 30  4:13:47 GMT+2:00 1990 by shand */


/* bnMult.c: a piece of the bignum kernel written in C */


		/***************************************/

#define BNNMACROS_OFF
#include "BigNum.h"

                        /*** copyright ***/

static char copyright[]="@(#)bnMult.c: copyright Digital Equipment Corporation & INRIA 1988, 1989, 1990\n";


BigNumCarry BnnMultiply (pp, pl, mm, ml, nn, nl)

register BigNum		pp, nn;
	 BigNum 	mm;
register BigNumLength	pl, nl;
	 BigNumLength	ml;

/*
 * Performs the product:
 *    Q = P + M * N
 *    BB = BBase(P)
 *    Q mod BB => P
 *    Q div BB => CarryOut
 *
 * Returns the CarryOut.  
 *
 * Assumes: 
 *    Size(P) >= Size(M) + Size(N), 
 *    Size(M) >= Size(N).
 */

{
   BigNumCarry c;

   /* Multiply one digit at a time */

   /* the following code give higher performance squaring.
   ** Unfortunately for small nl, procedure call overheads kills it
   */
#ifndef mips_v131
#ifndef MSDOS
    /* Squaring code provoke a mips optimizer bug in V1.31 */
    /* It also doesn't work using MSDOS */
    if (mm == nn && ml == nl && nl > 6)
    {
        register BigNumDigit n_prev = 0;
        /* special case of squaring */
        for (c = 0; nl > 0; )
	{
            register BigNumDigit n = *nn;
            c += BnnMultiplyDigit(pp, pl, nn, 1, n);
            if (n_prev)
                c += BnnAdd(pp, pl, nn, 1, (BigNumCarry) 0);
            nl--, nn++;
            pp += 2, pl -= 2;
            c += BnnMultiplyDigit(pp-1, pl+1, nn, nl, n+n+n_prev);
	     /* note following if statements are resolved at compile time */
	    if (sizeof(BigNumDigit) == sizeof(short))
		n_prev = ((short) n) < 0;
	    else if (sizeof(BigNumDigit) == sizeof(int))
		n_prev = ((int) n) < 0;
	    else if (sizeof(BigNumDigit) == sizeof(long))
		n_prev = ((long) n) < 0;
	    else
		n_prev = ((n<<1)>>1) == n;
	}
   }
   else
#endif
#endif
       for (c = 0; nl-- > 0; pp++, nn++, pl--)
          c += BnnMultiplyDigit (pp, pl, mm, ml, *nn);

   return c;
}

