/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Mon Jan 23 16:05:27 GMT+1:00 1989 by herve */

/*
 * bzf.c: Miscellaneous functions built on top of BigZ.
 *
 */


#include "BigZ.h"

		/***************************************/

#define BzToBn(z)               ((z)->Digits)

		/***************************************/


#ifndef _NO_PROTO
BigZ BzFactorial (BigZ z)
#else  /* _NO_PROTO */
BigZ BzFactorial (z)
BigZ z;
#endif /* _NO_PROTO */

/*
 * Returns Z!
 * Assumes Z < Base.
 */

{
    BigZ 	f;
    BigNumDigit zval;
    int 	fl = 1;


    zval = BnnGetDigit (BzToBn (z));
    f = BzCreate (zval+1);
    BnnSetDigit (BzToBn (f), 1);
    BzSetSign (f, BzGetSign (z));

    while (zval-- > 1) 
    {
	BnnMultiplyDigit (BzToBn (f), fl+1, BzToBn (f), fl, zval);
	fl = BnnNumDigits (BzToBn (f), fl+1);
    }
    
    return (f);
}

