/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Fri Oct  5 16:45:46 GMT+1:00 1990 by herve */
/*      modified_on Thu Mar 22 21:29:09 GMT+1:00 1990 by shand */

/* BigZ.h: Types and structures for clients of BigZ */
 

       			/* BigZ sign */


#define BZ_PLUS				1
#define BZ_ZERO				0
#define BZ_MINUS			-1
#define BzSign	        		BigNumCmp


       			/* BigZ compare result */


#define BZ_LT				BN_LT
#define BZ_EQ				BN_EQ
#define BZ_GT				BN_GT
#define BzCmp				BigNumCmp


       			/* BigZ number */

#ifndef BIGNUM
#include "BigNum.h"
#endif

struct BigZHeader
{
    BigNumLength			Size;
    BzSign				Sign;
};


struct BigZStruct
{
    struct BigZHeader			Header;
    BigNumDigit 			Digits [16];
};


typedef struct BigZStruct * 		BigZ;

/**/


		/*********** macros of bz.c **********/


#define BzGetSize(z)			((BigNumLength)(z)->Header.Size)
#define BzGetSign(z)			((z)->Header.Sign)

#define BzSetSize(z,s)			(z)->Header.Size = s
#define BzSetSign(z,s)			(z)->Header.Sign = s

#define BzGetOppositeSign(z)		(-(z)->Header.Sign)


		/*********** functions of bz.c **********/

extern void	BzInit			__((void));
extern void	BzClose			__((void));

extern BigZ	BzCreate		__((BigNumLength));
extern void	BzFree			__((BigZ));
extern void	BzFreeString		__((char *));

extern BigNumLength BzNumDigits		__((BigZ));

extern BigZ	BzCopy			__((BigZ));
extern BigZ	BzNegate		__((BigZ));
extern BigZ	BzAbs			__((BigZ));
extern BigNumCmp BzCompare		__((BigZ, BigZ));

extern BigZ	BzAdd			__((BigZ, BigZ));
extern BigZ	BzSubtract		__((BigZ, BigZ));
extern BigZ	BzMultiply		__((BigZ, BigZ));
extern BigZ	BzDivide		__((BigZ, BigZ, BigZ *));
extern BigZ	BzDiv			__((BigZ, BigZ));
extern BigZ	BzMod			__((BigZ, BigZ));

extern BigZ	BzFromString		__((char *, BigNumDigit));
extern char *	BzToString		__((BigZ, BigNumDigit));

extern BigZ	BzFromInteger		__((int));
extern int	BzToInteger		__((BigZ));

extern BigZ	BzFromBigNum		__((BigNum, BigNumLength));
extern BigNum	BzToBigNum		__((BigZ, BigNumLength *));

		/*********** functions of bzf.c **********/

extern BigZ 	BzFactorial		__((BigZ));
