/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Thu Apr  4 20:01:18 GMT+2:00 1991 by herve */
/*      modified_on Thu Mar 22 20:45:38 GMT+1:00 1990 by shand */


/* bz.c: provides an implementation of "unlimited-precision" 
 * arithmetic for signed integers. 
 *
 * Several conventions are used in the commentary:  
 *    A "BigZ" is the name for an arbitrary-precision signed integer.  
 *    Capital letters (e.g., "Z") are used to refer to the value of BigZs.
 */


#include "BigZ.h"


		/***************************************/
/*
#include <stdio.h>
#include <macros.h>
#include <math.h>
#include <malloc.h>
#include <values.h>
*/

#define NULL                    0
#define max(a,b) 		(a<b ? b : a)
#define abs(x)			(x>=0 ? x : -(x))
#define M_LN2	                0.69314718055994530942
#define M_LN10	                2.30258509299404568402
#define BITSPERBYTE	        8
#define BITS(type)	        (BITSPERBYTE * (int)sizeof(type))
#define HIBITI	                (1 << BITS(int) - 1)
#define MAXINT	                (~HIBITI)

		/***************************************/

#define BzToBn(z)               ((z)->Digits)
#define CTOI(c)			(c >= '0' && c <= '9' ? c - '0' :\
                                 c >= 'a' && c <= 'f' ? c - 'a' + 10:\
                                 c >= 'A' && c <= 'F' ? c - 'A' + 10:\
                                 0)

extern char *malloc();

                        /*** copyright ***/

static char copyright[]="@(#)bz.c: copyright Digital Equipment Corporation & INRIA 1988, 1989\n";


		/***************************************/

static int Initialized = FALSE;

/* constants used by BzToString() and BzFromString() */
static double BzLog [] = 
{
    0, 
    0, 			/* log (1) */
    M_LN2,  		/* log (2) */
    1.098612, 		/* log (3) */
    1.386294, 		/* log (4) */
    1.609438, 		/* log (5) */
    1.791759, 		/* log (6) */
    1.945910, 		/* log (7) */
    2.079442, 		/* log (8) */
    2.197225, 		/* log (9) */
    M_LN10, 		/* log (10) */
    2.397895, 		/* log (11) */
    2.484907, 		/* log (12) */
    2.564949, 		/* log (13) */
    2.639057, 		/* log (14) */
    2.708050, 		/* log (15) */
    2.772588, 		/* log (16) */
};    

/**/
    

#ifndef _NO_PROTO
void BzInit (void)
#else  /* _NO_PROTO */
void BzInit ()
#endif /* _NO_PROTO */
{
    if (!Initialized)
    {
        BnnInit ();
        Initialized = TRUE;
    }
}

		/***************************************/


#ifndef _NO_PROTO
BigZ BzCreate (BigNumLength Size)
#else  /* _NO_PROTO */
BigZ BzCreate (Size)
BigNumLength Size;
#endif /* _NO_PROTO */

/* 
 * Allocates a BigZ of the desired size.
 * Sets it to 0.
 */

{
    BigZ z;


    if ((z = (BigZ) (malloc (sizeof (struct BigZHeader) + Size * sizeof (BigNumDigit)))) != NULL)
    {
	/* reset digits */
	BnnSetToZero (BzToBn (z), Size);

	/* init header */
	BzSetSize (z, Size);
	BzSetSign (z, BZ_ZERO);
    }

    return (z);
}



#ifndef _NO_PROTO
void BzFree (BigZ z)	
#else  /* _NO_PROTO */
void BzFree (z)	
BigZ z;
#endif /* _NO_PROTO */

/*
 * Frees an existing BigZ.
 */

{
    free (z);
}

		/***************************************/
		/***************************************/


#ifndef _NO_PROTO
void BzFreeString (char *s)	
#else  /* _NO_PROTO */
void BzFreeString (s)	
char *s;
#endif /* _NO_PROTO */

/*
 * Frees an existing BigZ allocated string.
 */

{
    free (s);
}

		/***************************************/
/**/

#ifndef _NO_PROTO
BigNumLength BzNumDigits (BigZ z)
#else  /* _NO_PROTO */
BigNumLength BzNumDigits (z)
BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns the number of digits used by z.
 */

{
    return (BnnNumDigits (BzToBn (z), BzGetSize (z)));
}


		/***************************************/


#ifndef _NO_PROTO
BigZ BzCopy (BigZ z)	
#else  /* _NO_PROTO */
BigZ BzCopy (z)	
BigZ z;
#endif /* _NO_PROTO */

/* 
 * Creates a copy of the passed BigZ.
 */

{
    BigZ y;
    int zl;

   
    zl = BzNumDigits (z);
    if ((y = BzCreate (zl)) != NULL)
    {
	/* copy the digits */
        BnnAssign (BzToBn (y), BzToBn (z), zl);

	/* copy the header WITHOUT the size !! */
	BzSetSign (y, BzGetSign (z));
    }

    return (y);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzNegate (BigZ z)	
#else  /* _NO_PROTO */
BigZ BzNegate (z)	
BigZ z;
#endif /* _NO_PROTO */

/* 
 * Negates the passed BigZ.
 */

{
    BigZ y;

    y = BzCopy (z);
    BzSetSign (y, BzGetOppositeSign (z));

    return (y);
}

		/***************************************/


#ifndef _NO_PROTO
BigZ BzAbs (BigZ z)	
#else  /* _NO_PROTO */
BigZ BzAbs (z)	
BigZ z;
#endif /* _NO_PROTO */

/* 
 * Takes the absolute value of the passed BigZ.
 */

{
    BigZ y;

    y = BzCopy (z);
    BzSetSign (y, abs (BzGetSign (z)));

    return (y);
}

		/***************************************/


#ifndef _NO_PROTO
BzCmp BzCompare (BigZ y, BigZ z)
#else  /* _NO_PROTO */
BzCmp BzCompare (y, z)
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns BZ_GT	if Y > Z,
 *         BZ_LT	if Y < Z, 
 *         BZ_EQ	otherwise.
 */

{
    return (BzGetSign (y) > BzGetSign (z) ? BZ_GT :
	    BzGetSign (y) < BzGetSign (z) ? BZ_LT :
	    BzGetSign (y) > 0 ? BnnCompare (BzToBn (y), BzGetSize (y),
					    BzToBn (z), BzGetSize (z)) :
	    BzGetSign (y) < 0 ? BnnCompare (BzToBn (z), BzGetSize (z),
					    BzToBn (y), BzGetSize (y)) :
	    BZ_EQ);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzAdd (BigZ y, BigZ z)
#else  /* _NO_PROTO */
BigZ BzAdd (y, z)
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns Y + Z.
 */

{
    BigZ n;
    int yl;
    int zl;


    yl = BzNumDigits (y);
    zl = BzNumDigits (z);

    if (BzGetSign (y) == BzGetSign (z))
    {
	/* Add magnitudes if signs are the same */
        switch (BnnCompare (BzToBn (y), yl, BzToBn (z), zl))
	{
	    case BZ_EQ:
	    case BZ_GT:	/* |Y| >= |Z| */

	        if ((n = BzCreate (yl+1)) != NULL)
		{
		    BnnAssign (BzToBn (n), BzToBn (y), yl);
		    BnnAdd (BzToBn (n), yl+1, BzToBn (z), zl, (BigNumCarry) 0);
		    BzSetSign (n, BzGetSign (y));
		}
	    break;

	    default:		/* BZ_LT: |Y| < |Z| */

		if ((n = BzCreate (zl+1)) != NULL)
		{
		    BnnAssign (BzToBn (n), BzToBn (z), zl);
		    BnnAdd (BzToBn (n), zl+1, BzToBn (y), yl, (BigNumCarry) 0);
		    BzSetSign (n, BzGetSign (z));
		}
	    break;
	}
    }
/**/


    else
    {
	/* Subtract magnitudes if signs are different */
	switch (BnnCompare (BzToBn (y), yl, BzToBn (z), zl))
	{
	    case BZ_EQ:	/* Y = -Z */

	        n = BzCreate (1);
	    break;

	    case BZ_GT:	/* |Y| > |Z| */

		if ((n = BzCreate (yl)) != NULL)
		{
		    BnnAssign (BzToBn (n), BzToBn (y), yl);
		    BnnSubtract (BzToBn (n), yl, BzToBn (z), zl, (BigNumCarry) 1);
		    BzSetSign (n, BzGetSign (y));
		}
	    break;

	    default:		/* BZ_LT: |Y| < |Z| */

		if ((n = BzCreate (zl)) != NULL)
		{
		    BnnAssign (BzToBn (n), BzToBn (z), zl);
		    BnnSubtract (BzToBn (n), zl, BzToBn (y), yl, (BigNumCarry) 1);
		    BzSetSign (n, BzGetSign (z));
		}
	    break;
	}
    }

    return (n);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzSubtract (BigZ y, BigZ z)	
#else  /* _NO_PROTO */
BigZ BzSubtract (y, z)	
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns Y - Z.
 */

{
    if (y == z)
        return (BzCreate (1));
    else
    {
	BigZ diff;

	BzSetSign (z, BzGetOppositeSign (z));
	diff = BzAdd (y, z);
	BzSetSign (z, BzGetOppositeSign (z));

	return diff;
    }
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzMultiply (BigZ y, BigZ z)	
#else  /* _NO_PROTO */
BigZ BzMultiply (y, z)	
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns Y * Z.
 */

{
    BigZ n;
    int yl, zl;


    yl = BzNumDigits (y);
    zl = BzNumDigits (z);

    if ((n = BzCreate (yl+zl)) != NULL)
    {
	BnnMultiply (BzToBn (n), yl+zl, BzToBn (y), yl, BzToBn (z), zl);
	BzSetSign (n, BzGetSign (y) * BzGetSign (z));
    }

    return (n);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzDivide (BigZ y, BigZ z, BigZ *r)	
#else  /* _NO_PROTO */
BigZ BzDivide (y, z, r)	
BigZ y; BigZ z; BigZ *r;
#endif /* _NO_PROTO */

/* 
 * Sets Y mod Z => R, 
 * Returns Y div Z => Q
 * 
 * such that Y = ZQ + R 
 * and 0 <= R < |Z|.
 *
 * Return NULL if Z = 0
 *
 * Return floor(Y/Z) if Z > 0 
 * otherwise return ceil(Y/Z)
 * where / is the real numbers division.
 */

{
    BigZ 	q;
    int 	yl, zl, ql, rl;
    Boolean     rnotnul;


    if (BzGetSign (z) == BZ_ZERO)
        return (NULL);

    yl = BzNumDigits (y);
    zl = BzNumDigits (z);

    /* max +1 since BnnAddCarry can overflow */
    ql = max (yl-zl+1, 1) +1;
    rl = max (zl,yl) + 1;

    /* Set up quotient, remainder */
    q = BzCreate (ql); 
    *r = BzCreate (rl);
    
    if (!*r || !q) 
        return (NULL);

    BnnAssign (BzToBn (*r), BzToBn (y), yl);
    
    /* Do the division */
    BnnDivide (BzToBn (*r), rl, BzToBn (z), zl);
    BnnAssign (BzToBn (q), BzToBn (*r) + zl, rl-zl);
    BnnSetToZero (BzToBn (*r) + zl, rl-zl);
    rl = zl;

    /* Correct the signs, adjusting the quotient and remainder */
    rnotnul = !BnnIsZero (BzToBn (*r), rl);
    if (BzGetSign (y) == BZ_MINUS && rnotnul)
    {
        /* Y < 0, R > 0: (Q+1)=>Q,  Z-R=>R */
        BnnAddCarry (BzToBn (q), ql, (BigNumCarry) 1);

        BzSetSign (q, BzGetOppositeSign (z));
        BnnComplement (BzToBn (*r), rl);
        BnnAdd (BzToBn (*r), rl, BzToBn (z), zl, (BigNumCarry) 1);
    }
    else
        BzSetSign (q, BzGetSign (y) * BzGetSign (z));

    if (BnnIsZero (BzToBn(q),ql))
	BzSetSign (q,BZ_ZERO);

    /* Correct the sign of the remainder */
    if (rnotnul)
        BzSetSign (*r, BZ_PLUS);

    return (q);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzDiv (BigZ y, BigZ z)		
#else  /* _NO_PROTO */
BigZ BzDiv (y, z)		
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns Y div Z.
 *
 * Return NULL if Z = 0
 *
 * Return floor(Y/Z) if Z > 0 
 * otherwise return ceil(Y/Z)
 * where / is the real numbers division
 */

{
    BigZ q, r;


    q = BzDivide (y, z, &r);
    BzFree (r);

    return (q);
}

		/***************************************/


#ifndef _NO_PROTO
BigZ BzMod (BigZ y, BigZ z)		
#else  /* _NO_PROTO */
BigZ BzMod (y, z)		
BigZ y; BigZ z;
#endif /* _NO_PROTO */

/* 
 * Returns Y mod Z.
 */

{
    BigZ r;


    BzFree (BzDivide (y, z, &r));

    return (r);
}

		/***************************************/
/**/


#ifndef _NO_PROTO
char * BzToString (BigZ z, BigNumDigit base)
#else  /* _NO_PROTO */
char * BzToString (z, base)
BigZ z; BigNumDigit base;
#endif /* _NO_PROTO */

/*
 * Returns a pointer to a string that represents Z in the specified base.
 * Assumes 2 <= base <= 16.
 */

{
    char *	string;
    BigZ 	y, q, t;
    BigNumDigit r;

    static char Digit[] = "0123456789ABCDEF";
    char *	s;
    int 	sd;
    int 	zl, sl;


    if (base < 2 || base > 16)
        return (NULL);

    /* Allocate BigNums and set up string */
    zl = BzNumDigits (z) + 1;
    sl = BzLog[2] * BN_DIGIT_SIZE * zl / BzLog[base] + 3;

    y = BzCreate (zl);
    q = BzCreate (zl);

    string = malloc (sl * sizeof (char));

    if (!y || !q || !string)
        return (NULL);

    BnnAssign (BzToBn (y), BzToBn (z), zl-1);
    s = string + sl;

    /* Divide Z by base repeatedly; successive digits given by remainders */
    *--s = '\0';
    if (BzGetSign (z) == BZ_ZERO) 
        *--s = '0';
    else
    do
    {
	r = BnnDivideDigit (BzToBn (q), BzToBn (y), zl, base);
	*--s = Digit[r];

	/* exchange y and q (to avoid BzMove (y, q) */
	t = q,  q = y,  y = t;
    } while (!BnnIsZero (BzToBn (y), zl));
    
    /* Set sign if negative */
    if (BzGetSign (z) < 0) 
        *--s = '-';

    /* and move string into position */
    if ((sd = s-string) > 0) 
        while (s < string + sl)
	{
	    *(s-sd) = *s;
	    s++;
	}

    /* Free temporary BigNums and return the string */
    BzFree(y);
    BzFree(q);

    return string;
}

		/***************************************/
/**/


#ifndef _NO_PROTO
BigZ BzFromString (char *s, BigNumDigit base)
#else  /* _NO_PROTO */
BigZ BzFromString (s, base)
char *s; BigNumDigit base;
#endif /* _NO_PROTO */

/*
 * Creates a BigZ whose value is represented by "string" in the
 * specified base.  The "string" may contain leading spaces,
 * followed by an optional sign, followed by a series of digits.
 * Assumes 2 <= base <= 16.
 * When called from C, only the first 2 arguments are passed.
 */

{
    BigZ 	z, p, t;
    BzSign 	sign;
    int 	zl;

    
    /* Throw away any initial space */
    while (*s == ' ') 
        s++;
    
    /* Allocate BigNums */
    zl = strlen (s) * BzLog[base] / (BzLog[2] * BN_DIGIT_SIZE) + 1;

    z = BzCreate (zl);
    p = BzCreate (zl);

    if (!z || !p) 
        return (NULL);
    
    /* Set up sign, base, initialize result */
    sign = (*s == '-' ? (s++, BZ_MINUS) : *s == '+' ? (s++, BZ_PLUS) : BZ_PLUS);
    
    /* Multiply in the digits of the string, one at a time */
    for (;  *s != '\0';  s++)
    {
	BnnSetToZero (BzToBn (p), zl);
	BnnSetDigit (BzToBn (p), CTOI (*s));
	BnnMultiplyDigit (BzToBn (p), zl, BzToBn (z), zl, base);  

	/* exchange z and p (to avoid BzMove (z, p) */
	t = p,  p = z,  z = t;
    }
    
    /* Set sign of result */
    BzSetSign (z, BnnIsZero (BzToBn (z), zl) ? BZ_ZERO : sign);
    
    /* Free temporary BigNums */
    BzFree (p);

    return (z);
}

		/***************************************/

#ifndef _NO_PROTO
BigZ BzFromInteger (int i)
#else  /* _NO_PROTO */
BigZ BzFromInteger (i)
int i;
#endif /* _NO_PROTO */

{
    BigZ z;


    z = BzCreate (1);

    z->Digits[0] = abs (i);

    if (i > 0) 
        BzSetSign (z, BZ_PLUS);
    else
    if (i < 0) 
        BzSetSign (z, BZ_MINUS);
    else
        BzSetSign (z, BZ_ZERO);

    return z;
}

		/***************************************/


#ifndef _NO_PROTO
int BzToInteger (BigZ z)
#else  /* _NO_PROTO */
int BzToInteger (z)
BigZ z;
#endif /* _NO_PROTO */

{
    if (BzNumDigits (z) > 1)
        return (MAXINT);

    if (BzGetSign (z) == BZ_MINUS)
        return (- z->Digits[0]);
    else
        return (z->Digits[0]);
}

		/***************************************/


#ifndef _NO_PROTO
BigZ BzFromBigNum (BigNum n, BigNumLength nl)
#else  /* _NO_PROTO */
BigZ BzFromBigNum (n, nl)
BigNum n; BigNumLength nl;
#endif /* _NO_PROTO */

{
    BigZ z;
    int  i;


    z = BzCreate (nl);

    /* set the sign of z such that the pointer n is unchanged yet */
    if (BnnIsZero (n, nl))
        BzSetSign (z, BZ_ZERO);
    else
        BzSetSign (z, BZ_PLUS);

    for (i = 0; i < nl; i++, n++)
        z->Digits[i] = *n;

    return z;
}

		/***************************************/

#ifndef _NO_PROTO
BigNum BzToBigNum (BigZ z, BigNumLength *nl)
#else  /* _NO_PROTO */
BigNum BzToBigNum (z, nl)
BigZ z; BigNumLength *nl;
#endif /* _NO_PROTO */

{
    BigNum n, m;
    int i;


    if (BzGetSign (z) == BZ_MINUS)
        return NULL;

    *nl = BzNumDigits (z);

    if ((n = (BigNum) (malloc (((*nl+1) * sizeof (BigNumDigit))))) != NULL)
    {
        *n = *nl; /* set size */

        for (i = 0, m = ++n; i < *nl; i++, m++)
            *m = z->Digits[i];
    }

    return n;
}

		/***************************************/


#ifndef _NO_PROTO
void BzClose (void)
#else  /* _NO_PROTO */
void BzClose ()
#endif /* _NO_PROTO */
{
    if (Initialized)
    {
        BnnClose ();
        Initialized = FALSE;
    }
}

		/***************************************/
