/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Thu Feb 20 18:41:41 GMT+1:00 1992 by shand */
/*      modified_on Thu Oct 31 16:41:47 1991 by herve */
/*      modified_on Wed Jul  5 10:19:33 GMT+2:00 1989 by bertin */
/* Adapted to Caml Light by Xavier Leroy, Mon May 9. */

/* BigN.h - Types and structures for clients of BigNum */

#if !defined(_stdc_)
#define _NO_PROTO
#endif


		/******** representation of a bignum ******/
/*
**  <--------------------------- nl ---------------------------->
**  |   Least                                           Most    |
**  |Significant|           |           |           |Significant|
**  |BigNumDigit|           |           |           |BigNumDigit|
**  |___________|___________|___________|___________|___________|
**        ^                                          (sometimes
**        |                                            is zero)
**       nn
*/

/* signals BigNum.h already included */
#define BIGNUM

		/*************** sizes ********************/

#define BN_BYTE_SIZE			8
#ifdef CAML_LIGHT
#define BN_WORD_SIZE			(sizeof (long) * BN_BYTE_SIZE - 2)
#else
#define BN_WORD_SIZE			(sizeof (int) * BN_BYTE_SIZE)
#endif
#define BN_DIGIT_SIZE			(sizeof (BigNumDigit) * BN_BYTE_SIZE)

/* notes: */
/* BN_BYTE_SIZE: number of bits in a byte */
/* BN_WORD_SIZE: number of bits in an "int" in the target language */
/* BN_DIGIT_SIZE: number of bits in a digit of a BigNum */


		/****** results of compare functions ******/

 /* Note: we don't use "enum" to interface with Modula2+, Lisp, ... */
#define BN_LT				-1
#define BN_EQ				0
#define BN_GT				1

		/*************** boolean ******************/

#define TRUE				1
#define FALSE				0

typedef unsigned long			BigNumDigit;

#ifndef BigZBoolean
typedef int				Boolean;
#define BigZBoolean
#endif

#ifndef __
#if defined(_NO_PROTO)
#define __(args) ()
#else
#define __(args) args
#endif
#endif

		/* bignum types: digits, big numbers, carries ... */

typedef BigNumDigit * 	BigNum;		/* A big number is a digit pointer */
typedef BigNumDigit	BigNumCarry;	/* Either 0 or 1 */
typedef unsigned long 	BigNumProduct;	/* The product of two digits */
/* BigNumLength must be int as nl is in the code, remember int is 16 bits on MSDOS - jch */
typedef unsigned long	BigNumLength;	/* The length of a bignum */
typedef int		BigNumCmp;	/* result of comparison */

/**/


		/************ functions of bn.c ***********/

extern void             BnnInit 	__((void));
extern void             BnnClose 	__((void));

extern Boolean		BnnIsZero 	__((BigNum nn, BigNumLength nl));
extern BigNumCarry 	BnnMultiply	__((BigNum pp,BigNumLength pl, BigNum nn, BigNumLength nl, BigNum mm, BigNumLength ml));
extern void		BnnDivide	__((BigNum nn, BigNumLength nl, BigNum dd, BigNumLength dl));
extern BigNumCmp	BnnCompare	__((BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl));

		/*********** functions of KerN.c **********/
extern void 		BnnSetToZero	__((BigNum nn, BigNumLength nl));
extern void 		BnnAssign	__((BigNum mm, BigNum nn, BigNumLength nl));
extern void 		BnnSetDigit	__((BigNum nn, BigNumDigit d));
extern BigNumDigit 	BnnGetDigit	__((BigNum nn));
extern BigNumLength	BnnNumDigits	__((BigNum nn, BigNumLength nl));
extern BigNumDigit	BnnNumLeadingZeroBitsInDigit	__((BigNumDigit d));
extern Boolean 		BnnDoesDigitFitInWord 		__((BigNumDigit d));
extern Boolean		BnnIsDigitZero 	__((BigNumDigit d));
extern Boolean		BnnIsDigitNormalized 		__((BigNumDigit d));
extern Boolean 		BnnIsDigitOdd	__((BigNumDigit d));
extern BigNumCmp	BnnCompareDigits __((BigNumDigit d1, BigNumDigit d2));
extern void 		BnnComplement	__((BigNum nn, BigNumLength nl));
extern void 		BnnAndDigits	__((BigNum n, BigNumDigit d));
extern void		BnnOrDigits	__((BigNum n, BigNumDigit d));
extern void		BnnXorDigits	__((BigNum n, BigNumDigit d));
extern BigNumDigit	BnnShiftLeft	__((BigNum mm, BigNumLength ml, int nbits));
extern BigNumDigit	BnnShiftRight	__((BigNum mm, BigNumLength ml, int nbits));
extern BigNumCarry 	BnnAddCarry	__((BigNum nn, BigNumLength nl, BigNumCarry carryin));
extern BigNumCarry 	BnnAdd		__((BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl, BigNumCarry carryin));
extern BigNumCarry 	BnnSubtractBorrow __((BigNum nn, BigNumLength nl, BigNumCarry carryin));
extern BigNumCarry 	BnnSubtract	__((BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl, BigNumCarry carryin));
extern BigNumCarry 	BnnMultiplyDigit __((BigNum mm, BigNumLength ml, BigNum nn, BigNumLength nl, BigNumDigit d));
extern BigNumDigit	BnnDivideDigit	__((BigNum qq, BigNum nn, BigNumLength nl, BigNumDigit d));

/**/

		/* some functions can be written with macro-procedures */


#ifndef BNNMACROS_OFF
/* the functions BnnIsZero and BnnCompareDigits are not macro procedures
   since they use parameters twice, and that can produce bugs if
   you pass a parameter like x++
 */
#define BnnSetDigit(nn,d) 		(*(nn) = (d))
#define BnnGetDigit(nn)			(*(nn))
#define BnnDoesDigitFitInWord(d)	(BN_DIGIT_SIZE > BN_WORD_SIZE ? ((d) >= (BigNumDigit)1 << BN_WORD_SIZE ? FALSE : TRUE) : TRUE)
#define BnnIsDigitZero(d)		((d) == 0)
#define BnnIsDigitNormalized(d)		((d) & (((BigNumDigit) 1) << (BN_DIGIT_SIZE - 1)) ? TRUE : FALSE)
#define BnnIsDigitOdd(d) 		((d) & ((BigNumDigit) 1) ? TRUE : FALSE)
#define BnnAndDigits(nn, d)		(*(nn) &= (d))
#define BnnOrDigits(nn, d)		(*(nn) |= (d))
#define BnnXorDigits(nn, d)		(*(nn) ^= (d))

#endif


#ifdef MSDOS
#define realaddr(p)  ((((long)(p) & (65535 << 16)) >> 12)+((long)(p) & 65535))
#endif
