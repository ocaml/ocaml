/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Fri Oct  5 16:28:39 GMT+1:00 1990 by herve */
/*      modified_on Fri Mar 30  3:28:56 GMT+2:00 1990 by shand */


/* bnInit.c: a piece of the bignum kernel written in C */


		/***************************************/

#define BNNMACROS_OFF
#include "BigNum.h"

static int Initialized = FALSE;

                        /*** copyright ***/

static char copyright[]="@(#)bnInit.c: copyright Digital Equipment Corporation & INRIA 1988, 1989, 1990\n";


		/***************************************/

void BnnInit ()
{
    if (!Initialized)
    {


        Initialized = TRUE;
    }
}

		/***************************************/

void BnnClose ()
{
    if (Initialized)
    {


        Initialized = FALSE;
    }
}

		/***************************************/

		/* some U*ix standard functions do not exist on VMS */
		/* neither on MSDOS */

#ifdef NOMEM

/* Copies LENGTH bytes from string SRC to string DST */
void bcopy(src, dst, length)
char	*src, *dst;
register int	length;
{
    for (; length > 0; length--)
	*dst++ = *src++;
}

/* Places LENGTH 0 bytes in the string B */
void bzero(buffer, length)
char	*buffer;
register int	length;
{
    for (;  length>0; length--)
	*buffer++ = 0;
}

#endif



		/***************************************/
