/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* Based on public-domain code from Berkeley Yacc */

/* $Id$ */

#include "defs.h"

void transitive_closure(R, n)
        unsigned int *R;
        int n;
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rowj;
    register unsigned *rp;
    register unsigned *rend;
    register unsigned *ccol;
    register unsigned *relend;
    register unsigned *cword;
    register unsigned *rowi;

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    cword = R;
    mask = 1;
    rowi = R;
    while (rowi < relend)
    {
	ccol = cword;
	rowj = R;

	while (rowj < relend)
	{
	    if (*ccol & mask)
	    {
		rp = rowi;
		rend = rowj + rowsize;
		while (rowj < rend)
		    *rowj++ |= *rp++;
	    }
	    else
	    {
		rowj += rowsize;
	    }

	    ccol += rowsize;
	}

	mask <<= 1;
	if (mask == 0)
	{
	    mask = 1;
	    cword++;
	}

	rowi += rowsize;
    }
}

void reflexive_transitive_closure(R, n)
        unsigned int *R;
        int n;
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rp;
    register unsigned *relend;

    transitive_closure(R, n);

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    mask = 1;
    rp = R;
    while (rp < relend)
    {
	*rp |= mask;
	mask <<= 1;
	if (mask == 0)
	{
	    mask = 1;
	    rp++;
	}

	rp += rowsize;
    }
}
