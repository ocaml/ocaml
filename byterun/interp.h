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

/* $Id$ */

/* The bytecode interpreter */

#ifndef _interp_
#define _interp_


#include "misc.h"
#include "mlvalues.h"

value interprete P((code_t prog, asize_t prog_size));
value callback P((value closure, value argument));


#endif
