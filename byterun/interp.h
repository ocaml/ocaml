/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The bytecode interpreter */

#ifndef _interp_
#define _interp_


#include "misc.h"
#include "mlvalues.h"

value interprete (code_t prog, asize_t prog_size);


#endif
