/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The bytecode interpreter */

#ifndef CAML_INTERP_H
#define CAML_INTERP_H

/* cvs $Id$ */

#include "misc.h"
#include "mlvalues.h"

/* interpret a bytecode */
value caml_interprete (code_t prog, asize_t prog_size);

/* tell the runtime that a bytecode program is no more needed */
void caml_clear_bytecode(code_t prog, asize_t prog_size);

#endif /* CAML_INTERP_H */
