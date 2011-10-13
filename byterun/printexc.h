/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef CAML_PRINTEXC_H
#define CAML_PRINTEXC_H


#include "misc.h"
#include "mlvalues.h"

CAMLextern char * caml_format_exception (value);
void caml_fatal_uncaught_exception (value) Noreturn;


#endif /* CAML_PRINTEXC_H */
