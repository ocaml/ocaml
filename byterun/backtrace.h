/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
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

#ifndef CAML_BACKTRACE_H
#define CAML_BACKTRACE_H

#include "mlvalues.h"

CAMLextern int backtrace_active;
CAMLextern int backtrace_pos;
CAMLextern code_t * backtrace_buffer;
CAMLextern value backtrace_last_exn;

extern void init_backtrace(void);
extern void stash_backtrace(value exn, code_t pc, value * sp);
CAMLextern void print_exception_backtrace(void);

#endif /* CAML_BACKTRACE_H */
