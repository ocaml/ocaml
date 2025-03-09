/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_SYS_H
#define CAML_SYS_H

#ifdef CAML_INTERNALS

#include "misc.h"

CAMLextern char * caml_strerror(int errnum, char * buf, size_t buflen);

#define NO_ARG Val_int(0)

CAMLnoret CAMLextern void caml_sys_error (value);

CAMLnoret CAMLextern void caml_sys_io_error (value);

CAMLextern double caml_sys_time_unboxed(value);
CAMLextern void caml_sys_init (const char_os * exe_name, char_os ** argv);

CAMLnoret CAMLextern void caml_do_exit (int);

#endif /* CAML_INTERNALS */

#endif /* CAML_SYS_H */
