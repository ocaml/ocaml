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

#include "misc.h"

#define NO_ARG Val_int(0)

CAMLextern void caml_sys_error (value);
CAMLextern void caml_sys_io_error (value);
CAMLextern double caml_sys_time_unboxed(value);
CAMLextern void caml_sys_init (char * exe_name, char ** argv);
CAMLextern value caml_sys_exit (value);
CAMLextern value caml_sys_get_argv(value unit);

extern char * caml_exe_name;

#endif /* CAML_SYS_H */
