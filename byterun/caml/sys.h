/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_SYS_H
#define CAML_SYS_H

#include "misc.h"

#define NO_ARG Val_int(0)

CAMLextern void caml_sys_error (value);
CAMLextern void caml_sys_io_error (value);
CAMLextern value caml_sys_exit (value);

#endif /* CAML_SYS_H */
