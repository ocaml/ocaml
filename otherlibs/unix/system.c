/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "caml/unixsupport.h"
#include <process.h>
#include <stdio.h>

CAMLprim value caml_unix_system(value command)
{
  CAMLparam1(command);
  CAMLlocal1(st);
  int status;
  wchar_t *buf;

  if (! caml_string_is_c_safe (command))
    caml_unix_error(EINVAL, "system", Nothing);
  buf = caml_stat_strdup_to_utf16(String_val(command));
  caml_enter_blocking_section();
  _flushall();
  status = _wsystem(buf);
  caml_leave_blocking_section();
  caml_stat_free(buf);
  if (status == -1) caml_uerror("system", Nothing);
  st = caml_alloc_small(1, 0); /* Tag 0: Exited */
  Field(st, 0) = Val_int(status);
  CAMLreturn(st);
}
