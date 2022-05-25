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

#ifndef _WIN32
#include <sys/types.h>
#include <sys/stat.h>
#endif

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_mkdir(value path, value perm)
{
  CAMLparam2(path, perm);
  char_os * p;
  int ret;
  caml_unix_check_path(path, "mkdir");
  p = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();
  ret = mkdir_os(p, Int_val(perm));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) caml_uerror("mkdir", path);
  CAMLreturn(Val_unit);
}
