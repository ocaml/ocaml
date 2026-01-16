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

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "caml/unixsupport.h"

CAMLprim value caml_unix_rename(value oldname, value newname)
{
  char_os * p_old;
  char_os * p_new;
  int ret;
  caml_unix_check_path(oldname, "rename");
  caml_unix_check_path(newname, "rename");
  p_old = caml_stat_strdup_to_os(String_val(oldname));
  p_new = caml_stat_strdup_to_os(String_val(newname));
  caml_enter_blocking_section();
  ret = rename_os(p_old, p_new);
  caml_leave_blocking_section();
  caml_stat_free(p_new);
  caml_stat_free(p_old);
  if (ret == -1)
    caml_uerror("rename", oldname);
  return Val_unit;
}
