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

/* Needed to get linkat exposed in compliant OS.
   Must be defined before the first system .h is included. */
#define _XOPEN_SOURCE 700

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

CAMLprim value unix_link(value follow, value path1, value path2)
{
  CAMLparam3(follow, path1, path2);
  char * p1;
  char * p2;
  int ret;
  caml_unix_check_path(path1, "link");
  caml_unix_check_path(path2, "link");
  p1 = caml_stat_strdup(String_val(path1));
  p2 = caml_stat_strdup(String_val(path2));
  caml_enter_blocking_section();
  if (Is_none(follow))
    ret = link(p1, p2);
  else {
# ifdef AT_SYMLINK_FOLLOW
    int flags =
      Is_some(follow) && Bool_val(Some_val(follow))
      ? AT_SYMLINK_FOLLOW
      : 0;
    ret = linkat(AT_FDCWD, p1, AT_FDCWD, p2, flags);
# else
    ret = -1; errno = ENOSYS;
# endif
  }
  caml_leave_blocking_section();
  caml_stat_free(p1);
  caml_stat_free(p2);
  if (ret == -1) caml_uerror("link", path2);
  CAMLreturn(Val_unit);
}
