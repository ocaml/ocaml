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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#ifdef HAS_GETGROUPS

#include <sys/types.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <errno.h>
#include "caml/unixsupport.h"

CAMLprim value caml_unix_getgroups(value unit)
{
  gid_t* gidset = NULL;
  int n, ngroups_max;
  value res;

  while (1) {
    if ((ngroups_max = getgroups(0, NULL)) == -1) {
      if (gidset != NULL) caml_stat_free(gidset);
      caml_uerror("getgroups", Nothing);
    }
    gidset = caml_stat_resize(gidset, ngroups_max * sizeof(gid_t));
    if ((n = getgroups(ngroups_max, gidset)) == -1) {
      if (errno == EINVAL) continue; // result size changed, retry
      caml_stat_free(gidset);
      caml_uerror("getgroups", Nothing);
    }
    break;
  }
  res = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++)
    Field(res, i) = Val_int(gidset[i]);
  caml_stat_free(gidset);
  return res;
}

#else

CAMLprim value caml_unix_getgroups(value unit)
{ caml_invalid_argument("getgroups not implemented"); }

#endif
