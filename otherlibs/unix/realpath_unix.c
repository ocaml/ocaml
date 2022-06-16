/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         The OCaml programmers                          */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_REALPATH

CAMLprim value caml_unix_realpath (value p)
{
  CAMLparam1 (p);
  char *r, *path;
  value rp;

  caml_unix_check_path (p, "realpath");
  path = caml_stat_strdup_to_os(String_val(p));
  r = realpath (path, NULL);
  if (r == NULL) { caml_uerror ("realpath", p); }
  rp = caml_copy_string (r);
  free (r);
  caml_stat_free(path);
  CAMLreturn (rp);
}

#else

CAMLprim value caml_unix_realpath (value p)
{ caml_invalid_argument ("realpath not implemented"); }

#endif
