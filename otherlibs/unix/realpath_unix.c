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
#include "caml/unixsupport.h"

#ifdef HAS_REALPATH

CAMLprim value caml_unix_realpath (value p)
{
  CAMLparam1 (p);
  char *path, *r;
  value rp;

  caml_unix_check_path (p, "realpath");
  path = caml_stat_strdup(String_val(p));
  caml_enter_blocking_section();
  r = realpath (path, NULL);
  caml_leave_blocking_section();
  caml_stat_free(path);
  if (r == NULL) { caml_uerror ("realpath", p); }
  rp = caml_copy_string (r);
  free (r);
  CAMLreturn (rp);
}

#else

CAMLprim value caml_unix_realpath (value p)
{ caml_invalid_argument ("realpath not implemented"); }

#endif
