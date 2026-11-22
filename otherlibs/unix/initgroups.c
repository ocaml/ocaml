/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Stephane Glondu <steph@glondu.net>                    */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
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

#ifdef HAS_INITGROUPS

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <grp.h>
#include "caml/unixsupport.h"

CAMLprim value caml_unix_initgroups(value user, value group)
{
  gid_t group_c =
    caml_unix_gid_val(group, "Unix.initgroups: gid out of range");
  if (! caml_string_is_c_safe(user))
    caml_unix_error(EINVAL, "initgroups", user);
  if (initgroups(String_val(user), group_c) == -1) {
    caml_uerror("initgroups", Nothing);
  }
  return Val_unit;
}

#else

CAMLprim value caml_unix_initgroups(value user, value group)
{ caml_invalid_argument("initgroups not implemented"); }

#endif
