/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "unixsupport.h"

#ifdef HAS_PUTENV

CAMLprim value unix_putenv(value name, value val)
{
  mlsize_t namelen = caml_string_length(name);
  mlsize_t vallen = caml_string_length(val);
  char * s;

  if (! (caml_string_is_c_safe(name) && caml_string_is_c_safe(val)))
    unix_error(EINVAL, "putenv", name);
  s = (char *) caml_stat_alloc(namelen + 1 + vallen + 1);
  memmove (s, String_val(name), namelen);
  s[namelen] = '=';
  memmove (s + namelen + 1, String_val(val), vallen);
  s[namelen + 1 + vallen] = 0;
  if (putenv(s) == -1) {
    caml_stat_free(s);
    uerror("putenv", name);
  }
  return Val_unit;
}

#else

CAMLprim value unix_putenv(value name, value val)
{ caml_invalid_argument("putenv not implemented"); }

#endif
