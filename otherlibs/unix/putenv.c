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

#define CAML_INTERNALS

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>

#include "caml/unixsupport.h"

#ifdef HAS_PUTENV

CAMLprim value caml_unix_putenv(value name, value val)
{
  char * s;
  char_os * p;
  int ret;

  if (! (caml_string_is_c_safe(name) && caml_string_is_c_safe(val)))
    caml_unix_error(EINVAL, "putenv", name);
  s = caml_stat_strconcat(3, name, "=", val);
  p = caml_stat_strdup_to_os(s);
  caml_stat_free(s);
  ret = putenv_os(p);
  if (ret == -1) {
    caml_stat_free(p);
    caml_uerror("putenv", name);
  }
  return Val_unit;
}

#else

CAMLprim value caml_unix_putenv(value name, value val)
{ caml_invalid_argument("putenv not implemented"); }

#endif

#if defined(_WIN32) || defined(HAS_SETENV_UNSETENV)

CAMLprim value caml_unix_unsetenv(value name)
{
  int ret;

  if (! caml_string_is_c_safe(name))
    caml_unix_error(EINVAL, "unsetenv", name);
#ifdef _WIN32
  char_os * s = caml_stat_strdup_to_utf16(String_val(name));
  ret = _wputenv_s(s, L"");
  caml_stat_free(s);
#else
  ret = unsetenv(String_val(name));
#endif
  if (ret == -1) {
    caml_uerror("unsetenv", name);
  }
  return Val_unit;
}

#else

CAMLprim value caml_unix_unsetenv(value name)
{ caml_invalid_argument("unsetenv not implemented"); }

#endif
