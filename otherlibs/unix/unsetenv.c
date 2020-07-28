/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2020 David Allsopp Ltd.                                    */
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

#include "unixsupport.h"

#if defined(_WIN32) || defined(HAS_SETENV_UNSETENV)

CAMLprim value unix_unsetenv(value name)
{
  char_os * s = NULL;
  int ret;

  if (!(caml_string_is_c_safe(name)))
    unix_error(EINVAL, "unsetenv", name);

#ifdef _WIN32
  /* unix.ml formats and validates name */
  s = caml_stat_strdup_to_os(String_val(name));
  ret = putenv_os(s);
  if (ret == -1) {
    caml_stat_free(s);
  }
#else
  ret = unsetenv(String_val(name));
#endif

  if (ret == -1) {
    if (s) caml_stat_free(s);
    uerror("unsetenv", name);
  }

  return Val_unit;
}

#else

CAMLprim value unix_unsetenv(value name)
{ caml_invalid_argument("unsetenv not implemented"); }

#endif
