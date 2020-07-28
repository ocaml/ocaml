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

#ifdef HAS_SETENV_UNSETENV

CAMLprim value unix_setenv(value overwrite, value name, value val)
{
  int ret;

  if (!(caml_string_is_c_safe(name) && caml_string_is_c_safe(val)))
    unix_error(EINVAL, "setenv", name);

  ret = setenv(String_val(name),
               String_val(val),
               (Is_long(overwrite) || Val_bool(Some_val(overwrite))));

  if (ret == -1) {
    uerror("setenv", name);
  }

  return Val_unit;
}

#else

CAMLprim value unix_setenv(value overwrite, value name, value val)
{ caml_invalid_argument("setenv not implemented"); }

#endif
