/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                David Allsopp, MetaStack Solutions Ltd.                 */
/*                                                                        */
/*   Copyright 2015 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include <caml/sys.h>
#include "unixsupport.h"

CAMLprim value unix_symlink(value to_dir, value osource, value odest)
{
  CAMLparam3(to_dir, osource, odest);
  BOOLEAN result;
  LPWSTR source;
  LPWSTR dest;
  caml_unix_check_path(osource, "symlink");
  caml_unix_check_path(odest, "symlink");

  if (!caml_win32_init_symlink())
    caml_invalid_argument("symlink not available");

  /* Copy source and dest outside the OCaml heap */
  source = caml_stat_strdup_to_utf16(String_val(osource));
  dest = caml_stat_strdup_to_utf16(String_val(odest));

  caml_enter_blocking_section();
  result = caml_win32_symlink(Bool_val(to_dir), source, dest);
  caml_leave_blocking_section();

  caml_stat_free(source);
  caml_stat_free(dest);

  if (!result) {
    win32_maperr(GetLastError());
    uerror("symlink", odest);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value unix_has_symlink(value unit)
{
  return caml_sys_has_symlink(unit);
}
