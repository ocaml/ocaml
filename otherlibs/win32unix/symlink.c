/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*               David Allsopp, MetaStack Solutions Ltd.               */
/*                                                                     */
/*  Copyright 2015 MetaStack Solutions Ltd.  All rights reserved.      */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file ../../LICENSE.                                   */
/*                                                                     */
/***********************************************************************/

/*
 * Windows Vista functions enabled
 */
#define _WIN32_WINNT 0x0600

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_symlink(value to_dir, value source, value dest)
{
  CAMLparam3(to_dir, source, dest);
  DWORD flags = (Bool_val(to_dir) ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0);
  BOOL result;

  caml_enter_blocking_section();
  result = CreateSymbolicLink(String_val(dest), String_val(source), flags);
  caml_leave_blocking_section();

  if (!result) {
    win32_maperr(GetLastError());
    uerror("symlink", dest);
  }

  CAMLreturn(Val_unit);
}
