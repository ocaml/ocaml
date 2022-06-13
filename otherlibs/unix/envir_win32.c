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

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/osdeps.h>

#include <windows.h>

/* Win32 doesn't have a notion of setuid bit. */
CAMLprim value caml_unix_environment(value unit)
{
  CAMLparam0();
  CAMLlocal2(v, result);
  wchar_t * envp, * p;
  int size, i;

  envp = GetEnvironmentStrings();
  for (p = envp, size = 0; *p; p += wcslen(p) + 1) size++;
  result = caml_alloc(size, 0);
  for (p = envp, i = 0; *p; p += wcslen(p) + 1) {
    v = caml_copy_string_of_utf16(p);
    Store_field(result, i ++, v);
  }
  FreeEnvironmentStrings(envp);

  CAMLreturn(result);
}
