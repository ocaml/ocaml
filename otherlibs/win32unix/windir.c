/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*   Pascal Cuoq and Xavier Leroy, projet Cristal, INRIA Rocquencourt  */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include "u8tou16.h"

CAMLprim value win_findfirst(value name)
{
  HANDLE h;
  value v;
  value valname = Val_unit;
  value valh = Val_unit;
  WINAPI_(WIN32_FIND_DATA) fileinfo;
  CRT_STR c_name = Crt_str_val(name);

  caml_unix_check_path(name, "opendir");
  Begin_roots2 (valname,valh);
    h = WINAPI_(FindFirstFile)(c_name,&fileinfo);
    Crt_str_free(c_name);
    if (h == INVALID_HANDLE_VALUE) {
      DWORD err = GetLastError();
      if (err == ERROR_NO_MORE_FILES)
        raise_end_of_file();
      else {
        win32_maperr(err);
        uerror("opendir", Nothing);
      }
    }
    valname = caml_copy_crt_str(fileinfo.cFileName);
    valh = win_alloc_handle(h);
    v = alloc_small(2, 0);
    Field(v,0) = valname;
    Field(v,1) = valh;
  End_roots();
  return v;
}

CAMLprim value win_findnext(value valh)
{
  CAMLparam0 ();
  CAMLlocal1 (v);
  WINAPI_(WIN32_FIND_DATA) fileinfo;
  BOOL retcode;

  retcode = WINAPI_(FindNextFile)(Handle_val(valh), &fileinfo);
  if (!retcode) {
    DWORD err = GetLastError();
    if (err == ERROR_NO_MORE_FILES)
      raise_end_of_file();
    else {
      win32_maperr(err);
      uerror("readdir", Nothing);
    }
  }
  v = caml_copy_crt_str(fileinfo.cFileName);
  CAMLreturn(v);
}

CAMLprim value win_findclose(value valh)
{
  if (! FindClose(Handle_val(valh))) {
    win32_maperr(GetLastError());
    uerror("closedir", Nothing);
  }
  return Val_unit;
}
