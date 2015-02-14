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
#ifdef UTF16
#include "u8tou16.h"
#endif

CAMLprim value win_findfirst(value name)
{
  HANDLE h;
  value v;
  value valname = Val_unit;
  value valh = Val_unit;
#ifdef UTF16
	WIN32_FIND_DATAW fileinfo;
	char * tempo, *temp=String_val(name);
	WCHAR * wtemp;
	wtemp = to_utf16(temp);
#else
  WIN32_FIND_DATA fileinfo;
#endif

  caml_unix_check_path(name, "opendir");
  Begin_roots2 (valname,valh);
#ifdef UTF16
    h = FindFirstFileW(wtemp,&fileinfo);
#else
    h = FindFirstFile(String_val(name),&fileinfo);
#endif
    if (h == INVALID_HANDLE_VALUE) {
      DWORD err = GetLastError();
      if (err == ERROR_NO_MORE_FILES)
        raise_end_of_file();
      else {
        win32_maperr(err);
        uerror("opendir", Nothing);
      }
    }
#ifdef UTF16
	tempo = utf16_to_utf8(fileinfo.cFileName);
	valname = copy_string(tempo);
	free(tempo);
#else
    valname = copy_string(fileinfo.cFileName);
#endif
    valh = win_alloc_handle(h);
    v = alloc_small(2, 0);
    Field(v,0) = valname;
    Field(v,1) = valh;
  End_roots();
#ifdef UTF16
	free(wtemp);
#endif
  return v;
}

CAMLprim value win_findnext(value valh)
{
#ifdef UTF16
	CAMLparam0 ();
	CAMLlocal1 (v);
	WIN32_FIND_DATAW fileinfo;
	char * temp;
#else
  WIN32_FIND_DATA fileinfo;
#endif
  BOOL retcode;

#ifdef UTF16
  retcode = FindNextFileW(Handle_val(valh), &fileinfo);
#else
  retcode = FindNextFile(Handle_val(valh), &fileinfo);
#endif
  if (!retcode) {
    DWORD err = GetLastError();
    if (err == ERROR_NO_MORE_FILES)
      raise_end_of_file();
    else {
      win32_maperr(err);
      uerror("readdir", Nothing);
    }
  }
#ifdef UTF16
	temp = utf16_to_utf8(fileinfo.cFileName);
	v=copy_string(temp);
	free(temp);
	CAMLreturn (v);
#else
  return copy_string(fileinfo.cFileName);
#endif
}

CAMLprim value win_findclose(value valh)
{
  if (! FindClose(Handle_val(valh))) {
    win32_maperr(GetLastError());
    uerror("closedir", Nothing);
  }
  return Val_unit;
}
