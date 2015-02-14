/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 File contributed by Lionel Fourquaux                */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include <windows.h>
  
#ifdef UTF16
#include "u8tou16.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#endif
  
#ifdef UTF16
typedef  
BOOL (WINAPI *tCreateHardLink)(
  LPCWSTR lpFileName,
  LPCWSTR lpExistingFileName,
  LPSECURITY_ATTRIBUTES lpSecurityAttributes  
);
#else
typedef  
BOOL (WINAPI *tCreateHardLink)(
  LPCTSTR lpFileName,
  LPCTSTR lpExistingFileName,
  LPSECURITY_ATTRIBUTES lpSecurityAttributes
);
#endif
  
CAMLprim value unix_link(value path1, value path2)
{
  HMODULE hModKernel32;
  tCreateHardLink pCreateHardLink;
#ifdef UTF16
	char * temp1=String_val(path1);
	char * temp2=String_val(path2);
	WCHAR * wtemp1, * wtemp2;
	wtemp1 = to_utf16(temp1);
	wtemp2 = to_utf16(temp2);
#endif
  hModKernel32 = GetModuleHandle("KERNEL32.DLL");
#ifdef UTF16
  pCreateHardLink =
    (tCreateHardLink) GetProcAddress(hModKernel32, "CreateHardLinkW");
#else
  pCreateHardLink =
    (tCreateHardLink) GetProcAddress(hModKernel32, "CreateHardLinkA");
#endif
  if (pCreateHardLink == NULL)
    invalid_argument("Unix.link not implemented");
  caml_unix_check_path(path1, "link");
  caml_unix_check_path(path2, "link");
#ifdef UTF16
	if (! pCreateHardLink(wtemp2, wtemp1, NULL)) {
#else
  if (! pCreateHardLink(String_val(path2), String_val(path1), NULL)) {
#endif
    win32_maperr(GetLastError());
    uerror("link", path2);
  }
#ifdef UTF16
	free(wtemp1);
	free(wtemp2);
#endif
  return Val_unit;
}
