/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>   */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"
#ifdef UTF16
#include "u8tou16.h"
#endif

CAMLprim value unix_rename(value path1, value path2)
{
  static int supports_MoveFileEx = -1; /* don't know yet */
  BOOL ok;

  caml_unix_check_path(path1, "rename");
  caml_unix_check_path(path2, "rename");
#ifdef UTF16
	char * temp1=String_val(path1);
	char * temp2=String_val(path2);
	WCHAR * wtemp1, * wtemp2;
	if(is_valid_utf8(temp1))
		wtemp1 = utf8_to_utf16(temp1);
	else
		wtemp1 = ansi_to_utf16(temp1);
	if(is_valid_utf8(temp2))
		wtemp2 = utf8_to_utf16(temp2);
	else
		wtemp2 = ansi_to_utf16(temp2);
#endif
  if (supports_MoveFileEx < 0) {
    OSVERSIONINFO VersionInfo;
    VersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    supports_MoveFileEx =
      (GetVersionEx(&VersionInfo) != 0)
      && (VersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
  }
  if (supports_MoveFileEx > 0)
#ifdef UTF16
	ok = MoveFileExW(wtemp1, wtemp2,
		    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
		    MOVEFILE_COPY_ALLOWED);
#else
    ok = MoveFileEx(String_val(path1), String_val(path2),
                    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                    MOVEFILE_COPY_ALLOWED);
#endif
  else
#ifdef UTF16
	ok = MoveFileW(wtemp1, wtemp2);
#else
    ok = MoveFile(String_val(path1), String_val(path2));
#endif
#ifdef UTF16
	free(wtemp1);
	free(wtemp2);
#endif
  if (! ok) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }
  return Val_unit;
}
