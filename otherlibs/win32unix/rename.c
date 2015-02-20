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
#include "u8tou16.h"

CAMLprim value unix_rename(value path1, value path2)
{
  static int supports_MoveFileEx = -1; /* don't know yet */
  BOOL ok;

  caml_unix_check_path(path1, "rename");
  caml_unix_check_path(path2, "rename");
  CRT_STR p1 = Crt_str_val(path1);
  CRT_STR p2 = Crt_str_val(path2);
  if (supports_MoveFileEx < 0) {
    OSVERSIONINFO VersionInfo;
    VersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    supports_MoveFileEx =
      (GetVersionEx(&VersionInfo) != 0)
      && (VersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
  }
  if (supports_MoveFileEx > 0)
    ok = WINAPI_(MoveFileEx)(p1, p2,
		    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
		    MOVEFILE_COPY_ALLOWED);
  else
    ok = WINAPI_(MoveFile)(p1, p2);
  Crt_str_free(p1);
  Crt_str_free(p2);
  if (!ok) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }
  return Val_unit;
}
