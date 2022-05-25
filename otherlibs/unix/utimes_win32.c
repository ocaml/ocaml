/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                        Nicolas Ojeda Bar, LexiFi                       */
/*                                                                        */
/*   Copyright 2017 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

#include <windows.h>

static void convert_time(double unixTime, FILETIME* ft)
{
  ULARGE_INTEGER u;
  /* There are 11644473600 seconds between 1 January 1601 (the NT Epoch) and 1
   * January 1970 (the Unix Epoch). FILETIME is measured in 100ns ticks.
   */
  u.QuadPart =
    (ULONGLONG)(unixTime * 10000000.0) + INT64_LITERAL(116444736000000000U);
  ft->dwLowDateTime = u.LowPart;
  ft->dwHighDateTime = u.HighPart;
}

CAMLprim value unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  WCHAR *wpath;
  HANDLE hFile;
  FILETIME lastAccessTime, lastModificationTime;
  SYSTEMTIME systemTime;
  double at, mt;
  BOOL res;

  caml_unix_check_path(path, "utimes");
  at = Double_val(atime);
  mt = Double_val(mtime);
  wpath = caml_stat_strdup_to_utf16(String_val(path));
  caml_enter_blocking_section();
  hFile = CreateFile(wpath,
                     FILE_WRITE_ATTRIBUTES,
                     FILE_SHARE_READ | FILE_SHARE_WRITE,
                     NULL,
                     OPEN_EXISTING,
                     FILE_FLAG_BACKUP_SEMANTICS,
                     NULL);
  caml_leave_blocking_section();
  caml_stat_free(wpath);
  if (hFile == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    caml_uerror("utimes", path);
  }
  if (at == 0.0 && mt == 0.0) {
    GetSystemTime(&systemTime);
    SystemTimeToFileTime(&systemTime, &lastAccessTime);
    memcpy(&lastModificationTime, &lastAccessTime, sizeof(FILETIME));
  } else {
    convert_time(at, &lastAccessTime);
    convert_time(mt, &lastModificationTime);
  }
  caml_enter_blocking_section();
  res = SetFileTime(hFile, NULL, &lastAccessTime, &lastModificationTime);
  caml_leave_blocking_section();
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    CloseHandle(hFile);
    caml_uerror("utimes", path);
  }
  CloseHandle(hFile);
  CAMLreturn(Val_unit);
}
