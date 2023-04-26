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
#include <caml/winsupport.h>
#include "unixsupport.h"

#include <windows.h>

CAMLprim value caml_unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  WCHAR *wpath;
  HANDLE hFile;
  CAML_ULONGLONG_FILETIME lastAccessTime, lastModificationTime;
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
    SystemTimeToFileTime(&systemTime, &lastAccessTime.ft);
    lastModificationTime.ul = lastAccessTime.ul;
  } else {
    lastAccessTime.ul =
      (ULONGLONG)(at * 10000000.0) + CAML_NT_EPOCH_100ns_TICKS;
    lastModificationTime.ul =
      (ULONGLONG)(mt * 10000000.0) + CAML_NT_EPOCH_100ns_TICKS;
  }
  caml_enter_blocking_section();
  res = SetFileTime(hFile, NULL, &lastAccessTime.ft, &lastModificationTime.ft);
  caml_leave_blocking_section();
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    CloseHandle(hFile);
    caml_uerror("utimes", path);
  }
  CloseHandle(hFile);
  CAMLreturn(Val_unit);
}
