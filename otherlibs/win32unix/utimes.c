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

/* Used to prevent race conditions when disabling SeRestorePrivilege */
static LONG processing = 0;
static BOOL restore_privilege = FALSE;

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
  HANDLE hFile, hToken = INVALID_HANDLE_VALUE;
  TOKEN_PRIVILEGES restore;
  DWORD dwFlags = 0;
  FILETIME lastAccessTime, lastModificationTime;
  SYSTEMTIME systemTime;

  double at, mt;
  BOOL res = FALSE;

  caml_unix_check_path(path, "utimes");
  at = Double_val(atime);
  mt = Double_val(mtime);
  wpath = caml_stat_strdup_to_utf16(String_val(path));

  /* Directories require special handling for CreateFile */
  if (((GetFileAttributes(wpath) & FILE_ATTRIBUTE_DIRECTORY) != 0)) {
    LUID seRestorePrivilege;
    DWORD dwReturnLength;

    /* FILE_FLAG_BACKUP_SEMANTICS is required to get a handle to a directory */
    dwFlags = FILE_FLAG_BACKUP_SEMANTICS;

    /* However, we don't want to access a directory which we can only see
       through SeRestorePrivilege, so check if it's enabled. There are two
       possible race conditions:
         - Another call may restore SeRestorePrivilege between
           GetTokenInformation and CreateFile
         - Another call may restore SeRestorePrivilege between
           AdjustTokenPrivileges and CreateFile
     */

    LookupPrivilegeValue(NULL, SE_RESTORE_NAME, &seRestorePrivilege);
    /* -4 is the pseudo-handle for GetCurrentProcessToken, which is not
       present in the mingw-w64 headers */
    if (!GetTokenInformation((HANDLE)(LONG_PTR)-4,
                             TokenPrivileges,
                             NULL, 0, &dwReturnLength)
        && GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
      TOKEN_PRIVILEGES* privs;

      if (!(privs = (TOKEN_PRIVILEGES*)malloc(dwReturnLength)))
        caml_raise_out_of_memory();

      if (GetTokenInformation((HANDLE)(LONG_PTR)-4,
                              TokenPrivileges, privs,
                              dwReturnLength, &dwReturnLength)) {
        int i = 0;
        LUID_AND_ATTRIBUTES* privilege = privs->Privileges;

        /* Search for SeRestorePrivilege */
        while (i < privs->PrivilegeCount
               && (privilege->Luid.HighPart != seRestorePrivilege.HighPart
                   || privilege->Luid.LowPart != seRestorePrivilege.LowPart)) {
          i++;
          privilege++;
        }

        /* If SeRestorePrivilege is present, then open the process token */
        if (i < privs->PrivilegeCount) {
          if (OpenProcessToken(GetCurrentProcess(),
                               TOKEN_ADJUST_PRIVILEGES,
                               &hToken)) {
            restore.PrivilegeCount = 1;
            restore.Privileges->Luid = seRestorePrivilege;
            restore.Privileges->Attributes = 0;
            /* The first thread records whether ultimately the privilege must be
               restored, but all threads will disable it (since we can't know
               which will reach AdjustTokenPrivilege first) */
            if (InterlockedIncrement(&processing) == 1)
              restore_privilege =
                (privilege->Attributes & SE_PRIVILEGE_ENABLED);
          } else {
            win32_maperr(GetLastError());
            caml_stat_free(wpath);
            uerror("utimes", path);
          }
        } else {
          CloseHandle(hToken);
          hToken = INVALID_HANDLE_VALUE;
        }
      } else {
        win32_maperr(GetLastError());
        caml_stat_free(wpath);
        uerror("utimes", path);
      }
      free(privs);
    } else {
      win32_maperr(GetLastError());
      caml_stat_free(wpath);
      uerror("utimes", path);
    }
  }

  /* Disable SeRestorePrivilege, if necessary */
  if (hToken != INVALID_HANDLE_VALUE
      && !AdjustTokenPrivileges(hToken, FALSE,
                                &restore, sizeof(TOKEN_PRIVILEGES),
                                NULL, NULL)) {
    win32_maperr(GetLastError());
  } else {
    caml_enter_blocking_section();
    hFile = CreateFile(wpath,
                       FILE_WRITE_ATTRIBUTES,
                       FILE_SHARE_READ | FILE_SHARE_WRITE,
                       NULL,
                       OPEN_EXISTING,
                       dwFlags,
                       NULL);
    caml_leave_blocking_section();
    caml_stat_free(wpath);
    if (hFile != INVALID_HANDLE_VALUE) {
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
      if (!res)
        win32_maperr(GetLastError());
      CloseHandle(hFile);
    } else {
      win32_maperr(GetLastError());
    }
  }
  if (hToken != INVALID_HANDLE_VALUE) {
    /* restore_privilege must be read before processing == 0 */
    BOOL will_restore = restore_privilege;
    if (InterlockedDecrement(&processing) == 0 && will_restore) {
      /* Restore SeRestorePrivilege */
      restore.Privileges->Attributes = SE_PRIVILEGE_ENABLED;
      AdjustTokenPrivileges(hToken, FALSE,
                            &restore, sizeof(TOKEN_PRIVILEGES), NULL, NULL);
    }
    CloseHandle(hToken);
  }
  if (!res)
    uerror("utimes", path);
  CAMLreturn(Val_unit);
}
