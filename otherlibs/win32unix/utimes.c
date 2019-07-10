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
       through SeRestorePrivilege, so ensure it's disabled when CreateFile is
       called and restore it if necessary afterwards.

       As always, this isn't easy...
     */

    /* Step 1: get the effective privileges for this thread */
    LookupPrivilegeValue(NULL, SE_RESTORE_NAME, &seRestorePrivilege);
    /* -6 is the pseudo-handle for GetCurrentThreadEffectiveToken, which is not
       present in the mingw-w64 headers */
    if (!GetTokenInformation((HANDLE)(LONG_PTR)-6,
                             TokenPrivileges,
                             NULL, 0, &dwReturnLength)
        && GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
      TOKEN_PRIVILEGES* privs;

      if (!(privs = (TOKEN_PRIVILEGES*)malloc(dwReturnLength)))
        caml_raise_out_of_memory();

      if (GetTokenInformation((HANDLE)(LONG_PTR)-6,
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

        if (i < privs->PrivilegeCount) {
          HANDLE hProcessToken = INVALID_HANDLE_VALUE;
          restore.PrivilegeCount = 1;
          restore.Privileges->Luid = seRestorePrivilege;
          restore.Privileges->Attributes = 0;

          /* If this thread already has a token, adjust it, otherwise duplicate
             the process's token, adjust that and impersonate it on this thread
           */
          if (OpenThreadToken(GetCurrentThread(),
                              TOKEN_ADJUST_PRIVILEGES,
                              TRUE,
                              &hToken)) {
            /* This thread has a token, but the privilege isn't enabled so we
               can assume that it will remain disabled for the duration of this
               call (or at least we can't prevent another thread mucking around)
             */
            if ((privilege->Attributes & SE_PRIVILEGE_ENABLED) == 0) {
              CloseHandle(hToken);
              restore.PrivilegeCount = 0;
            } else {
              /* Adjust the privileges of this thread */
              if (!AdjustTokenPrivileges(hToken, FALSE,
                                         &restore, sizeof(TOKEN_PRIVILEGES),
                                         NULL, NULL)) {
                win32_maperr(GetLastError());
                CloseHandle(hToken);
                res = TRUE;
              }
            }
          } else if (OpenProcessToken(GetCurrentProcess(),
                                      TOKEN_ADJUST_PRIVILEGES | TOKEN_DUPLICATE,
                                      &hProcessToken)
                  && DuplicateTokenEx(hProcessToken,
                                      TOKEN_ADJUST_PRIVILEGES
                                        | TOKEN_IMPERSONATE, NULL,
                                      SecurityImpersonation, TokenImpersonation,
                                      &hToken)
                  && AdjustTokenPrivileges(hToken, FALSE,
                                           &restore, sizeof(TOKEN_PRIVILEGES),
                                           NULL, NULL)
                  && SetThreadToken(NULL, hToken)) {
            /* Success - the token has been duplicated, adjusted and
               impersonated, so close the handle. Setting hToken to
               INVALID_HANDLE_VALUE signals the cleanup code to call
               RevertToSelf */
            CloseHandle(hProcessToken);
            CloseHandle(hToken);
            hToken = INVALID_HANDLE_VALUE;
          } else {
            /* An API call, somewhere, failed! */
            win32_maperr(GetLastError());
            if (hProcessToken != INVALID_HANDLE_VALUE)
              CloseHandle(hProcessToken);
            res = TRUE;
          }
        }
      } else {
        win32_maperr(GetLastError());
        res = TRUE;
      }
      free(privs);
    } else {
      win32_maperr(GetLastError());
      res = TRUE;
    }
    if (res) {
      caml_stat_free(wpath);
      if (hToken != INVALID_HANDLE_VALUE) CloseHandle(hToken);
      uerror("utimes", path);
    }
  } else {
    restore.PrivilegeCount = 0;
  }

  /* Attempt to open the file */
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

  /* The cleanup code after this block must always be called */
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

  /* If necessary, re-enable SeRestorePrivilege for the thread, or revert the
     impersonation */
  if (restore.PrivilegeCount != 0) {
    if (hToken != INVALID_HANDLE_VALUE) {
      restore.Privileges->Attributes = SE_PRIVILEGE_ENABLED;
      AdjustTokenPrivileges(hToken, FALSE,
                            &restore, sizeof(TOKEN_PRIVILEGES), NULL, NULL);
      CloseHandle(hToken);
    } else {
      RevertToSelf();
    }
  }

  if (!res)
    uerror("utimes", path);

  CAMLreturn(Val_unit);
}
