/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                David Allsopp, MetaStack Solutions Ltd.                 */
/*                                                                        */
/*   Copyright 2015 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/*
 * Windows Vista functions enabled
 */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

#ifndef SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
#define SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE (0x2)
#endif

typedef BOOLEAN (WINAPI *LPFN_CREATESYMBOLICLINK) (LPWSTR, LPWSTR, DWORD);

static LPFN_CREATESYMBOLICLINK pCreateSymbolicLink = NULL;
static int no_symlink = 0;
static DWORD additional_symlink_flags = 0;

// Developer Mode allows the creation of symlinks without elevation - see
// https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createsymboliclinkw
static BOOL IsDeveloperModeEnabled()
{
  HKEY hKey;
  LSTATUS status;
  DWORD developerModeRegistryValue, dwordSize = sizeof(DWORD);

  status = RegOpenKeyExW(
    HKEY_LOCAL_MACHINE,
    L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\AppModelUnlock",
    0,
    KEY_READ | KEY_WOW64_64KEY,
    &hKey
  );
  if (status != ERROR_SUCCESS) {
    return FALSE;
  }

  status = RegQueryValueExW(
    hKey,
    L"AllowDevelopmentWithoutDevLicense",
    NULL,
    NULL,
    (LPBYTE)&developerModeRegistryValue,
    &dwordSize
  );
  RegCloseKey(hKey);
  if (status != ERROR_SUCCESS) {
    return FALSE;
  }
  return developerModeRegistryValue != 0;
}

CAMLprim value unix_symlink(value to_dir, value osource, value odest)
{
  CAMLparam3(to_dir, osource, odest);
  DWORD flags;
  BOOLEAN result;
  LPWSTR source;
  LPWSTR dest;
  caml_unix_check_path(osource, "symlink");
  caml_unix_check_path(odest, "symlink");

again:
  if (no_symlink) {
    caml_invalid_argument("symlink not available");
  }

  if (!pCreateSymbolicLink) {
    if (!(pCreateSymbolicLink = (LPFN_CREATESYMBOLICLINK)GetProcAddress(GetModuleHandle(L"kernel32"), "CreateSymbolicLinkW"))) {
      no_symlink = 1;
    } else if (IsDeveloperModeEnabled()) {
      additional_symlink_flags = SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
    }

    goto again;
  }

  flags = (Bool_val(to_dir) ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0) | additional_symlink_flags;

  /* Copy source and dest outside the OCaml heap */
  source = caml_stat_strdup_to_utf16(String_val(osource));
  dest = caml_stat_strdup_to_utf16(String_val(odest));

  caml_enter_blocking_section();
  result = pCreateSymbolicLink(dest, source, flags);
  caml_leave_blocking_section();

  caml_stat_free(source);
  caml_stat_free(dest);

  if (!result) {
    caml_win32_maperr(GetLastError());
    caml_uerror("symlink", odest);
  }

  CAMLreturn(Val_unit);
}

#define luid_eq(l, r) (l.LowPart == r.LowPart && l.HighPart == r.HighPart)

CAMLprim value unix_has_symlink(value unit)
{
  CAMLparam1(unit);
  HANDLE hProcess = GetCurrentProcess();
  BOOL result = FALSE;

  if (IsDeveloperModeEnabled()) {
    CAMLreturn(Val_true);
  }

  if (OpenProcessToken(hProcess, TOKEN_READ, &hProcess)) {
    LUID seCreateSymbolicLinkPrivilege;

    if (LookupPrivilegeValue(NULL,
                             SE_CREATE_SYMBOLIC_LINK_NAME,
                             &seCreateSymbolicLinkPrivilege)) {
      DWORD length;

      if (!GetTokenInformation(hProcess, TokenPrivileges, NULL, 0, &length)) {
        if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
          TOKEN_PRIVILEGES* privileges = (TOKEN_PRIVILEGES*)caml_stat_alloc(length);
          if (GetTokenInformation(hProcess,
                                  TokenPrivileges,
                                  privileges,
                                  length,
                                  &length)) {
            DWORD count = privileges->PrivilegeCount;

            if (count) {
              LUID_AND_ATTRIBUTES* privs = privileges->Privileges;
              while (count-- && !(result = luid_eq(privs->Luid, seCreateSymbolicLinkPrivilege)))
                privs++;
            }
          }

          caml_stat_free(privileges);
        }
      }
    }

    CloseHandle(hProcess);
  }

  CAMLreturn(Val_bool(result));
}
