/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2018 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Stubs for ocamltest's standard library */

#include <stdio.h>
#include <stdlib.h>

#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
/*
#include <caml/fail.h>
*/
#include <caml/signals.h>
#include <caml/osdeps.h>


#ifdef _WIN32

/*
 * Windows Vista functions enabled
 */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600

#include <wtypes.h>
#include <winbase.h>
#include <process.h>
#include <sys/types.h>

#define luid_eq(l, r) (l.LowPart == r.LowPart && l.HighPart == r.HighPart)

CAMLprim value caml_has_symlink(value unit)
{
  CAMLparam1(unit);
  HANDLE hProcess = GetCurrentProcess();
  BOOL result = FALSE;

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


#else /* _WIN32 */

#ifdef HAS_SYMLINK

CAMLprim value caml_has_symlink(value unit)
{
  CAMLparam0();
  CAMLreturn(Val_true);
}

#else /* HAS_SYMLINK */

CAMLprim value unix_symlink(value to_dir, value path1, value path2)
{ caml_invalid_argument("symlink not implemented"); }

CAMLprim value caml_has_symlink(value unit)
{
  CAMLparam0();
  CAMLreturn(Val_false);
}

#endif

#endif /* _WIN32 */
