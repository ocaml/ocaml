/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         The OCaml programmers                          */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
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
#include <caml/osdeps.h>
#include "unixsupport.h"

#include <windows.h>
#include <stdio.h>

CAMLprim value caml_unix_realpath (value p)
{
  CAMLparam1 (p);
  HANDLE h;
  wchar_t *wp;
  wchar_t *wr;
  DWORD wr_len;
  value rp;

  caml_unix_check_path (p, "realpath");
  wp = caml_stat_strdup_to_utf16 (String_val (p));
  h = CreateFile (wp, 0,
                  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                  OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  caml_stat_free (wp);

  if (h == INVALID_HANDLE_VALUE)
  {
    caml_win32_maperr (GetLastError ());
    caml_uerror ("realpath", p);
  }

  wr_len = GetFinalPathNameByHandle (h, NULL, 0, VOLUME_NAME_DOS);
  if (wr_len == 0)
  {
    caml_win32_maperr (GetLastError ());
    CloseHandle (h);
    caml_uerror ("realpath", p);
  }

  wr = caml_stat_alloc ((wr_len + 1) * sizeof (wchar_t));
  wr_len = GetFinalPathNameByHandle (h, wr, wr_len, VOLUME_NAME_DOS);

  if (wr_len == 0)
  {
    caml_win32_maperr (GetLastError ());
    CloseHandle (h);
    caml_stat_free (wr);
    caml_uerror ("realpath", p);
  }

  rp = caml_copy_string_of_utf16 (wr);
  CloseHandle (h);
  caml_stat_free (wr);
  CAMLreturn (rp);
}
