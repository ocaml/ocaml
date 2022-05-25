/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                            Florent Monnier                             */
/*                       Nicolas Ojeda Bar, LexiFi                        */
/*                                                                        */
/*   Copyright 2019 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <sys/types.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/io.h>
#include <caml/osdeps.h>
#include "unixsupport.h"
#include <windows.h>

static int truncate_handle(HANDLE fh, __int64 len)
{
  LARGE_INTEGER fp;
  fp.QuadPart = len;
  if (SetFilePointerEx(fh, fp, NULL, FILE_BEGIN) == 0 ||
      SetEndOfFile(fh) == 0) {
    caml_win32_maperr(GetLastError());
    return -1;
  }
  return 0;
}

static int ftruncate(HANDLE fh, __int64 len)
{
  HANDLE dupfh, currproc;
  int ret;
  currproc = GetCurrentProcess();
  /* Duplicate the handle, so we are free to modify its file position. */
  if (DuplicateHandle(currproc, fh, currproc, &dupfh, 0, FALSE,
                      DUPLICATE_SAME_ACCESS) == 0) {
    caml_win32_maperr(GetLastError());
    return -1;
  }
  ret = truncate_handle(dupfh, len);
  CloseHandle(dupfh);
  return ret;
}

static int truncate(WCHAR * path, __int64 len)
{
  HANDLE fh;
  int ret;
  fh = CreateFile(path, GENERIC_WRITE, 0, NULL,
                  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (fh == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    return -1;
  }
  ret = truncate_handle(fh, len);
  CloseHandle(fh);
  return ret;
}

CAMLprim value unix_truncate(value path, value len)
{
  CAMLparam2(path, len);
  WCHAR * p;
  int ret;
  caml_unix_check_path(path, "truncate");
  p = caml_stat_strdup_to_utf16(String_val(path));
  caml_enter_blocking_section();
  ret = truncate(p, Long_val(len));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    caml_uerror("truncate", path);
  CAMLreturn(Val_unit);
}

CAMLprim value unix_truncate_64(value path, value vlen)
{
  CAMLparam2(path, vlen);
  WCHAR * p;
  int ret;
  __int64 len = Int64_val(vlen);
  caml_unix_check_path(path, "truncate");
  p = caml_stat_strdup_to_utf16(String_val(path));
  caml_enter_blocking_section();
  ret = truncate(p, len);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    caml_uerror("truncate", path);
  CAMLreturn(Val_unit);
}

CAMLprim value unix_ftruncate(value fd, value len)
{
  int ret;
  HANDLE h = Handle_val(fd);
  caml_enter_blocking_section();
  ret = ftruncate(h, Long_val(len));
  caml_leave_blocking_section();
  if (ret == -1)
    caml_uerror("ftruncate", Nothing);
  return Val_unit;
}

CAMLprim value unix_ftruncate_64(value fd, value vlen)
{
  int ret;
  HANDLE h = Handle_val(fd);
  __int64 len = Int64_val(vlen);
  caml_enter_blocking_section();
  ret = ftruncate(h, len);
  caml_leave_blocking_section();
  if (ret == -1)
    caml_uerror("ftruncate", Nothing);
  return Val_unit;
}
