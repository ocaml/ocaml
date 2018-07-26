/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2018 Institut National de Recherche en Informatique et     */
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
#include "unixsupport.h"

#include <windows.h>

int win_truncate(char * path, long long len)
{
  HANDLE fh;
  LARGE_INTEGER fp;
  fp.QuadPart = len;
  fh = CreateFile(path, GENERIC_WRITE, 0, NULL,
                  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL); 
  if (fh == INVALID_HANDLE_VALUE) {  // failure: Unable to open file
    return -1;
  }
  if (SetFilePointerEx(fh, fp, NULL, FILE_BEGIN) == 0 ||
      SetEndOfFile(fh) == 0) {
    CloseHandle(fh);
    return -1;
  }
  CloseHandle(fh);
  return 0;
}

CAMLprim value unix_truncate(value path, value len)
{
  CAMLparam2(path, len);
  char * p;
  int ret;
  caml_unix_check_path(path, "truncate");
  p = caml_stat_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = win_truncate(p, Long_val(len));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("truncate", path);
  CAMLreturn(Val_unit);
}

CAMLprim value unix_truncate_64(value path, value vlen)
{
  CAMLparam2(path, vlen);
  char * p;
  int ret;
  file_offset len = File_offset_val(vlen);
  caml_unix_check_path(path, "truncate");
  p = caml_stat_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = win_truncate(p, len);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("truncate", path);
  CAMLreturn(Val_unit);
}
