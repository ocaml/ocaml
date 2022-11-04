/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static DWORD seek_command_table[] = {
  FILE_BEGIN, FILE_CURRENT, FILE_END
};

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

CAMLprim value caml_unix_lseek(value fd, value ofs, value cmd)
{
  LARGE_INTEGER i;
  HANDLE h = Handle_val(fd);
  BOOL success;

  i.QuadPart = Long_val(ofs);

  caml_enter_blocking_section();
  success = SetFilePointerEx(h, i, &i, seek_command_table[Int_val(cmd)]);
  caml_leave_blocking_section();

  if (!success) {
    caml_win32_maperr(GetLastError());
    caml_uerror("lseek", Nothing);
  }
  if (i.QuadPart > Max_long) {
    caml_win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    caml_uerror("lseek", Nothing);
  }
  return Val_long(i.QuadPart);
}

CAMLprim value caml_unix_lseek_64(value fd, value ofs, value cmd)
{
  LARGE_INTEGER i;
  HANDLE h = Handle_val(fd);
  BOOL success;

  i.QuadPart = Long_val(ofs);

  caml_enter_blocking_section();
  success = SetFilePointerEx(h, i, &i, seek_command_table[Int_val(cmd)]);
  caml_leave_blocking_section();

  if (!success) {
    caml_win32_maperr(GetLastError());
    caml_uerror("lseek", Nothing);
  }
  return caml_copy_int64(i.QuadPart);
}
