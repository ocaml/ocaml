/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static int seek_command_table[] = {
  FILE_BEGIN, FILE_CURRENT, FILE_END
};

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

CAMLprim value unix_lseek(value fd, value ofs, value cmd)
{
  long ret;
  long ofs_low = Long_val(ofs);
  long ofs_high = ofs_low >= 0 ? 0 : -1;
  long err;

  ret = SetFilePointer(Handle_val(fd), ofs_low, &ofs_high,
                       seek_command_table[Int_val(cmd)]);
  if (ret == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("lseek", Nothing);
    }
  }
  if (ofs_high != 0 || ret > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("lseek", Nothing);
  }
  return Val_long(ret);
}

CAMLprim value unix_lseek_64(value fd, value ofs, value cmd)
{
  long ret;
  long ofs_low = (long) Int64_val(ofs);
  long ofs_high = (long) (Int64_val(ofs) >> 32);
  long err;

  ret = SetFilePointer(Handle_val(fd), ofs_low, &ofs_high,
                       seek_command_table[Int_val(cmd)]);
  if (ret == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("lseek", Nothing);
    }
  }
  return copy_int64((int64) ofs_high << 32 | ret);
}
