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

#include <errno.h>
#include <sys/types.h>
#include <mlvalues.h>
#include <alloc.h>
#include <io.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifndef EOVERFLOW
#define EOVERFLOW ERANGE
#endif

static int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};

CAMLprim value unix_lseek(value fd, value ofs, value cmd)
{
  file_offset ret;
  ret = lseek(Int_val(fd), Long_val(ofs),
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) uerror("lseek", Nothing);
  if (ret > Max_long) unix_error(EOVERFLOW, "lseek", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_lseek_64(value fd, value ofs, value cmd)
{
  file_offset ret;
  ret = lseek(Int_val(fd), File_offset_val(ofs),
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) uerror("lseek", Nothing);
  return Val_file_offset(ret);
}

