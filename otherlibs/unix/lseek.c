/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};

value unix_lseek(fd, ofs, cmd)   /* ML */
     value fd, ofs, cmd;
{
  long ret;
  ret = lseek(Int_val(fd), Long_val(ofs),
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) uerror("lseek", Nothing);
  return Val_long(ret);
}
