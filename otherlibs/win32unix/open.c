/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, 0, O_APPEND, O_CREAT, O_TRUNC, O_EXCL, 0, 0
};

static int open_text_flag_table[] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1
};

value unix_open(path, flags, perm) /* ML */
     value path, flags, perm;
{
  int fl, ret;

  fl = convert_flag_list(flags, open_flag_table);
  if (convert_flag_list(flags, open_text_flag_table) == 0) fl |= O_BINARY;
  ret = open(String_val(path), fl, Int_val(perm));
  if (ret == -1) uerror("open", path);
  return Val_int(ret);
}
