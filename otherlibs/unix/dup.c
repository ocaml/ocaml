/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unix.h"

value unix_dup(fd)               /* ML */
     value fd;
{
  int ret;
  ret = dup(Int_val(fd));
  if (ret == -1) uerror("dup", Nothing);
  return Val_int(ret);
}
