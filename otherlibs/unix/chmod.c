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

value unix_chmod(path, perm)     /* ML */
     value path, perm;
{
  int ret;
  ret = chmod(String_val(path), Int_val(perm));
  if (ret == -1) uerror("chmod", path);
  return Val_unit;
}
