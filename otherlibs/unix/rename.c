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

value unix_rename(path1, path2)  /* ML */
     value path1, path2;
{
  if (rename(String_val(path1), String_val(path2)) == -1)
    uerror("rename", path1);
  return Val_unit;
}
