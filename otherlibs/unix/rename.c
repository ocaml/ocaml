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

#include <stdio.h>
#include <mlvalues.h>
#include "unixsupport.h"

value unix_rename(path1, path2)  /* ML */
     value path1, path2;
{
  if (rename(String_val(path1), String_val(path2)) == -1)
    uerror("rename", path1);
  return Val_unit;
}
