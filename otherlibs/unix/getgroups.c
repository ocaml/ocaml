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
#include <alloc.h>

#ifdef HAS_GETGROUPS

#include <sys/types.h>
#include <sys/param.h>
#include "unix.h"

value unix_getgroups()           /* ML */
{
  int gidset[NGROUPS];
  int n;
  value res;
  int i;

  n = getgroups(NGROUPS, gidset);
  if (n == -1) uerror("getgroups", Nothing);
  res = alloc_tuple(n);
  for (i = 0; i < n; i++)
    Field(res, i) = Val_int(gidset[i]);
  return res;
}

#else

value unix_getgroups() { invalid_argument("getgroups not implemented"); }

#endif
