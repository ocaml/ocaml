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

#ifdef HAS_TRUNCATE

value unix_truncate(path, len)   /* ML */
     value path, len;
{
  if (truncate(String_val(path), Long_val(len)) == -1)
    uerror("truncate", path);
  return Val_unit;
}

#else

value unix_truncate() { invalid_argument("truncate not implemented"); }

#endif
