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
#include <alloc.h>

#ifdef HAS_SYMLINK

#include <sys/param.h>
#include "unixsupport.h"

value unix_readlink(value path)        /* ML */
{
  char buffer[MAXPATHLEN];
  int len;
  len = readlink(String_val(path), buffer, sizeof(buffer) - 1);
  if (len == -1) uerror("readlink", path);
  buffer[len] = '\0';
  return copy_string(buffer);
}

#else

value unix_readlink(value path)
{ invalid_argument("readlink not implemented"); }

#endif
