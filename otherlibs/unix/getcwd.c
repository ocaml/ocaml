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
#include "unix.h"

#ifdef HAS_GETCWD

#include <sys/param.h>

value unix_getcwd()     /* ML */
{
  char buff[MAXPATHLEN];
  if (getcwd(buff, sizeof(buff)) == 0) uerror("getcwd", Nothing);
  return copy_string(buff);
}

#else
#ifdef HAS_GETWD

#include <sys/param.h>

value unix_getcwd()
{
  char buff[MAXPATHLEN];
  if (getwd(buff) == 0) uerror("getcwd", buff);
  return copy_string(buff);
}

#else

value unix_getcwd() { invalid_argument("getcwd not implemented"); }

#endif
#endif
