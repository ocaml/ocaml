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
#include "unixsupport.h"

#ifdef HAS_GETCWD

#ifndef _WIN32
#include <sys/param.h>
#else
#define MAXPATHLEN 512
#endif

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
  if (getwd(buff) == 0) uerror("getcwd", copy_string(buff));
  return copy_string(buff);
}

#else

value unix_getcwd() { invalid_argument("getcwd not implemented"); }

#endif
#endif
