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
#include <sys/param.h>
#include "unix.h"

#ifdef HAS_GETHOSTNAME

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

value unix_gethostname()         /* ML */
{
  char name[MAXHOSTNAMELEN];
  gethostname(name, MAXHOSTNAMELEN);
  name[MAXHOSTNAMELEN-1] = 0;
  return copy_string(name);
}

#else
#ifdef HAS_UNAME

#include <sys/utsname.h>

value unix_gethostname()
{
  struct utsname un;
  uname(&un);
  return copy_string(un.nodename);
}

#else

value unix_gethostname() { invalid_argument("gethostname not implemented"); }

#endif
#endif
