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
#include <memory.h>
#include "unixsupport.h"

extern char ** cstringvect();

value unix_execve(path, args, env)     /* ML */
     value path, args, env;
{
  char ** argv;
  char ** envp;
  argv = cstringvect(args);
  envp = cstringvect(env);
  (void) execve(String_val(path), argv, envp);
  stat_free((char *) argv);
  stat_free((char *) envp);
  uerror("execve", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

