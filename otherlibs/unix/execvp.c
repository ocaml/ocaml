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
extern char ** environ;

value unix_execvp(path, args)     /* ML */
     value path, args;
{
  char ** argv;
  argv = cstringvect(args);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

value unix_execvpe(path, args, env)     /* ML */
     value path, args, env;
{
  char ** argv;
  char ** saved_environ;
  argv = cstringvect(args);
  saved_environ = environ;
  environ = cstringvect(env);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  stat_free((char *) environ);
  environ = saved_environ;
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

