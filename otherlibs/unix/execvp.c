/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "unixsupport.h"

#ifndef _WIN32
extern char ** environ;
#endif

CAMLprim value unix_execvp(value path, value args)
{
  char ** argv;
  caml_unix_check_path(path, "execvp");
  argv = cstringvect(args, "execvp");
  (void) execvp(String_val(path), argv);
  caml_stat_free((char *) argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

CAMLprim value unix_execvpe(value path, value args, value env)
{
  char ** argv;
  char ** saved_environ;
  caml_unix_check_path(path, "execvpe");
  argv = cstringvect(args, "execvpe");
  saved_environ = environ;
  environ = cstringvect(env, "execvpe");
  (void) execvp(String_val(path), argv);
  caml_stat_free((char *) argv);
  caml_stat_free((char *) environ);
  environ = saved_environ;
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
