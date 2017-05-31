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

CAMLprim value unix_execve(value path, value args, value env)
{
  char ** argv;
  char ** envp;
  caml_unix_check_path(path, "execve");
  argv = cstringvect(args, "execve");
  envp = cstringvect(env, "execve");
  (void) execve(String_val(path), argv, envp);
  caml_stat_free((char *) argv);
  caml_stat_free((char *) envp);
  uerror("execve", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
