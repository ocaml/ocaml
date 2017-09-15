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
#define CAML_INTERNALS
#include <caml/osdeps.h>
#include "unixsupport.h"

CAMLprim value unix_execvp(value path, value args)
{
  charnat ** argv;
  charnat * wpath;
  caml_unix_check_path(path, "execvp");
  argv = cstringvect(args, "execvp");
  wpath = caml_stat_strdup_to_utf16(String_val(path));
  (void) _texecvp((const charnat *)wpath, EXECV_CAST argv);
  caml_stat_free(wpath);
  cstringvect_free(argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}

CAMLprim value unix_execvpe(value path, value args, value env)
{
  charnat * exefile, * wpath;
  charnat ** argv;
  charnat ** envp;
  caml_unix_check_path(path, "execvpe");
  wpath = caml_stat_strdup_to_utf16(String_val(path));
  exefile = caml_search_exe_in_path(wpath);
  caml_stat_free(wpath);
  argv = cstringvect(args, "execvpe");
  envp = cstringvect(env, "execvpe");
  (void) _texecve((const charnat *)exefile, EXECV_CAST argv, EXECV_CAST envp);
  caml_stat_free(exefile);
  cstringvect_free(argv);
  cstringvect_free(envp);
  uerror("execvpe", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}
