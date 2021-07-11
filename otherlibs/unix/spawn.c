/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy, projet Cambium, Collège de France and INRIA Paris      */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define _GNU_SOURCE  /* helps to find execvpe() */
#include <errno.h>
#include <sys/types.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "unixsupport.h"
#include <spawn.h>

extern char ** environ;

/* Implementation based on posix_spawn() */

CAMLprim value caml_unix_spawn(value executable, /* string */
                          value args,       /* string array */
                          value optenv,     /* string array option */
                          value usepath,    /* bool */
                          value redirect)   /* int array (size 3) */
{
  char ** argv;
  char ** envp;
  const char * path;
  pid_t pid;
  int src, dst, r, i;
  posix_spawn_file_actions_t act;

  caml_unix_check_path(executable, "create_process");
  path = String_val(executable);
  argv = caml_unix_cstringvect(args, "create_process");
  if (Is_some(optenv)) {
    envp = caml_unix_cstringvect(Some_val(optenv), "create_process");
  } else {
    envp = environ;
  }
  /* Prepare the redirections for stdin, stdout, stderr */
  posix_spawn_file_actions_init(&act);
  for (dst = 0; dst <= 2; dst++) {
    /* File descriptor [redirect.(dst)] becomes file descriptor [dst] */
    src = Int_val(Field(redirect, dst));
    if (src != dst) {
      r = posix_spawn_file_actions_adddup2(&act, src, dst);
      if (r != 0) goto error;
      /* Close [src] if this is its last use */
      for (i = dst + 1; i <= 2; i++) {
        if (src == Int_val(Field(redirect, i))) goto dontclose;
      }
      r = posix_spawn_file_actions_addclose(&act, src);
      if (r != 0) goto error;
    dontclose:
      /*skip*/;
    }
  }
  /* Spawn the new process */
  if (Bool_val(usepath)) {
    r = posix_spawnp(&pid, path, &act, NULL, argv, envp);
  } else {
    r = posix_spawn(&pid, path, &act, NULL, argv, envp);
  }
 error:
  posix_spawn_file_actions_destroy(&act);
  caml_unix_cstringvect_free(argv);
  if (Is_some(optenv)) caml_unix_cstringvect_free(envp);
  if (r != 0) caml_unix_error(r, "create_process", executable);
  return Val_long(pid);
}
