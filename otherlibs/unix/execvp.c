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

#define _GNU_SOURCE  /* helps to find execvpe() */
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#define CAML_INTERNALS
#include <caml/osdeps.h>
#include "unixsupport.h"
#include "errno.h"

CAMLprim value caml_unix_execvp(value path, value args)
{
  char_os ** argv;
  char_os * wpath;
  caml_unix_check_path(path, "execvp");
  argv = caml_unix_cstringvect(args, "execvp");
  wpath = caml_stat_strdup_to_os(String_val(path));
  (void) execvp_os((const char_os *)wpath, EXECV_CAST argv);
  caml_stat_free(wpath);
  caml_unix_cstringvect_free(argv);
  caml_uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}

#ifndef HAS_EXECVPE
int caml_unix_execvpe_emulation(const char * name,
                           char * const argv[],
                           char * const envp[]);
#endif

CAMLprim value caml_unix_execvpe(value path, value args, value env)
{
  char_os ** argv;
  char_os ** envp;
  char_os * wpath;
  int err;
  caml_unix_check_path(path, "execvpe");
  argv = caml_unix_cstringvect(args, "execvpe");
  envp = caml_unix_cstringvect(env, "execvpe");
  wpath = caml_stat_strdup_to_os(String_val(path));
#ifdef HAS_EXECVPE
  (void) execvpe_os((const char_os *)wpath, EXECV_CAST argv, EXECV_CAST envp);
  err = errno;
#else
  err = caml_unix_execvpe_emulation(wpath, argv, envp);
#endif
  caml_stat_free(wpath);
  caml_unix_cstringvect_free(argv);
  caml_unix_cstringvect_free(envp);
  caml_unix_error(err, "execvpe", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                    /* from smart compilers */
}

#ifndef HAS_EXECVPE

static int caml_unix_execve_script(const char * path,
                              char * const argv[],
                              char * const envp[])
{
  size_t argc, i;
  char ** new_argv;

  /* Try executing directly.  Will not return if it succeeds. */
  execve(path, argv, envp);
  if (errno != ENOEXEC) return errno;
  /* Try executing as a shell script. */
  for (argc = 0; argv[argc] != NULL; argc++) /*skip*/;
  /* The new argument vector is
            {"/bin/sh", path, argv[1], ..., argv[argc-1], NULL} */
  new_argv = calloc(argc + 3, sizeof (char *));
  if (new_argv == NULL) return ENOMEM;
  new_argv[0] = "/bin/sh";
  new_argv[1] = (char *) path;
  for (i = 1; i < argc; i++) new_argv[i + 1] = argv[i];
  new_argv[argc + 1] = NULL;
  /* Execute the shell with the new argument vector.
     Will not return if it succeeds. */
  execve(new_argv[0], new_argv, envp);
  /* Shell execution failed. */
  free(new_argv);
  return errno;
}

int caml_unix_execvpe_emulation(const char * name,
                           char * const argv[],
                           char * const envp[])
{
  char * searchpath, * p, * q, * fullname;
  size_t namelen, dirlen;
  int r, got_eacces;

  /* If name contains a '/', do not search in path */
  if (strchr(name, '/') != NULL)
    return caml_unix_execve_script(name, argv, envp);
  /* Determine search path */
  searchpath = getenv("PATH");
  if (searchpath == NULL) searchpath = "/bin:/usr/bin";
  if (searchpath[0] == 0) return ENOENT;
  namelen = strlen(name);
  got_eacces = 0;
  p = searchpath;
  while (1) {
    /* End of path component is next ':' or end of string */
    for (q = p; *q != 0 && *q != ':'; q++) /*skip*/;
    /* Path component is between p (included) and q (excluded) */
    dirlen = q - p;
    if (dirlen == 0) {
      /* An empty path component means "current working directory" */
      r = caml_unix_execve_script(name, argv, envp);
    } else {
      /* Construct the string "directory/name" */
      fullname = malloc(dirlen + 1 + namelen + 1);
      if (fullname == NULL) return ENOMEM;
      memcpy(fullname, p, dirlen);   /* copy directory from path */
      fullname[dirlen] = '/';        /* add separator */
      memcpy(fullname + dirlen + 1, name, namelen + 1);
                                     /* add name, including final 0 */
      r = caml_unix_execve_script(fullname, argv, envp);
      free(fullname);
    }
    switch (r) {
    case EACCES:
      /* Record that we got a "Permission denied" error and continue. */
      got_eacces = 1; break;
    case ENOENT: case ENOTDIR:
      /* The file was not found.  Continue the search. */
      break;
    case EISDIR: case ELOOP:
    case ENODEV: case ETIMEDOUT:
      /* Strange, unexpected error.  Continue the search. */
      break;
    default:
      /* Serious error.  We found an executable file but could not
         execute it.  Stop the search and return the error. */
      return r;
    }
    /* Continue with next path component, if any */
    if (*q == 0) break;
    p = q + 1;                  /* skip ':' */
  }
  /* If we found a file but had insufficient permissions, return
     EACCES to our caller.  Otherwise, say we did not find a file
     (ENOENT). */
  return got_eacces ? EACCES : ENOENT;
}

#endif
