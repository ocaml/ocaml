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

#include "caml/config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <sys/types.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"

#ifndef _WIN32
extern char ** environ;
#endif

static char **secure_environ(void)
{
#if defined(HAS_ISSETUGID)
  if (!issetugid ())
    return environ;
  else
    return NULL;
#else
  if (geteuid () == getuid () && getegid () == getgid ())
    return environ;
  else
    return NULL;
#endif
}

CAMLprim value unix_environment(value unit)
{
  char **e = secure_environ();
  if (e != NULL) {
    return caml_copy_string_array((const char**)e);
  } else {
    return Atom(0);
  }
}
