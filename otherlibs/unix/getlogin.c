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

#include <caml/alloc.h>
#include "caml/unixsupport.h"
#include <limits.h>
#include <unistd.h>
#include <errno.h>

#define CAML_LOGIN_NAME_MAX (256 * 4)

CAMLprim value caml_unix_getlogin(value unit)
{
  value res;
  char * name;
#ifdef HAVE_GETLOGIN_R
  size_t bufsize;
#ifdef LOGIN_NAME_MAX
  bufsize = LOGIN_NAME_MAX;
#else
  long initlen = sysconf(_SC_LOGIN_NAME_MAX);
  bufsize = initlen <= 0 ? _POSIX_LOGIN_NAME_MAX : (size_t) initlen;
#endif
  name = caml_stat_alloc_noexc(bufsize);
  if (name == NULL)
    caml_unix_error(ENOMEM, "getlogin", Nothing);
  int e;
  while ((e = getlogin_r(name, bufsize)) == ERANGE) {
    bufsize *= 2;
    char *newname;
    if (bufsize > CAML_LOGIN_NAME_MAX ||
        (newname = caml_stat_realloc(name)) == NULL) {
      caml_stat_free(name);
      caml_unix_error(ENOMEM, "getlogin", Nothing);
    }
    name = newname;
  }
  if (e != 0) {
    caml_stat_free(name);
#else
  name = getlogin();
  if (name == NULL) {
#endif
    caml_unix_error(ENOENT, "getlogin", Nothing);
  }
  res = caml_copy_string(name);
#ifdef HAVE_GETLOGIN_R
  caml_stat_free(name);
#endif
  return res;
}
