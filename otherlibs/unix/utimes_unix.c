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

#define CAML_INTERNALS

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

#if defined(HAS_UTIMES)

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  struct timeval tv[2], * t;
  char * p;
  int ret;
  double at, mt;
  caml_unix_check_path(path, "utimes");
  at = Double_val(atime);
  mt = Double_val(mtime);
  if (at == 0.0 && mt == 0.0) {
    t = (struct timeval *) NULL;
  } else {
    tv[0].tv_sec = at;
    tv[0].tv_usec = (at - tv[0].tv_sec) * 1000000;
    tv[1].tv_sec = mt;
    tv[1].tv_usec = (mt - tv[1].tv_sec) * 1000000;
    t = tv;
  }
  p = caml_stat_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = utimes(p, t);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) caml_uerror("utimes", path);
  CAMLreturn(Val_unit);
}

#elif defined(HAS_UTIME)

#include <sys/types.h>
#include <utime.h>

CAMLprim value unix_utimes(value path, value atime, value mtime)
{
  CAMLparam3(path, atime, mtime);
  struct utimbuf times, * t;
  char * p;
  int ret;
  double at, mt;
  caml_unix_check_path(path, "utimes");
  at = Double_val(atime);
  mt = Double_val(mtime);
  if (at == 0.0 && mt == 0.0) {
    t = NULL;
  } else {
    times.actime = at;
    times.modtime = mt;
    t = &times;
  }
  p = caml_stat_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = utime(p, t);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) caml_uerror("utimes", path);
  CAMLreturn(Val_unit);
}

#else

CAMLprim value unix_utimes(value path, value atime, value mtime)
{ caml_invalid_argument("utimes not implemented"); }

#endif
