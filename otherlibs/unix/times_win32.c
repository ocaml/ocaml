/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  File contributed by Josh Berdine                      */
/*                                                                        */
/*   Copyright 2011 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"
#include <windows.h>

#define CAML_INTERNALS
#include <caml/winsupport.h>

value caml_unix_times(value unit) {
  value res;
  FILETIME _creation, _exit;
  CAML_ULONGLONG_FILETIME stime, utime;

  if (!(GetProcessTimes(GetCurrentProcess(), &_creation, &_exit,
                        &stime.ft, &utime.ft))) {
    caml_win32_maperr(GetLastError());
    caml_uerror("times", Nothing);
  }

  res = caml_alloc_small(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, (double)(utime.ul / 1e7));
  Store_double_field(res, 1, (double)(stime.ul / 1e7));
  Store_double_field(res, 2, 0);
  Store_double_field(res, 3, 0);
  return res;
}
