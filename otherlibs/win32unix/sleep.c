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
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_sleep(t) {
  double d = Double_val(t);
  caml_enter_blocking_section();
  Sleep(d * 1e3);
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value unix_sleep(value sec, value usec) {
  DWORD time = Int64_val(sec) * 1e3 + Int64_val(usec) / 1e3; 
  caml_enter_blocking_section();
  Sleep(time);
  caml_leave_blocking_section();
  return Val_unit;
}
