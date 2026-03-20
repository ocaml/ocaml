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
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "caml/unixsupport.h"

CAMLprim value caml_unix_sleep(value sec)
{
  DWORD msec = (DWORD) (Double_val(sec) * MSEC_PER_SEC);
  caml_enter_blocking_section();
  Sleep(msec);
  caml_leave_blocking_section();
  return Val_unit;
}
