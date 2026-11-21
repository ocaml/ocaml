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
#include <caml/fail.h>
#include "caml/unixsupport.h"
#include <signal.h>
#include <caml/signals.h>

CAMLprim value caml_unix_kill(value pid, value signal)
{
  int sig;
  pid_t pid_c =
    (pid_t) caml_checked_c_int(pid, "Unix.kill: pid out of range");
  int signal_c = caml_checked_c_int(signal, "Unix.kill: signal out of range");
  sig = caml_convert_signal_number(signal_c);
  if (kill(pid_c, sig) == -1)
    caml_uerror("kill", Nothing);
  caml_process_pending_actions();
  return Val_unit;
}
