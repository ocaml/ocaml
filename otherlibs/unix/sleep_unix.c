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

#include <errno.h>
#include <time.h>

CAMLprim value caml_unix_sleep(value duration)
{
  double d = Double_val(duration);
  if (d < 0.0) return Val_unit;
  {
    struct timespec t;
    int ret;
    t.tv_sec = (time_t) d;
    t.tv_nsec = (d - t.tv_sec) * 1e9;
    do {
      caml_enter_blocking_section();
      ret = nanosleep(&t, &t);
      /* MPR#7903: if we were interrupted by a signal, and this signal
         is handled in OCaml, we should run its handler now,
         not at the end of the full sleep duration.  Leaving the blocking
         section and re-entering it does the job. */
      caml_leave_blocking_section();
    } while (ret == -1 && errno == EINTR);
    if (ret == -1) caml_uerror("sleep", Nothing);
  }
  return Val_unit;
}
