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
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "caml/unixsupport.h"

#ifdef HAS_SETITIMER

#include <math.h>
#include <sys/time.h>

static void caml_unix_set_timeval(struct timeval * tv, double sec)
{
  double int_sec, frac_sec;
  frac_sec = modf(sec, &int_sec);
  /* Round time up so that if [sec] is small but not 0, we end up with
     a non-0 timeval. */
  tv->tv_sec = int_sec;
  tv->tv_usec = ceil(frac_sec * USEC_PER_SEC);
  if (tv->tv_usec >= USEC_PER_SEC) { tv->tv_sec++; tv->tv_usec = 0; }
}

static value caml_unix_convert_itimer(struct itimerval *tp)
{
#define Get_timeval(tv) (double) tv.tv_sec + (double) tv.tv_usec / USEC_PER_SEC
  value res = caml_alloc_small(Double_wosize * 2, Double_array_tag);
  Store_double_flat_field(res, 0, Get_timeval(tp->it_interval));
  Store_double_flat_field(res, 1, Get_timeval(tp->it_value));
  return res;
#undef Get_timeval
}

static const int itimers[3] = { ITIMER_REAL, ITIMER_VIRTUAL, ITIMER_PROF };

CAMLprim value caml_unix_setitimer(value which, value newval)
{
  struct itimerval new, old;
  caml_unix_set_timeval(&new.it_interval, Double_flat_field(newval, 0));
  caml_unix_set_timeval(&new.it_value, Double_flat_field(newval, 1));
  if (setitimer(itimers[Int_val(which)], &new, &old) == -1)
    caml_uerror("setitimer", Nothing);
  return caml_unix_convert_itimer(&old);
}

CAMLprim value caml_unix_getitimer(value which)
{
  struct itimerval val;
  if (getitimer(itimers[Int_val(which)], &val) == -1)
    caml_uerror("getitimer", Nothing);
  return caml_unix_convert_itimer(&val);
}

#else

CAMLprim value caml_unix_setitimer(value which, value newval)
{ caml_invalid_argument("setitimer not implemented"); }
CAMLprim value caml_unix_getitimer(value which)
{ caml_invalid_argument("getitimer not implemented"); }

#endif
