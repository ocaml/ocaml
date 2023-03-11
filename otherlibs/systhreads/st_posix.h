/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* POSIX thread implementation of the "st" interface */

#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

Caml_inline void st_msleep(int msec)
{
  struct timeval timeout = {0, msec * 1000};
  select(0, NULL, NULL, NULL, &timeout);
}

#include "st_pthreads.h"

/* Signal handling */

static void st_decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  for (/*nothing*/; vset != Val_emptylist; vset = Field(vset, 1)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
  }
}

#ifndef NSIG
#define NSIG 64
#endif

static value st_encode_sigset(sigset_t * set)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  res = Val_emptylist;

  for (i = 1; i < NSIG; i++)
    if (sigismember(set, i) > 0) {
      res = caml_alloc_2(Tag_cons,
                         Val_int(caml_rev_convert_signal_number(i)), res);
    }
  CAMLreturn(res);
}

static int sigmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

value caml_thread_sigmask(value cmd, value sigs)
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigmask_cmd[Int_val(cmd)];
  st_decode_sigset(sigs, &set);
  caml_enter_blocking_section();
  retcode = pthread_sigmask(how, &set, &oldset);
  caml_leave_blocking_section();
  sync_check_error(retcode, "Thread.sigmask");
  /* Run any handlers for just-unmasked pending signals */
  caml_process_pending_actions();
  return st_encode_sigset(&oldset);
}

value caml_wait_signal(value sigs)
{
#ifdef HAS_SIGWAIT
  sigset_t set;
  int retcode, signo;

  st_decode_sigset(sigs, &set);
  caml_enter_blocking_section();
  retcode = sigwait(&set, &signo);
  caml_leave_blocking_section();
  sync_check_error(retcode, "Thread.wait_signal");
  return Val_int(caml_rev_convert_signal_number(signo));
#else
  caml_invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
#endif
}
