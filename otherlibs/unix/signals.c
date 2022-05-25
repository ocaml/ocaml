/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <errno.h>
#include <signal.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef POSIX_SIGNALS

static void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  for (/*nothing*/; vset != Val_emptylist; vset = Field(vset, 1)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
  }
}

static value encode_sigset(sigset_t * set)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  for (i = 1; i < NSIG; i++)
    if (sigismember(set, i) > 0) {
      value newcons = caml_alloc_2(Tag_cons,
        Val_int(caml_rev_convert_signal_number(i)),
        res);
      res = newcons;
    }
  CAMLreturn(res);
}

static int sigprocmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

CAMLprim value unix_sigprocmask(value vaction, value vset)
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigprocmask_cmd[Int_val(vaction)];
  decode_sigset(vset, &set);
  caml_enter_blocking_section();
  retcode = sigprocmask(how, &set, &oldset);
  caml_leave_blocking_section();
  /* Run any handlers for just-unmasked pending signals */
  caml_process_pending_actions();
  if (retcode != 0) unix_error(retcode, "sigprocmask", Nothing);
  return encode_sigset(&oldset);
}

CAMLprim value unix_sigpending(value unit)
{
  sigset_t pending;
  int i, j;
  uintnat curr;
  if (sigpending(&pending) == -1) caml_uerror("sigpending", Nothing);
  for (i = 0; i < NSIG_WORDS; i++) {
    curr = atomic_load(&caml_pending_signals[i]);
    if (curr == 0) continue;
    for (j = 0; j < BITS_PER_WORD; j++) {
      if (curr & ((uintnat)1 << j))
      sigaddset(&pending, i * BITS_PER_WORD + j + 1);
    }
  }
  return encode_sigset(&pending);
}

CAMLprim value unix_sigsuspend(value vset)
{
  sigset_t set;
  int retcode;
  decode_sigset(vset, &set);
  caml_enter_blocking_section();
  retcode = sigsuspend(&set);
  caml_leave_blocking_section();
  if (retcode == -1 && errno != EINTR) caml_uerror("sigsuspend", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_sigprocmask(value vaction, value vset)
{ caml_invalid_argument("Unix.sigprocmask not available"); }

CAMLprim value unix_sigpending(value unit)
{ caml_invalid_argument("Unix.sigpending not available"); }

CAMLprim value unix_sigsuspend(value vset)
{ caml_invalid_argument("Unix.sigsuspend not available"); }

#endif
