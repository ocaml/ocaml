/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <errno.h>
#include <signal.h>

#include <alloc.h>
#include <memory.h>
#include <mlvalues.h>
#include <signals.h>
#include "unixsupport.h"

static void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

static value encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i)) {
        value newcons = alloc_small(2, 0);
        Field(newcons, 0) = Val_int(i);
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigprocmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

value unix_sigprocmask(value vaction, value vset) /* ML */
{
  int how;
  sigset_t set, oldset;

  how = sigprocmask_cmd[Int_val(vaction)];
  decode_sigset(vset, &set);
  if (sigprocmask(how, &set, &oldset) == -1) uerror("sigprocmask", Nothing);
  return encode_sigset(&oldset);
}

value unix_sigpending(value unit) /* ML */
{
  sigset_t pending;
  if (sigpending(&pending) == -1) uerror("sigpending", Nothing);
  return encode_sigset(&pending);
}

value unix_sigsuspend(value vset) /* ML */
{
  sigset_t set;
  decode_sigset(vset, &set);
  if (sigsuspend(&set) == -1 && errno != EINTR) uerror("sigsuspend", Nothing);
  return Val_unit;
}
