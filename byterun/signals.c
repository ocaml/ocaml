/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <signal.h>
#include "alloc.h"
#include "config.h"
#include "fail.h"
#include "interp.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

static Volatile int async_signal_mode = 0;
Volatile int pending_signal = 0;
value signal_handlers = 0;

static void execute_signal(signal_number)
     int signal_number;
{
  Assert (!async_signal_mode);
  callback(Field(signal_handlers, signal_number), Val_int(signal_number));
}

void handle_signal(signal_number)
     int signal_number;
{
#ifndef POSIX_SIGNALS
#ifndef BSD_SIGNALS
  signal(signal_number, handle_signal);
#endif
#endif
  if (async_signal_mode){
    leave_blocking_section ();
    execute_signal(signal_number);
    enter_blocking_section ();
  }else{
    pending_signal = signal_number;
    something_to_do = 1;
  }
}

void enter_blocking_section()
{
  int temp;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = pending_signal;   pending_signal = 0;
    if (temp) execute_signal(temp);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
}

/* This function may be called from outside a blocking section. */
void leave_blocking_section()
{
  async_signal_mode = 0;
}

#ifndef SIGABRT
#define SIGABRT -1
#endif
#ifndef SIGALRM
#define SIGALRM -1
#endif
#ifndef SIGFPE
#define SIGFPE -1
#endif
#ifndef SIGHUP
#define SIGHUP -1
#endif
#ifndef SIGILL
#define SIGILL -1
#endif
#ifndef SIGINT
#define SIGINT -1
#endif
#ifndef SIGKILL
#define SIGKILL -1
#endif
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
#ifndef SIGTERM
#define SIGTERM -1
#endif
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
#ifndef SIGCONT
#define SIGCONT -1
#endif
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif

int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM
};

#ifndef NSIG
#define NSIG 32
#endif

value install_signal_handler(signal_number, action) /* ML */
     value signal_number, action;
{
  int sig;
  void (*act)();
#ifdef POSIX_SIGNALS
  struct sigaction sigact;
#endif

  sig = Int_val(signal_number);
  if (sig < 0) sig = posix_signals[-sig-1];
  if (sig < 0 || sig >= NSIG) 
    invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = SIG_DFL;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = SIG_IGN;
    break;
  default:                      /* Signal_handle */
    if (signal_handlers == 0) {
      int i;
      Push_roots(r, 1);
      r[0] = action;
      signal_handlers = alloc_tuple(NSIG);
      action = r[0];
      Pop_roots();
      for (i = 0; i < NSIG; i++) Field(signal_handlers, i) = Val_int(0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
    act = handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(sig, &sigact, NULL);
#else
  signal(sig, act);
#endif
  return Val_unit;
}
