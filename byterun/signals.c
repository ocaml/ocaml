/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <signal.h>
#include "alloc.h"
#include "callback.h"
#include "config.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "sys.h"

#ifdef _WIN32
typedef void (*sighandler)(int sig);
extern sighandler caml_win32_signal(int sig, sighandler action);
#define signal(sig,act) caml_win32_signal(sig,act)
#endif

CAMLexport int volatile caml_async_signal_mode = 0;
CAMLexport int volatile caml_pending_signal = 0;
CAMLexport int volatile caml_something_to_do = 0;
int volatile caml_force_major_slice = 0;
value caml_signal_handlers = 0;
CAMLexport void (*caml_enter_blocking_section_hook)(void) = NULL;
CAMLexport void (*caml_leave_blocking_section_hook)(void) = NULL;
CAMLexport void (* volatile caml_async_action_hook)(void) = NULL;

void caml_process_event(void)
{
  int signal_number;
  void (*async_action)(void);
  if (caml_force_major_slice) caml_minor_collection ();
                             /* FIXME should be [caml_check_urgent_gc] */
  /* If a signal arrives between the following two instructions,
     it will be lost.  To do: use atomic swap or atomic read-and-clear
     for processors that support it? */
  signal_number = caml_pending_signal;
  caml_pending_signal = 0;
  if (signal_number) caml_execute_signal(signal_number, 0);
  /* If an async action is scheduled between the following two instructions,
     it will be lost. */
  async_action = caml_async_action_hook;
  caml_async_action_hook = NULL;
  if (async_action != NULL) (*async_action)();
}

static int rev_convert_signal_number(int signo);

void caml_execute_signal(int signal_number, int in_signal_handler)
{
  value res;
#ifdef POSIX_SIGNALS
  sigset_t sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&sigs);
  sigaddset(&sigs, signal_number);
  sigprocmask(SIG_BLOCK, &sigs, &sigs);
#endif
  res = caml_callback_exn(Field(caml_signal_handlers, signal_number),
                          Val_int(rev_convert_signal_number(signal_number)));
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  }
#endif
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
}

static void handle_signal(int signal_number)
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(signal_number, handle_signal);
#endif
  if (caml_async_signal_mode){
    caml_leave_blocking_section ();
    caml_execute_signal(signal_number, 1);
    caml_enter_blocking_section ();
  }else{
    caml_pending_signal = signal_number;
    caml_something_to_do = 1;
  }
}

void caml_urge_major_slice (void)
{
  caml_force_major_slice = 1;
  caml_something_to_do = 1;
}

CAMLexport void caml_enter_blocking_section(void)
{
  int temp;

  while (1){
    Assert (!caml_async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = caml_pending_signal;   caml_pending_signal = 0;
    if (temp) caml_execute_signal(temp, 0);
    caml_async_signal_mode = 1;
    if (!caml_pending_signal) break;
    caml_async_signal_mode = 0;
  }
  if (caml_enter_blocking_section_hook != NULL){
    caml_enter_blocking_section_hook();
  }
}

CAMLexport void caml_leave_blocking_section(void)
{
#ifdef _WIN32
  int signal_number;
#endif

  if (caml_leave_blocking_section_hook != NULL){
    caml_leave_blocking_section_hook();
  }
#ifdef _WIN32
  /* Under Win32, asynchronous signals such as ctrl-C are not processed
     immediately (see ctrl_handler in win32.c), but simply set
     [caml_pending_signal] and let the system call run to completion.
     Hence, test [caml_pending_signal] here and act upon it, before we get
     a chance to process the result of the system call. */
  signal_number = caml_pending_signal;
  caml_pending_signal = 0;
  if (signal_number) caml_execute_signal(signal_number, 1);
#endif
  Assert(caml_async_signal_mode);
  caml_async_signal_mode = 0;
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
#ifndef SIGPROF
#define SIGPROF -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

CAMLexport int caml_convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

static int rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

#ifndef NSIG
#define NSIG 64
#endif

CAMLprim value caml_install_signal_handler(value signal_number, value action)
{
  CAMLparam2 (signal_number, action);
  int sig;
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif
  CAMLlocal1 (res);

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG) 
    caml_invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = SIG_DFL;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = SIG_IGN;
    break;
  default:                      /* Signal_handle */
    act = handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  if (sigaction(sig, &sigact, &oldsigact) == -1) caml_sys_error(NO_ARG);
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(sig, act);
  if (oldact == SIG_ERR) caml_sys_error(NO_ARG);
#endif
  if (oldact == handle_signal) {
    res = caml_alloc_small (1, 0);          /* Signal_handle */
    Field(res, 0) = Field(caml_signal_handlers, sig);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  if (Is_block(action)) {
    if (caml_signal_handlers == 0) {
      caml_signal_handlers = caml_alloc(NSIG, 0);
      caml_register_global_root(&caml_signal_handlers);
    }
    caml_modify(&Field(caml_signal_handlers, sig), Field(action, 0));
  }
  CAMLreturn (res);
}
