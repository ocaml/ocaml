/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
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

#if macintosh
#include "rotatecursor.h"
#endif /* macintosh */

int volatile async_signal_mode = 0;
int volatile pending_signal = 0;
int volatile something_to_do = 0;
int volatile force_major_slice = 0;
value signal_handlers = 0;
void (*enter_blocking_section_hook)(void) = NULL;
void (*leave_blocking_section_hook)(void) = NULL;
void (* volatile async_action_hook)(void) = NULL;

void process_event(void)
{
  int signal_number;
  void (*async_action)(void);
  if (force_major_slice) minor_collection ();
  /* If a signal arrives between the following two instructions,
     it will be lost.  To do: use atomic swap or atomic read-and-clear
     for processors that support it? */
  signal_number = pending_signal;
  pending_signal = 0;
  if (signal_number) execute_signal(signal_number, 0);
  /* If an async action is scheduled between the following two instructions,
     it will be lost. */
  async_action = async_action_hook;
  async_action_hook = NULL;
  if (async_action != NULL) (*async_action)();
#if macintosh
  ROTATECURSOR_MAGIC ();
#endif
}

void execute_signal(int signal_number, int in_signal_handler)
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
  res = callback_exn(Field(signal_handlers, signal_number),
                     Val_int(signal_number));
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
  if (Is_exception_result(res)) mlraise(Extract_exception(res));
}

void handle_signal(int signal_number)
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(signal_number, handle_signal);
#endif
  if (async_signal_mode){
    leave_blocking_section ();
    execute_signal(signal_number, 1);
    enter_blocking_section ();
  }else{
    pending_signal = signal_number;
    something_to_do = 1;
  }
}

#ifdef _WIN32
#include <windows.h>

#define hexa_digit(ch) (ch >= 97 ? ch - 87 : \
                        (ch >= 65 ? ch - 55 : \
                         (ch >= 48 ? ch - 48 : 0)))
  
DWORD WINAPI caml_signal_thread(LPVOID lpParam)
{
  char *data;
  int i;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  data = getenv("CAMLSIGPIPE");
  for(i = 0; i < sizeof(HANDLE); i++)
    ((char*)&h)[i] = (hexa_digit(data[2*i]) << 4) + hexa_digit(data[2*i+1]);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) sys_exit(Val_int(0));
    switch (iobuf[0]) {
    case 'C':
      pending_signal = SIGINT;
      something_to_do = 1;
      break;
    case 'T':
      exit(0);
      break;
    }
  }
}
#endif   

void urge_major_slice (void)
{
  force_major_slice = 1;
  something_to_do = 1;
}

void enter_blocking_section(void)
{
  int temp;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = pending_signal;   pending_signal = 0;
    if (temp) execute_signal(temp, 0);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
  if (enter_blocking_section_hook != NULL) enter_blocking_section_hook();
}

void leave_blocking_section(void)
{
  if (leave_blocking_section_hook != NULL) leave_blocking_section_hook();
  Assert(async_signal_mode);
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
#ifndef SIGPROF
#define SIGPROF -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

int convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

#ifndef NSIG
#define NSIG 32
#endif

value install_signal_handler(value signal_number, value action) /* ML */
{
  CAMLparam2 (signal_number, action);
  int sig;
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif
  CAMLlocal1 (res);

  sig = convert_signal_number(Int_val(signal_number));
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
      signal_handlers = alloc_tuple(NSIG);
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
  if (sigaction(sig, &sigact, &oldsigact) == -1) sys_error(NO_ARG);
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(sig, act);
  if (oldact == SIG_ERR) sys_error(NO_ARG);
#endif
  if (oldact == handle_signal) {
    res = alloc_small (1, 0);          /* Signal_handle */
    Field(res, 0) = Field(signal_handlers, sig);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  CAMLreturn (res);
}
