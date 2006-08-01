/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#if defined(TARGET_amd64) && defined (SYS_linux)
#define _GNU_SOURCE
#endif
#include <signal.h>
#include <stdio.h>
#include "alloc.h"
#include "callback.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "osdeps.h"
#include "signals.h"
#include "signals_machdep.h"
#include "signals_osdep.h"
#include "stack.h"
#include "sys.h"
#ifdef HAS_STACK_OVERFLOW_DETECTION
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifndef NSIG
#define NSIG 64
#endif

#ifdef _WIN32
typedef void (*sighandler)(int sig);
extern sighandler caml_win32_signal(int sig, sighandler action);
#define signal(sig,act) caml_win32_signal(sig,act)
#endif

extern char * caml_code_area_start, * caml_code_area_end;

#define In_code_area(pc) \
  ((char *)(pc) >= caml_code_area_start && \
   (char *)(pc) <= caml_code_area_end)

intnat volatile caml_signals_are_pending = 0;
volatile intnat caml_pending_signals[NSIG];
volatile int caml_force_major_slice = 0;
value caml_signal_handlers = 0;

static void caml_process_pending_signals(void)
{
  int i;

  if (caml_signals_are_pending) {
    caml_signals_are_pending = 0;
    for (i = 0; i < NSIG; i++) {
      if (caml_pending_signals[i]) {
        caml_pending_signals[i] = 0;
        caml_execute_signal(i, 0);
      }
    }
  }
}

static intnat volatile caml_async_signal_mode = 0;

static void caml_enter_blocking_section_default(void)
{
  Assert (caml_async_signal_mode == 0);
  caml_async_signal_mode = 1;
}

static void caml_leave_blocking_section_default(void)
{
  Assert (caml_async_signal_mode == 1);
  caml_async_signal_mode = 0;
}

static int caml_try_leave_blocking_section_default(void)
{
  intnat res;
  Read_and_clear(res, caml_async_signal_mode);
  return res;
}

CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;
CAMLexport int (*caml_try_leave_blocking_section_hook)(void) =
   caml_try_leave_blocking_section_default;

int caml_rev_convert_signal_number(int signo);

/* Execute a signal handler immediately. */

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
  res = caml_callback_exn(
           Field(caml_signal_handlers, signal_number),
           Val_int(caml_rev_convert_signal_number(signal_number)));
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

/* Record the delivery of a signal and play with the allocation limit
   so that the next allocation will trigger a garbage collection. */

void caml_record_signal(int signal_number)
{
  caml_pending_signals[signal_number] = 1;
  caml_signals_are_pending = 1;
  caml_young_limit = caml_young_end;
}

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to Caml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection(void)
{
  caml_young_limit = caml_young_start;
  if (caml_young_ptr < caml_young_start || caml_force_major_slice) {
    caml_minor_collection();
  }
  caml_process_pending_signals();
}

/* Trigger a garbage collection as soon as possible */

void caml_urge_major_slice (void)
{
  caml_force_major_slice = 1;
  caml_young_limit = caml_young_end;
  /* This is only moderately effective on ports that cache [caml_young_limit]
     in a register, since [caml_modify] is called directly, not through
     [caml_c_call], so it may take a while before the register is reloaded
     from [caml_young_limit]. */
}

void caml_enter_blocking_section(void)
{
  while (1){
    /* Process all pending signals now */
    caml_process_pending_signals();
    caml_enter_blocking_section_hook ();
    /* Check again for pending signals.
       If none, done; otherwise, try again */
    if (! caml_signals_are_pending) break;
    caml_leave_blocking_section_hook ();
  }
}

CAMLexport void caml_leave_blocking_section(void)
{
  caml_leave_blocking_section_hook ();
  caml_process_pending_signals();
}

DECLARE_SIGNAL_HANDLER(handle_signal)
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(sig, handle_signal);
#endif
  if (sig < 0 || sig >= NSIG) return;
  if (caml_try_leave_blocking_section_hook ()) {
    caml_execute_signal(sig, 1);
    caml_enter_blocking_section_hook();
  } else {
    caml_record_signal(sig);
  /* Some ports cache [caml_young_limit] in a register.
     Use the signal context to modify that register too, but only if
     we are inside Caml code (not inside C code). */
#if defined(CONTEXT_PC) && defined(CONTEXT_YOUNG_LIMIT)
    if (In_code_area(CONTEXT_PC))
      CONTEXT_YOUNG_LIMIT = (context_reg) caml_young_limit;
#endif
  }
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

int caml_convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

int caml_rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

typedef void (*signal_handler)(int signo);

value caml_install_signal_handler(value signal_number, value action) /* ML */
{
  CAMLparam2 (signal_number, action);
  int sig;
  signal_handler oldact;
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#else
  signal_handler act;
#endif
  CAMLlocal1 (res);

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG) 
    caml_invalid_argument("Sys.signal: unavailable signal");
#ifdef POSIX_SIGNALS
  switch(action) {
  case Val_int(0):              /* Signal_default */
    sigact.sa_handler = SIG_DFL;
    sigact.sa_flags = 0;
    break;
  case Val_int(1):              /* Signal_ignore */
    sigact.sa_handler = SIG_IGN;
    sigact.sa_flags = 0;
    break;
  default:                      /* Signal_handle */
    SET_SIGACT(sigact, handle_signal);
    break;
  }
  sigemptyset(&sigact.sa_mask);
  if (sigaction(sig, &sigact, &oldsigact) == -1) caml_sys_error(NO_ARG);
  oldact = oldsigact.sa_handler;
#else
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
  oldact = signal(sig, act);
  if (oldact == SIG_ERR) caml_sys_error(NO_ARG);
#endif
  if (oldact == (signal_handler) handle_signal) {
    res = caml_alloc_small(1, 0);          /* Signal_handle */
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
  caml_process_pending_signals();
  CAMLreturn (res);
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_power) || (defined(TARGET_sparc) && defined(SYS_solaris))
DECLARE_SIGNAL_HANDLER(trap_handler)
{
#if defined(SYS_solaris)
  if (info->si_code != ILL_ILLTRP) {
    /* Deactivate our exception handler and return. */
    struct sigaction act;
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(sig, &act, NULL);
    return;
  }
#endif
#if defined(SYS_rhapsody)
  /* Unblock SIGTRAP */
  { sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGTRAP);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
  }
#endif
  caml_exception_pointer = (char *) CONTEXT_EXCEPTION_POINTER;
  caml_young_ptr = (char *) CONTEXT_YOUNG_PTR;
  caml_array_bound_error();
}
#endif

/* Machine- and OS-dependent handling of stack overflow */

#ifdef HAS_STACK_OVERFLOW_DETECTION

static char * system_stack_top;
static char sig_alt_stack[SIGSTKSZ];

DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct rlimit limit;
  struct sigaction act;
  char * fault_addr;

  /* Sanity checks:
     - faulting address is word-aligned
     - faulting address is within the stack
     - we are in Caml code */
  fault_addr = CONTEXT_FAULTING_ADDRESS;
  if (((uintnat) fault_addr & (sizeof(intnat) - 1)) == 0
      && getrlimit(RLIMIT_STACK, &limit) == 0
      && fault_addr < system_stack_top
      && fault_addr >= system_stack_top - limit.rlim_cur - 0x2000
#ifdef CONTEXT_PC
      && In_code_area(CONTEXT_PC)
#endif
      ) {
    /* Turn this into a Stack_overflow exception */
#if defined(CONTEXT_YOUNG_PTR) && defined(CONTEXT_EXCEPTION_POINTER)
    caml_exception_pointer = (char *) CONTEXT_EXCEPTION_POINTER;
    caml_young_ptr = (char *) CONTEXT_YOUNG_PTR;
#endif
    caml_raise_stack_overflow();
  }
  /* Otherwise, deactivate our exception handler and return,
     causing fatal signal to be generated at point of error. */
  act.sa_handler = SIG_DFL;
  act.sa_flags = 0;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, NULL);
}

#endif

/* Initialization of signal stuff */

void caml_init_signals(void)
{
  /* Bound-check trap handling */
#if defined(TARGET_sparc) && defined(SYS_solaris)
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    SET_SIGACT(act, trap_handler);
    act.sa_flags |= SA_NODEFER;
    sigaction(SIGILL, &act, NULL);
  }
#endif

#if defined(TARGET_power)
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    SET_SIGACT(act, trap_handler);
#if !defined(SYS_rhapsody)
    act.sa_flags |= SA_NODEFER;
#endif
    sigaction(SIGTRAP, &act, NULL);
  }
#endif

  /* Stack overflow handling */
#ifdef HAS_STACK_OVERFLOW_DETECTION
  {
    struct sigaltstack stk;
    struct sigaction act;
    stk.ss_sp = sig_alt_stack;
    stk.ss_size = SIGSTKSZ;
    stk.ss_flags = 0;
    SET_SIGACT(act, segv_handler);
    act.sa_flags |= SA_ONSTACK | SA_NODEFER;
    sigemptyset(&act.sa_mask);
    system_stack_top = (char *) &act;
    if (sigaltstack(&stk, NULL) == 0) { sigaction(SIGSEGV, &act, NULL); }
  }
#endif
}
