/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <signal.h>
#include <stdio.h>
#if defined(__linux) && defined(TARGET_power)
#include <asm/sigcontext.h>
#endif
#include "alloc.h"
#include "callback.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "signals.h"

volatile int async_signal_mode = 0;
volatile int pending_signal = 0;
volatile int force_major_slice = 0;
value signal_handlers = 0;
extern unsigned long caml_last_return_address;
void (*enter_blocking_section_hook)() = NULL;
void (*leave_blocking_section_hook)() = NULL;

/* Execute a signal handler immediately. */

static void execute_signal(int signal_number)
{
  Assert (!async_signal_mode);
  callback(Field(signal_handlers, signal_number), Val_int(signal_number));
}

/* This routine is the common entry point for garbage collection
   and signal handling */

void garbage_collection(void)
{
  int sig;

  if (young_ptr < young_start || force_major_slice) minor_collection();
  /* If a signal arrives between the following two instructions,
     it will be lost. */
  sig = pending_signal;
  pending_signal = 0;
  young_limit = young_start;
  if (sig) execute_signal(sig);
}

/* Trigger a garbage collection as soon as possible */

void urge_major_slice (void)
{
  force_major_slice = 1;
  young_limit = young_end;
  /* This is only moderately effective on ports that cache young_limit
     in a register, since modify() is called directly, not through
     caml_c_call, so it may take a while before the register is reloaded
     from young_limit. */
}

void enter_blocking_section(void)
{
  int sig;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    sig = pending_signal;
    pending_signal = 0;
    if (sig) execute_signal(sig);
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

#if defined(TARGET_alpha) || defined(TARGET_mips) || \
    (defined(TARGET_power) && defined(_AIX))
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_power) && defined(__linux)
void handle_signal(int sig, sutrct pt_regs * context)
#else
void handle_signal(int sig)
#endif
{
#ifndef POSIX_SIGNALS
#ifndef BSD_SIGNALS
  signal(sig, handle_signal);
#endif
#endif
  if (async_signal_mode) {
    /* We are interrupting a C function blocked on I/O.
       Callback the Caml code immediately. */
    leave_blocking_section();
    callback(Field(signal_handlers, sig), Val_int(sig));
    enter_blocking_section();
  } else {
    /* We can't execute the signal code immediately.
       Instead, we remember the signal and play with the allocation limit
       so that the next allocation will trigger a garbage collection. */
    pending_signal = sig;
    young_limit = young_end;
    /* Some ports cache young_limit in a register.
       Use the signal context to modify that register too, but not if
       we are inside C code (i.e. caml_last_return_address != 0). */
#ifdef TARGET_alpha
    /* Cached in register $14 */
    if (caml_last_return_address == 0)
      context->sc_regs[14] = (long) young_limit;
#endif
#ifdef TARGET_mips
      /* Cached in register $23 */
      if (caml_last_return_address == 0)
        context->sc_regs[23] = (int) young_limit;
#endif
#ifdef TARGET_power
      /* Cached in register 31 */
#ifdef _AIX
      if (caml_last_return_address == 0)
        context->sc_jmpbuf.jmp_context.gpr[31] = (ulong_t) young_limit;
#endif
#ifdef __linux
      if (caml_last_return_address == 0)
        context->gpr[31] = (unsigned long) young_limit;
#endif
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

int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

#ifndef NSIG
#define NSIG 32
#endif

value install_signal_handler(value signal_number, value action) /* ML */
                                 
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
      Begin_root (action);
        signal_handlers = alloc_tuple(NSIG);
      End_roots();
      for (i = 0; i < NSIG; i++) Field(signal_handlers, i) = Val_int(0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
    act = handle_signal;
    break;
  }
#ifndef POSIX_SIGNALS
  signal(sig, act);
#else
  sigact.sa_handler = act;
  sigact.sa_flags = 0;
  sigemptyset(&sigact.sa_mask);
  sigaction(sig, &sigact, NULL);
#endif
  return Val_unit;
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_sparc) && defined(SYS_sunos)
static void trap_handler(int sig, int code, 
                         struct sigcontext * context, char * address)
{
  if (code == ILL_TRAP_FAULT(5)) {
    array_bound_error();
  } else {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n", code);
    exit(100);
  }
}
#endif

#if defined(TARGET_sparc) && defined(SYS_solaris)
static void trap_handler(int sig, siginfo_t * info, void * context)
{
  if (info->si_code == ILL_ILLTRP) {
    array_bound_error();
  } else {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n",
            info->si_code);
    exit(100);
  }
}
#endif

#if defined(TARGET_sparc) && defined(SYS_bsd)
static void trap_handler(int sig)
{
  array_bound_error();
}
#endif

#if defined(TARGET_power)
static void trap_handler(int sig)
{
  array_bound_error();
}
#endif

/* Initialization of signal stuff */

void init_signals(void)
{
#if defined(TARGET_sparc) && (defined(SYS_sunos) || defined(SYS_bsd))
  signal(SIGILL, trap_handler);
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
  struct sigaction act;
  act.sa_sigaction = trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGILL, &act, NULL);
#endif
#if defined(TARGET_power)
  signal(SIGTRAP, trap_handler);
#endif
}

