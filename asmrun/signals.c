/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <signal.h>
#include <stdio.h>
#if defined(TARGET_power) && defined(__linux)
#include <asm/sigcontext.h>
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
#include <ucontext.h>
#endif
#include "alloc.h"
#include "callback.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "signals.h"
#include "stack.h"
#include "sys.h"

volatile int async_signal_mode = 0;
volatile int pending_signal = 0;
volatile int force_major_slice = 0;
value signal_handlers = 0;
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
    young_limit = young_start;
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

#if defined(TARGET_alpha) || defined(TARGET_mips)
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_power) && defined(_AIX)
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_power) && defined(__linux)
void handle_signal(int sig, struct pt_regs * context)
#else
void handle_signal(int sig)
#endif
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(sig, handle_signal);
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
    if (caml_last_return_address == 0) {
#if defined(TARGET_alpha)
      /* Cached in register $14 */
      context->sc_regs[14] = (long) young_limit;
#endif
#if defined(TARGET_mips)
      /* Cached in register $23 */
      context->sc_regs[23] = (int) young_limit;
#endif
#if defined(TARGET_power) && defined(_AIX)
      /* Cached in register 30 */
      context->sc_jmpbuf.jmp_context.gpr[30] = (ulong_t) young_limit;
#endif
#if defined(TARGET_power) && defined(__linux)
      /* Cached in register 30 */
      context->gpr[30] = (unsigned long) young_limit;
#endif
    }
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
  int sig;
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif
  value res;

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
      Begin_root (action);
        signal_handlers = alloc_tuple(NSIG);
      End_roots();
      for (i = 0; i < NSIG; i++) Field(signal_handlers, i) = Val_int(0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
    act = (void (*)(int)) handle_signal;
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
  if (oldact == (void (*)(int)) handle_signal) {
    res = alloc_small(1, 0);          /* Signal_handle */
    Field(res, 0) = Field(signal_handlers, sig);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  return res;
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_sparc) && defined(SYS_sunos)
static void trap_handler(int sig, int code, 
                         struct sigcontext * context, char * address)
{
  int * sp;

  if (code != ILL_TRAP_FAULT(5)) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n", code);
    exit(100);
  }
  /* Recover young_ptr and caml_exception_pointer from the %l5 and %l6 regs */
  Assert(context->sc_wbcnt == 0);
  sp = (int *) scp->sc_sp;
  caml_exception_pointer = (char *) sp[5];
  young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && defined(SYS_solaris)
static void trap_handler(int sig, siginfo_t * info, void * arg)
{
  ucontext_t * context;
  int * sp;

  if (info->si_code != ILL_ILLTRP) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n",
            info->si_code);
    exit(100);
  }
  /* Recover young_ptr and caml_exception_pointer from the %l5 and %l6 regs */
  context = (ucontext_t *) arg;
  Assert(context->uc_mcontext.gwins == NULL);
  sp = (int *) context->uc_mcontext.gregs[REG_SP];
  caml_exception_pointer = (char *) sp[5];
  young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && (defined(SYS_bsd) || defined(SYS_linux))
static void trap_handler(int sig)
{
  /* TODO: recover registers from context and call array_bound_error */
  fatal_error("Fatal error: out-of-bound access in array or string\n");
}
#endif

#if defined(TARGET_power) && defined(_AIX)
static void trap_handler(int sig, int code, struct sigcontext * context)
{
  /* Recover young_ptr and caml_exception_pointer from registers 31 and 29 */
  caml_exception_pointer = (char *) context->sc_jmpbuf.jmp_context.gpr[29];
  young_ptr = (char *) context->sc_jmpbuf.jmp_context.gpr[31];
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(__linux)
static void trap_handler(int sig, struct pt_regs * context)
{
  /* Recover young_ptr and caml_exception_pointer from registers 31 and 29 */
  caml_exception_pointer = (char *) context->gpr[29];
  young_ptr = (char *) context->gpr[31];
  array_bound_error();
}
#endif

/* Initialization of signal stuff */

void init_signals(void)
{
#if defined(TARGET_sparc) && \
      (defined(SYS_sunos) || defined(SYS_bsd) || defined(SYS_linux))
  struct sigaction act;
  act.sa_handler = (void (*)(int)) trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  sigaction(SIGILL, &act, NULL);
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
  struct sigaction act;
  act.sa_sigaction = trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGILL, &act, NULL);
#endif
#if defined(TARGET_power)
  struct sigaction act;
  act.sa_handler = (void (*)(int)) trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  sigaction(SIGTRAP, &act, NULL);
#endif
}

