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

#include <signal.h>
#include <stdio.h>
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
#ifdef HAS_STACK_OVERFLOW_DETECTION
#include <sys/time.h>
#include <sys/resource.h>
#endif

extern char * code_area_start, * code_area_end;

#define In_code_area(pc) \
  ((char *)(pc) >= code_area_start && (char *)(pc) <= code_area_end)

#ifdef _WIN32
typedef void (*sighandler)(int sig);
extern sighandler win32_signal(int sig, sighandler action);
#define signal(sig,act) win32_signal(sig,act)
#endif

#if defined(TARGET_power) && defined(SYS_rhapsody)

  #include <sys/utsname.h>

  #define STRUCT_SIGCONTEXT void
  #define CONTEXT_GPR(ctx, regno) (*context_gpr_p ((ctx), (regno)))
  #define CONTEXT_PC(ctx) CONTEXT_GPR ((ctx), -2)
  static int ctx_version = 0;
  static void init_ctx (void)
  {
    struct utsname name;
    if (uname (&name) == 0){
      if (name.release[1] == '.' && name.release[0] <= '5'){
        ctx_version = 1;
      }else{
        ctx_version = 2;
      }
    }else{
      caml_fatal_error ("cannot determine SIGCONTEXT format");
    }
  }

  #ifdef DARWIN_VERSION_6
    #include <sys/ucontext.h>
    static unsigned long *context_gpr_p (void *ctx, int regno)
    {
      unsigned long *regs;
      if (ctx_version == 0) init_ctx ();
      if (ctx_version == 1){
        /* old-style context (10.0 and 10.1) */
        regs = (unsigned long *)(((struct sigcontext *)ctx)->sc_regs);
      }else{
        Assert (ctx_version == 2);
        /* new-style context (10.2) */
        regs = (unsigned long *)&(((struct ucontext *)ctx)->uc_mcontext->ss);
      }
      return &(regs[2 + regno]);
    }
  #else
    #define SA_SIGINFO 0x0040
    struct ucontext {
      int       uc_onstack;
      sigset_t  uc_sigmask;
      struct sigaltstack uc_stack;
      struct ucontext   *uc_link;
      size_t    uc_mcsize;
      unsigned long     *uc_mcontext;
    };
    static unsigned long *context_gpr_p (void *ctx, int regno)
    {
      unsigned long *regs;
      if (ctx_version == 0) init_ctx ();
      if (ctx_version == 1){
        /* old-style context (10.0 and 10.1) */
        regs = (unsigned long *)(((struct sigcontext *)ctx)->sc_regs);
      }else{
        Assert (ctx_version == 2);
        /* new-style context (10.2) */
        regs = (unsigned long *)((struct ucontext *)ctx)->uc_mcontext + 8;
      }
      return &(regs[2 + regno]);
    }
  #endif
#endif

#if defined(TARGET_power) && defined(SYS_aix)
#ifdef _AIXVERSION_430
#define STRUCT_SIGCONTEXT struct __sigcontext
#define CONTEXT_GPR(ctx, regno) \
  ((ctx)->__sc_jmpbuf.__jmp_context.__gpr[(regno)])
#else
#define STRUCT_SIGCONTEXT struct sigcontext
#define CONTEXT_GPR(ctx, regno) \
  ((ctx)->sc_jmpbuf.jmp_context.gpr[(regno)])
#endif
#endif

volatile int async_signal_mode = 0;
volatile int pending_signal = 0;
volatile int force_major_slice = 0;
value signal_handlers = 0;
void (*enter_blocking_section_hook)() = NULL;
void (*leave_blocking_section_hook)() = NULL;

static int rev_convert_signal_number(int signo);

/* Execute a signal handler immediately. */

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
  res = caml_callback_exn(Field(signal_handlers, signal_number),
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
  if (Is_exception_result(res)) mlraise(Extract_exception(res));
}

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to Caml code.
   With system threads, this callback can cause a context switch.
   Hence [garbage_collection] must not be called from regular C code
   (e.g. the [alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [garbage_collection],
   via the caml_call_gc assembly stubs.  */

void garbage_collection(void)
{
  int sig;

  if (caml_young_ptr < caml_young_start || force_major_slice){
    caml_minor_collection();
  }
  /* If a signal arrives between the following two instructions,
     it will be lost. */
  sig = pending_signal;
  pending_signal = 0;
  caml_young_limit = caml_young_start;
  if (sig) execute_signal(sig, 0);
}

/* Trigger a garbage collection as soon as possible */

void urge_major_slice (void)
{
  force_major_slice = 1;
  caml_young_limit = caml_young_end;
  /* This is only moderately effective on ports that cache [caml_young_limit]
     in a register, since [caml_modify] is called directly, not through
     [caml_c_call], so it may take a while before the register is reloaded
     from [caml_young_limit]. */
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
    caml_young_limit = caml_young_start;
    if (sig) execute_signal(sig, 0);
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

#ifdef POSIX_SIGNALS
static void reraise(int sig, int now)
{
  struct sigaction sa;
  sa.sa_handler = 0;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(sig, &sa, 0);
  /* If the signal was sent using kill() (si_code == 0) or will
     not recur then raise it here.  Otherwise return.  The
     offending instruction will be reexecuted and the signal
     will recur.  */
  if (now == 1)
    raise(sig);
  return;
}
#endif

#if defined(TARGET_alpha) || defined(TARGET_mips)
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_power) && defined(SYS_aix)
void handle_signal(int sig, int code, STRUCT_SIGCONTEXT * context)
#elif defined(TARGET_power) && defined(SYS_elf)
void handle_signal(int sig, struct sigcontext * context)
#elif defined(TARGET_power) && defined(SYS_rhapsody)
void handle_signal(int sig, int code, STRUCT_SIGCONTEXT * context)
#elif defined(TARGET_power) && defined(SYS_bsd)
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_sparc) && defined(SYS_solaris)
void handle_signal(int sig, int code, void * context)
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
    execute_signal(sig, 1);
    enter_blocking_section();
  } else {
    /* We can't execute the signal code immediately.
       Instead, we remember the signal and play with the allocation limit
       so that the next allocation will trigger a garbage collection. */
    pending_signal = sig;
    caml_young_limit = caml_young_end;
    /* Some ports cache [caml_young_limit] in a register.
       Use the signal context to modify that register too, but only if
       we are inside Caml code (not inside C code). */
#if defined(TARGET_alpha)
    if (In_code_area(context->sc_pc)) {
      /* Cached in register $14 */
      context->sc_regs[14] = (long) caml_young_limit;
    }
#endif
#if defined(TARGET_mips)
    if (In_code_area(context->sc_pc)) {
      /* Cached in register $23 */
      context->sc_regs[23] = (int) caml_young_limit;
    }
#endif
#if defined(TARGET_power) && defined(SYS_aix)
    if (caml_last_return_address == 0) {
      /* Cached in register 30 */
      CONTEXT_GPR(context, 30) = (ulong_t) caml_young_limit;
    }
#endif
#if defined(TARGET_power) && defined(SYS_elf)
    if (caml_last_return_address == 0) {
      /* Cached in register 30 */
      context->regs->gpr[30] = (unsigned long) caml_young_limit;
    }
#endif
#if defined(TARGET_power) && defined(SYS_rhapsody)
    if (In_code_area(CONTEXT_PC(context))) {
      /* Cached in register 30 */
      CONTEXT_GPR(context, 30) = (unsigned long) caml_young_limit;
    }
#endif
#if defined(TARGET_power) && defined(SYS_bsd)
    if (caml_last_return_address == 0) {
      /* Cached in register 30 */
      context->sc_frame.fixreg[30] = (unsigned long) caml_young_limit;
    }
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
    { greg_t * gregs = ((ucontext_t *)context)->uc_mcontext.gregs;
      if (In_code_area(gregs[REG_PC])) {
      /* Cached in register l7, which is saved on the stack 7 words
	 after the stack pointer.  */
        ((long *)(gregs[REG_SP]))[7] = (long) caml_young_limit;
      }
    }
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

int convert_signal_number(int signo)
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
    act = (void (*)(int)) handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
#if defined(SYS_solaris) || defined(SYS_rhapsody)
  sigact.sa_flags = SA_SIGINFO;
#else
  sigact.sa_flags = 0;
#endif
  if (sigaction(sig, &sigact, &oldsigact) == -1) caml_sys_error(NO_ARG);
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(sig, act);
  if (oldact == SIG_ERR) caml_sys_error(NO_ARG);
#endif
  if (oldact == (void (*)(int)) handle_signal) {
    res = caml_alloc_small(1, 0);          /* Signal_handle */
    Field(res, 0) = Field(signal_handlers, sig);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  if (Is_block(action)) {
    if (signal_handlers == 0) {
      signal_handlers = caml_alloc(NSIG, 0);
      register_global_root(&signal_handlers);
    }
    caml_modify(&Field(signal_handlers, sig), Field(action, 0));
  }
  CAMLreturn (res);
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_sparc) && defined(SYS_sunos)
static void trap_handler(int sig, int code, 
                         struct sigcontext * context, char * address)
{
  int * sp;
  /* Unblock SIGILL */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGILL);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  if (code != ILL_TRAP_FAULT(5)) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n", code);
    exit(100);
  }
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from the %l5 and %l6 regs */
  sp = (int *) context->sc_sp;
  caml_exception_pointer = (char *) sp[5];
  caml_young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && defined(SYS_solaris)
static void trap_handler(int sig, siginfo_t * info, void * context)
{
  long * sp;

  if (info->si_code != ILL_ILLTRP) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n",
            info->si_code);
    exit(100);
  }
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from the %l5 and %l6 regs */
  sp = (long *) (((ucontext_t *)context)->uc_mcontext.gregs[REG_SP]);
  caml_exception_pointer = (char *) sp[5];
  caml_young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && (defined(SYS_bsd) || defined(SYS_linux))
static void trap_handler(int sig)
{
  /* TODO: recover registers from context and call array_bound_error */
  caml_fatal_error("Fatal error: out-of-bound access in array or string\n");
}
#endif

#if defined(TARGET_power) && defined(SYS_aix)
static void trap_handler(int sig, int code, STRUCT_SIGCONTEXT * context)
{
  /* Unblock SIGTRAP */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGTRAP);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from registers 31 and 29 */
  caml_exception_pointer = (char *) CONTEXT_GPR(context, 29);
  caml_young_ptr = (char *) CONTEXT_GPR(context, 31);
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(SYS_elf)
static void trap_handler(int sig, struct sigcontext * context)
{
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from registers 31 and 29 */
  caml_exception_pointer = (char *) context->regs->gpr[29];
  caml_young_ptr = (char *) context->regs->gpr[31];
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(SYS_rhapsody)
static void trap_handler(int sig, int code, STRUCT_SIGCONTEXT * context)
{
  /* Unblock SIGTRAP */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGTRAP);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from registers 31 and 29 */
  caml_exception_pointer = (char *) CONTEXT_GPR(context, 29);
  caml_young_ptr = (char *) CONTEXT_GPR(context, 31);
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(SYS_bsd)
static void trap_handler(int sig, int code, struct sigcontext * context)
{
  /* Recover [caml_young_ptr] and [caml_exception_pointer]
     from registers 31 and 29 */
  caml_exception_pointer = (char *) context->sc_frame.fixreg[29];
  caml_young_ptr = (char *) context->sc_frame.fixreg[31];
  array_bound_error();
}
#endif


/* Machine- and OS-dependent handling of stack overflow */

#ifdef HAS_STACK_OVERFLOW_DETECTION

static char * system_stack_top;
static char sig_alt_stack[SIGSTKSZ];

static int is_stack_overflow(char * fault_addr)
{
  struct rlimit limit;
  struct sigaction act;

  /* Sanity checks:
     - faulting address is word-aligned
     - faulting address is within the stack */
  if (((long) fault_addr & (sizeof(long) - 1)) == 0 &&
      getrlimit(RLIMIT_STACK, &limit) == 0 &&
      fault_addr < system_stack_top &&
      fault_addr >= system_stack_top - limit.rlim_cur - 0x2000) {
    /* OK, caller can turn this into a Stack_overflow exception */
    return 1;
  } else {
    /* Otherwise, deactivate our exception handler.  Caller will
       return, causing fatal signal to be generated at point of error. */
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
    return 0;
  }
}

#if defined(TARGET_i386) && defined(SYS_linux_elf)
static void segv_handler(int signo, struct sigcontext sc)
{
  if (is_stack_overflow((char *) sc.cr2))
    raise_stack_overflow();
}
#endif

#if defined(TARGET_i386) && !defined(SYS_linux_elf)
static void segv_handler(int signo, siginfo_t * info, void * arg)
{
  if (is_stack_overflow((char *) info->si_addr))
    raise_stack_overflow();
}
#endif

#endif

/* Initialization of signal stuff */

void init_signals(void)
{
  /* Bound-check trap handling */
#if defined(TARGET_sparc) && \
      (defined(SYS_sunos) || defined(SYS_bsd) || defined(SYS_linux))
  {
    struct sigaction act;
    act.sa_handler = (void (*)(int)) trap_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGILL, &act, NULL);
  }
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
  {
    struct sigaction act;
    act.sa_sigaction = trap_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_SIGINFO | SA_NODEFER;
    sigaction(SIGILL, &act, NULL);
  }
#endif
#if defined(TARGET_power)
  {
    struct sigaction act;
    act.sa_handler = (void (*)(int)) trap_handler;
    sigemptyset(&act.sa_mask);
#if defined (SYS_rhapsody)
    act.sa_flags = SA_SIGINFO;
#elif defined (SYS_aix)
    act.sa_flags = 0;
#else
    act.sa_flags = SA_NODEFER;
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
#if defined(TARGET_i386) && defined(SYS_linux_elf)
    act.sa_handler = (void (*)(int)) segv_handler;
    act.sa_flags = SA_ONSTACK | SA_NODEFER;
#else
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_NODEFER;
#endif
    sigemptyset(&act.sa_mask);
    system_stack_top = (char *) &act;
    if (sigaltstack(&stk, NULL) == 0) { sigaction(SIGSEGV, &act, NULL); }
  }
#endif
}
