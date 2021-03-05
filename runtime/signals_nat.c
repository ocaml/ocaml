/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Signal handling, code specific to the native-code compiler */

#if defined(TARGET_amd64) && defined (SYS_linux)
#define _GNU_SOURCE
#endif
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/signals_machdep.h"
#include "signals_osdep.h"
#include "caml/stack.h"
#include "caml/spacetime.h"
#include "caml/memprof.h"
#include "caml/finalise.h"

#ifndef NSIG
#define NSIG 64
#endif

typedef void (*signal_handler)(int signo);

#ifdef _WIN32
extern signal_handler caml_win32_signal(int sig, signal_handler action);
#define signal(sig,act) caml_win32_signal(sig,act)
extern void caml_win32_overflow_detection();
#endif

extern char * caml_code_area_start, * caml_code_area_end;
extern char caml_system__code_begin, caml_system__code_end;

/* Do not use the macro from address_class.h here. */
#undef Is_in_code_area
#define Is_in_code_area(pc) \
 ( ((char *)(pc) >= caml_code_area_start && \
    (char *)(pc) <= caml_code_area_end)     \
|| ((char *)(pc) >= &caml_system__code_begin && \
    (char *)(pc) <= &caml_system__code_end)     \
|| (Classify_addr(pc) & In_code_area) )

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to OCaml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection(void)
{
  /* TEMPORARY: if we have just sampled an allocation in native mode,
     we simply renew the sample to ignore it. Otherwise, renewing now
     will not have any effect on the sampling distribution, because of
     the memorylessness of the Bernoulli process.

     FIXME: if the sampling rate is 1, this leads to infinite loop,
     because we are using a binomial distribution in [memprof.c]. This
     will go away when the sampling of natively allocated blocks will
     be correctly implemented.
  */
  caml_memprof_renew_minor_sample();
  if (Caml_state->requested_major_slice || Caml_state->requested_minor_gc ||
      Caml_state->young_ptr - Caml_state->young_trigger < Max_young_whsize){
    caml_gc_dispatch ();
  }

#ifdef WITH_SPACETIME
  if (Caml_state->young_ptr == Caml_state->young_alloc_end) {
    caml_spacetime_automatic_snapshot();
  }
#endif

  caml_raise_if_exception(caml_do_pending_actions_exn());
}

DECLARE_SIGNAL_HANDLER(handle_signal)
{
  int saved_errno;
  /* Save the value of errno (PR#5982). */
  saved_errno = errno;
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(sig, handle_signal);
#endif
  if (sig < 0 || sig >= NSIG) return;
  if (caml_try_leave_blocking_section_hook ()) {
    caml_raise_if_exception(caml_execute_signal_exn(sig, 1));
    caml_enter_blocking_section_hook();
  } else {
    caml_record_signal(sig);
  /* Some ports cache [Caml_state->young_limit] in a register.
     Use the signal context to modify that register too, but only if
     we are inside OCaml code (not inside C code). */
#if defined(CONTEXT_PC) && defined(CONTEXT_YOUNG_LIMIT)
    if (Is_in_code_area(CONTEXT_PC))
      CONTEXT_YOUNG_LIMIT = (context_reg) Caml_state->young_limit;
#endif
  }
  errno = saved_errno;
}

int caml_set_signal_action(int signo, int action)
{
  signal_handler oldact;
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#else
  signal_handler act;
#endif

#ifdef POSIX_SIGNALS
  switch(action) {
  case 0:
    sigact.sa_handler = SIG_DFL;
    sigact.sa_flags = 0;
    break;
  case 1:
    sigact.sa_handler = SIG_IGN;
    sigact.sa_flags = 0;
    break;
  default:
    SET_SIGACT(sigact, handle_signal);
    break;
  }
  sigemptyset(&sigact.sa_mask);
  if (sigaction(signo, &sigact, &oldsigact) == -1) return -1;
  oldact = oldsigact.sa_handler;
#else
  switch(action) {
  case 0:  act = SIG_DFL; break;
  case 1:  act = SIG_IGN; break;
  default: act = handle_signal; break;
  }
  oldact = signal(signo, act);
  if (oldact == SIG_ERR) return -1;
#endif
  if (oldact == (signal_handler) handle_signal)
    return 2;
  else if (oldact == SIG_IGN)
    return 1;
  else
    return 0;
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_power) \
  || defined(TARGET_s390x)
DECLARE_SIGNAL_HANDLER(trap_handler)
{
#if defined(SYS_rhapsody)
  /* Unblock SIGTRAP */
  { sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGTRAP);
    caml_sigmask_hook(SIG_UNBLOCK, &mask, NULL);
  }
#endif
  Caml_state->exception_pointer = (char *) CONTEXT_EXCEPTION_POINTER;
  Caml_state->young_ptr = (value *) CONTEXT_YOUNG_PTR;
  Caml_state->bottom_of_stack = (char *) CONTEXT_SP;
  Caml_state->last_return_address = (uintnat) CONTEXT_PC;
  caml_array_bound_error();
}
#endif

/* Machine- and OS-dependent handling of stack overflow */

#ifdef HAS_STACK_OVERFLOW_DETECTION
#ifndef CONTEXT_SP
#error "CONTEXT_SP is required if HAS_STACK_OVERFLOW_DETECTION is defined"
#endif

/* Code compiled with ocamlopt never accesses more than
   EXTRA_STACK bytes below the stack pointer. */
#define EXTRA_STACK 256

#ifdef RETURN_AFTER_STACK_OVERFLOW
extern void caml_stack_overflow(caml_domain_state*);
#endif

/* Address sanitizer is confused when running the stack overflow
   handler in an alternate stack. We deactivate it for all the
   functions used by the stack overflow handler. */
CAMLno_asan
DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct sigaction act;
  char * fault_addr;

  /* Sanity checks:
     - faulting address is word-aligned
     - faulting address is on the stack, or within EXTRA_STACK of it
     - we are in OCaml code */
  fault_addr = CONTEXT_FAULTING_ADDRESS;
  if (((uintnat) fault_addr & (sizeof(intnat) - 1)) == 0
      && fault_addr < Caml_state->top_of_stack
      && (uintnat)fault_addr >= CONTEXT_SP - EXTRA_STACK
#ifdef CONTEXT_PC
      && Is_in_code_area(CONTEXT_PC)
#endif
      ) {
#ifdef RETURN_AFTER_STACK_OVERFLOW
    /* Tweak the PC part of the context so that on return from this
       handler, we jump to the asm function [caml_stack_overflow]
       (from $ARCH.S). */
#ifdef CONTEXT_PC
    CONTEXT_C_ARG_1 = (context_reg) Caml_state;
    CONTEXT_PC = (context_reg) &caml_stack_overflow;
#else
#error "CONTEXT_PC must be defined if RETURN_AFTER_STACK_OVERFLOW is"
#endif
#else
    /* Raise a Stack_overflow exception straight from this signal handler */
#if defined(CONTEXT_YOUNG_PTR) && defined(CONTEXT_EXCEPTION_POINTER)
    Caml_state->exception_pointer == (char *) CONTEXT_EXCEPTION_POINTER;
    Caml_state->young_ptr = (value *) CONTEXT_YOUNG_PTR;
#endif
    caml_raise_stack_overflow();
#endif
  } else {
    /* Otherwise, deactivate our exception handler and return,
       causing fatal signal to be generated at point of error. */
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
  }
}

#endif

/* Initialization of signal stuff */

#ifdef HAS_STACK_OVERFLOW_DETECTION
static int setup_stack_overflow_detection(void);
#endif

void caml_init_signals(void)
{
  /* Bound-check trap handling */

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

#if defined(TARGET_s390x)
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    SET_SIGACT(act, trap_handler);
    sigaction(SIGFPE, &act, NULL);
  }
#endif

#ifdef HAS_STACK_OVERFLOW_DETECTION
  if (setup_stack_overflow_detection() != -1) {
    struct sigaction act;
    SET_SIGACT(act, segv_handler);
    act.sa_flags |= SA_ONSTACK | SA_NODEFER;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
  }
#endif
}

/* Termination of signal stuff */

#if defined(TARGET_power) || defined(TARGET_s390x) \
    || defined(HAS_STACK_OVERFLOW_DETECTION)
static void set_signal_default(int signum)
{
  struct sigaction act;
  sigemptyset(&act.sa_mask);
  act.sa_handler = SIG_DFL;
  act.sa_flags = 0;
  sigaction(signum, &act, NULL);
}
#endif

int caml_stop_stack_overflow_detection(void);

void caml_terminate_signals(void)
{
#if defined(TARGET_power)
  set_signal_default(SIGTRAP);
#endif

#if defined(TARGET_s390x)
  set_signal_default(SIGFPE);
#endif

#ifdef HAS_STACK_OVERFLOW_DETECTION
  set_signal_default(SIGSEGV);
  caml_stop_stack_overflow_detection();
#endif
}

/* Allocate and select an alternate stack for handling signals,
   especially SIGSEGV signals.
   Each thread needs its own alternate stack.
   The alternate stack used to be statically-allocated for the main thread,
   but this is incompatible with Glibc 2.34 and newer, where SIGSTKSZ
   may not be a compile-time constant (issue #10250). */

#ifdef HAS_STACK_OVERFLOW_DETECTION
static int setup_stack_overflow_detection(void)
{
  stack_t stk;
  stk.ss_sp = malloc(SIGSTKSZ);
  if (stk.ss_sp == NULL) return -1;
  stk.ss_size = SIGSTKSZ;
  stk.ss_flags = 0;
  if (sigaltstack(&stk, NULL) == -1) {
    free(stk.ss_sp);
    return -1;
  }
  /* Success (or stack overflow detection not available) */
  return 0;
}
#endif

CAMLexport void caml_setup_stack_overflow_detection(void)
{
#ifdef HAS_STACK_OVERFLOW_DETECTION
  setup_stack_overflow_detection();
#endif
}

CAMLexport int caml_stop_stack_overflow_detection(void)
{
#ifdef HAS_STACK_OVERFLOW_DETECTION
  stack_t oldstk, stk;
  stk.ss_flags = SS_DISABLE;
  if (sigaltstack(&stk, &oldstk) == -1) return -1;
  /* If caml_setup_stack_overflow_detection failed, we are not using
     an alternate signal stack.  SS_DISABLE will be set in oldstk,
     and there is nothing to free in this case. */
  if (! (oldstk.ss_flags & SS_DISABLE)) free(oldstk.ss_sp);
  return 0;
#else
  return 0;
#endif
}
