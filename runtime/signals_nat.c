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
#if defined(TARGET_i386) && defined (SYS_linux_elf)
#define _GNU_SOURCE
#endif
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/signals_machdep.h"
#include "signals_osdep.h"
#include "caml/stack.h"
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
  frame_descr* d;
  intnat allocsz = 0, i, nallocs;
  unsigned char* alloc_len;

  { /* Find the frame descriptor for the current allocation */
    uintnat h = Hash_retaddr(Caml_state->last_return_address);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d->retaddr == Caml_state->last_return_address) break;
      h = (h + 1) & caml_frame_descriptors_mask;
    }
    /* Must be an allocation frame */
    CAMLassert(d && d->frame_size != 0xFFFF && (d->frame_size & 2));
  }

  /* Compute the total allocation size at this point,
     including allocations combined by Comballoc */
  alloc_len = (unsigned char*)(&d->live_ofs[d->num_live]);
  nallocs = *alloc_len++;

  if (nallocs == 0) {
    /* This is a poll */
    caml_process_pending_actions();
  }
  else
  {
    for (i = 0; i < nallocs; i++) {
      allocsz += Whsize_wosize(Wosize_encoded_alloc_len(alloc_len[i]));
    }

    /* We have computed whsize (including header), but need wosize (without) */
    allocsz -= 1;

    caml_alloc_small_dispatch(allocsz, CAML_DO_TRACK | CAML_FROM_CAML,
                              nallocs, alloc_len);
  }
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
  caml_record_signal(sig);
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
      && caml_find_code_fragment_by_pc((char *) CONTEXT_PC) != NULL
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
#if defined(CONTEXT_YOUNG_PTR)
    Caml_state->young_ptr = (value *) CONTEXT_YOUNG_PTR;
#endif
#if defined(CONTEXT_EXCEPTION_POINTER)
    Caml_state->exception_pointer = (char *) CONTEXT_EXCEPTION_POINTER;
#endif
    caml_raise_stack_overflow();
#endif
#ifdef NAKED_POINTERS_CHECKER
  } else if (Caml_state->checking_pointer_pc) {
#ifdef CONTEXT_PC
    CONTEXT_PC = (context_reg)Caml_state->checking_pointer_pc;
#else
#error "CONTEXT_PC must be defined if RETURN_AFTER_STACK_OVERFLOW is"
#endif /* CONTEXT_PC */
#endif /* NAKED_POINTERS_CHECKER */
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
static void * caml_signal_stack = NULL;
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
  caml_signal_stack = caml_setup_stack_overflow_detection();
  if (caml_signal_stack != NULL) {
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
  caml_stop_stack_overflow_detection(caml_signal_stack);
  caml_signal_stack = NULL;
#endif
}

/* Allocate and select an alternate stack for handling signals,
   especially SIGSEGV signals.
   Each thread needs its own alternate stack.
   The alternate stack used to be statically-allocated for the main thread,
   but this is incompatible with Glibc 2.34 and newer, where SIGSTKSZ
   may not be a compile-time constant (issue #10250).
   Return the dynamically-allocated alternate signal stack, or NULL
   if an error occurred.
   The returned pointer must be passed to [caml_stop_stack_overflow_detection].
*/

CAMLexport void * caml_setup_stack_overflow_detection(void)
{
#ifdef HAS_STACK_OVERFLOW_DETECTION
  stack_t stk;
  stk.ss_size = SIGSTKSZ;
  stk.ss_sp = malloc(stk.ss_size);
  if (stk.ss_sp == NULL) return NULL;
  stk.ss_flags = 0;
  if (sigaltstack(&stk, NULL) == -1) {
    free(stk.ss_sp);
    return NULL;
  }
  return stk.ss_sp;
#else
  return NULL;
#endif
}

CAMLexport int caml_stop_stack_overflow_detection(void * signal_stack)
{
#ifdef HAS_STACK_OVERFLOW_DETECTION
  stack_t oldstk, stk;
  stk.ss_flags = SS_DISABLE;
  stk.ss_sp = NULL;  /* not required but avoids a valgrind false alarm */
  stk.ss_size = SIGSTKSZ; /* macOS wants a valid size here */
  if (sigaltstack(&stk, &oldstk) == -1) return -1;
  /* Check whether someone else installed their own signal stack */
  if (!(oldstk.ss_flags & SS_DISABLE) && oldstk.ss_sp != signal_stack) {
    /* Re-activate their signal stack. */
    sigaltstack(&oldstk, NULL);
  }
  free(signal_stack);
#endif
  return 0;
}
