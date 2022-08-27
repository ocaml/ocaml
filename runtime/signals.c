/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Signal handling, code common to the bytecode and native systems */

#include <signal.h>
#include <errno.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/memprof.h"
#include "caml/finalise.h"

/* The set of pending signals (received but not yet processed).
   It is represented as a bit vector.
   Valid signal numbers range from 1 to NSIG - 1 included.
   (This is checked when we install a signal handler.)
   Signal 1 is the least significant bit of caml_pending_signals[0]. */

CAMLexport atomic_uintnat caml_pending_signals[NSIG_WORDS];

static caml_plat_mutex signal_install_mutex = CAML_PLAT_MUTEX_INITIALIZER;

int caml_check_pending_signals(void)
{
  int i;
  for (i = 0; i < NSIG_WORDS; i++) {
    if (atomic_load_relaxed(&caml_pending_signals[i]))
      return 1;
  }
  return 0;
}

/* Execute all pending signals */

CAMLexport value caml_process_pending_signals_exn(void)
{
  int i, j, signo;
  uintnat curr, mask ;
  value exn;
#ifdef POSIX_SIGNALS
  sigset_t set;
#endif

  /* Check that there is indeed a pending signal before issuing the
      syscall in [pthread_sigmask]. */
  if (!caml_check_pending_signals())
    return Val_unit;

#ifdef POSIX_SIGNALS
  pthread_sigmask(/* dummy */ SIG_BLOCK, NULL, &set);
#endif

  for (i = 0; i < NSIG_WORDS; i++) {
    curr = atomic_load_relaxed(&caml_pending_signals[i]);
    if (curr == 0) goto next_word;
    /* Scan curr for bits set */
    for (j = 0; j < BITS_PER_WORD; j++) {
      mask = (uintnat)1 << j;
      if ((curr & mask) == 0) goto next_bit;
      signo = i * 8 + j + 1;
#ifdef POSIX_SIGNALS
      if (sigismember(&set, signo)) goto next_bit;
#endif
      while (! atomic_compare_exchange_strong(&caml_pending_signals[i],
                                              &curr, curr & ~mask)) {
        /* curr was refreshed, test it again */
        if (curr == 0) goto next_word;
        if ((curr & mask) == 0) goto next_bit;
      }
      exn = caml_execute_signal_exn(signo, 0);
      if (Is_exception_result(exn)) return exn;
      /* curr probably changed during the evaluation of the signal handler;
         refresh it from memory */
      curr = atomic_load_relaxed(&caml_pending_signals[i]);
      if (curr == 0) goto next_word;
    next_bit: /* skip */;
    }
  next_word: /* skip */;
  }
  return Val_unit;
}

/* Record the delivery of a signal, and arrange for it to be processed
   as soon as possible:
   - via Caml_state->action_pending, processed in
     caml_process_pending_actions.
   - by playing with the allocation limit, processed in
     caml_alloc_small_dispatch.
*/
CAMLexport void caml_record_signal(int signal_number)
{
  unsigned int i;
  if (signal_number <= 0 || signal_number >= NSIG) return;
  i = signal_number - 1;
  atomic_fetch_or(&caml_pending_signals[i / BITS_PER_WORD],
                  (uintnat)1 << (i % BITS_PER_WORD));
  // FIXME: the TLS variable is not thread-safe
  caml_interrupt_self();
}

/* Management of blocking sections. */

static void caml_enter_blocking_section_default(void)
{
  caml_bt_exit_ocaml();
  caml_release_domain_lock();
}

static void caml_leave_blocking_section_default(void)
{
  caml_bt_enter_ocaml();
  caml_acquire_domain_lock();
}

CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;

CAMLexport void caml_enter_blocking_section(void)
{
  while (1){
    /* Process all pending signals now */
    caml_raise_if_exception(caml_process_pending_signals_exn());
    caml_enter_blocking_section_hook ();
    /* Check again for pending signals.
       If none, done; otherwise, try again */
    // FIXME: does this become very slow if a signal is recorded but
    // is masked for everybody in capacity of running signals at this
    // point?
    if (!caml_check_pending_signals()) break;
    caml_leave_blocking_section_hook ();
  }
}

CAMLexport void caml_enter_blocking_section_no_pending(void)
{
  caml_enter_blocking_section_hook ();
}

CAMLexport void caml_leave_blocking_section(void)
{
  int saved_errno;
  /* Save the value of errno (PR#5982). */
  saved_errno = errno;
  caml_leave_blocking_section_hook ();

  /* Some other thread may have switched [Caml_state->action_pending]
     to 0 even though there are still pending actions, e.g. a signal
     masked in the other thread.

     Another case where this is necessary (even in a single threaded
     setting) is when the blocking section unmasks a pending signal:
     If the signal is pending and masked but signals have already been
     examined by [caml_process_pending_actions], then
     [Caml_state->action_pending] is 0 but the signal needs to be
     handled at this point.

     So we force the examination of signals as soon as possible.
  */
  if (Caml_state->action_pending || caml_check_pending_signals())
    caml_set_action_pending(Caml_state);

  errno = saved_errno;
}

static value caml_signal_handlers;

void caml_init_signal_handling(void) {
  mlsize_t i;

  caml_signal_handlers = caml_alloc_shr(NSIG, 0);
  for (i = 0; i < NSIG; i++)
    Field(caml_signal_handlers, i) = Val_unit;
  caml_register_generational_global_root(&caml_signal_handlers);
}

/* Execute a signal handler immediately */

value caml_execute_signal_exn(int signal_number, int in_signal_handler)
{
  value res;
  value handler;
#ifdef POSIX_SIGNALS
  sigset_t nsigs, sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&nsigs);
  sigaddset(&nsigs, signal_number);
  pthread_sigmask(SIG_BLOCK, &nsigs, &sigs);
#endif
  handler = Field(caml_signal_handlers, signal_number);
    res = caml_callback_exn(
             handler,
             Val_int(caml_rev_convert_signal_number(signal_number)));
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    pthread_sigmask(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    pthread_sigmask(SIG_SETMASK, &sigs, NULL);
  }
#endif
  return res;
}

/* Arrange for a garbage collection to be performed as soon as possible */

void caml_request_major_slice (void)
{
  Caml_state->requested_major_slice = 1;
  caml_interrupt_self();
}

void caml_request_minor_gc (void)
{
  Caml_state->requested_minor_gc = 1;
  caml_interrupt_self();
}


/* Pending asynchronous actions ([Caml_state->action_pending])
   ===

   There are two kinds of asynchronous actions:

   - Those that cannot be delayed but never call OCaml code (STW
     interrupts, requested minor or major GC, forced systhread yield).

   - Those that may raise OCaml exceptions but can be delayed
     (asynchronous callbacks, finalisers, memprof callbacks).

   [Caml_state->action_pending] records whether an action of the
   second kind is currently pending, and is reset _at the beginning_
   of processing all actions.

   Hence, when a delayable action is pending, either
   [Caml_state->action_pending] is 1, or there is a function currently
   running which is executing all actions.

   This is used to ensure [Caml_state->young_limit] is always set
   appropriately.

   In case there are two different callbacks (say, a signal and a
   finaliser) arriving at the same time, then the processing of one
   awaits the return of the other. In case of long-running callbacks,
   we may want to run the second one without waiting the end of the
   first one. We do this by provoking an additional polling every
   minor collection and every major slice. To guarantee a low latency
   for signals, we avoid delaying signal handlers in that case by
   calling them first.
*/

CAMLno_tsan /* When called from [caml_record_signal], these memory
               accesses may not be synchronized. Otherwise we assume
               that we have unique access to dom_st. */
void caml_set_action_pending(caml_domain_state * dom_st)
{
  dom_st->action_pending = 1;
  atomic_store_rel(&dom_st->young_limit, (uintnat)-1);
}

CAMLexport int caml_check_pending_actions(void)
{
  return Caml_check_gc_interrupt(Caml_state) || Caml_state->action_pending;
}

value caml_do_pending_actions_exn(void)
{
  Caml_state->action_pending = 0;

  /* 1. Non-delayable actions that do not run OCaml code. */

  /* Do any pending STW interrupt, minor collection or major slice */
  caml_handle_gc_interrupt();
  /* [young_limit] has now been reset. */

  /* 2. Delayable actions that may raise OCaml exceptions. */

  /* Call signal handlers first */
  value exn = caml_process_pending_signals_exn();
  if (Is_exception_result(exn)) goto exception;

#if 0
  /* Call memprof callbacks */
  exn = caml_memprof_handle_postponed_exn();
  if (Is_exception_result(exn)) goto exception;
#endif

  /* Call finalisers */
  exn = caml_final_do_calls_exn();
  if (Is_exception_result(exn)) goto exception;

  return Val_unit;

exception:
  /* If an exception is raised during an asynchronous callback, then
     it might be the case that we did not run all the callbacks we
     needed. Therefore, we set [Caml_state->action_pending] again in
     order to force reexamination of callbacks. */
  caml_set_action_pending(Caml_state);
  return exn;
}

value caml_process_pending_actions_with_root_exn(value root)
{
  if (caml_check_pending_actions()) {
    CAMLparam1(root);
    value exn = caml_do_pending_actions_exn();
    if (Is_exception_result(exn)) CAMLreturn(exn);
    CAMLdrop;
  }
  return root;
}

value caml_process_pending_actions_with_root(value root)
{
  return caml_raise_if_exception(
    caml_process_pending_actions_with_root_exn(root));
}

CAMLexport value caml_process_pending_actions_exn(void)
{
  return caml_process_pending_actions_with_root_exn(Val_unit);
}

CAMLexport void caml_process_pending_actions(void)
{
  caml_process_pending_actions_with_root(Val_unit);
}

/* OS-independent numbering of signals */

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
#ifndef SIGBUS
#define SIGBUS -1
#endif
#ifndef SIGPOLL
#define SIGPOLL -1
#endif
#ifndef SIGSYS
#define SIGSYS -1
#endif
#ifndef SIGTRAP
#define SIGTRAP -1
#endif
#ifndef SIGURG
#define SIGURG -1
#endif
#ifndef SIGXCPU
#define SIGXCPU -1
#endif
#ifndef SIGXFSZ
#define SIGXFSZ -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF, SIGBUS,
  SIGPOLL, SIGSYS, SIGTRAP, SIGURG, SIGXCPU, SIGXFSZ
};

CAMLexport int caml_convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

CAMLexport int caml_rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

void * caml_init_signal_stack(void)
{
#ifdef POSIX_SIGNALS
  stack_t stk;
  stk.ss_flags = 0;
  stk.ss_size = SIGSTKSZ;
  /* The memory used for the alternate signal stack must not free'd before
     calling sigaltstack with SS_DISABLE. malloc is therefore used rather
     than caml_stat_alloc_noexc so that if a shutdown path erroneously fails
     to call caml_free_signal_stack then we have a memory leak rather than a
     nasty piece of undefined behaviour forced on the caller. */
  stk.ss_sp = malloc(stk.ss_size);
  if(stk.ss_sp == NULL) {
    return NULL;
  }
  if (sigaltstack(&stk, NULL) < 0) {
    free(stk.ss_sp);
    return NULL;
  }
  return stk.ss_sp;
#else
  return NULL;
#endif
}

void caml_free_signal_stack(void * signal_stack)
{
#ifdef POSIX_SIGNALS
  stack_t stk, disable;
  disable.ss_flags = SS_DISABLE;
  disable.ss_sp = NULL;  /* not required but avoids a valgrind false alarm */
  disable.ss_size = SIGSTKSZ; /* macOS wants a valid size here */
  if (sigaltstack(&disable, &stk) < 0) {
    caml_fatal_error("Failed to reset signal stack (err %d)", errno);
  }
  /* Check whether someone else installed their own signal stack */
  if (!(stk.ss_flags & SS_DISABLE) && stk.ss_sp != signal_stack) {
    /* Re-activate their signal stack. */
    sigaltstack(&stk, NULL);
  }
  /* Memory was allocated with malloc directly; see caml_init_signal_stack */
  free(signal_stack);
#endif
}

/* This is the alternate signal stack block for domain 0 */
static void * caml_signal_stack_0 = NULL;

void caml_init_signals(void)
{
  /* Bound-check trap handling for Power and S390x will go here eventually. */

  /* Set up alternate signal stack for domain 0 */
#ifdef POSIX_SIGNALS
  caml_signal_stack_0 = caml_init_signal_stack();
  if (caml_signal_stack_0 == NULL) {
    caml_fatal_error("Failed to allocate signal stack for domain 0");
  }

  /* gprof installs a signal handler for SIGPROF.
     Make it run on the alternate signal stack, to prevent segfaults. */
  {
    struct sigaction act;
    sigaction(SIGPROF, NULL, &act);
    if ((act.sa_flags & SA_SIGINFO) ||
        (act.sa_handler != SIG_IGN && act.sa_handler != SIG_DFL)) {
      /* found a handler */
      if ((act.sa_flags & SA_ONSTACK) == 0) {
        act.sa_flags |= SA_ONSTACK;
        sigaction(SIGPROF, &act, NULL);
      }
    }
  }
#endif
}

void caml_terminate_signals(void)
{
#ifdef POSIX_SIGNALS
  caml_free_signal_stack(caml_signal_stack_0);
  caml_signal_stack_0 = NULL;
#endif
}

/* Installation of a signal handler (as per [Sys.signal]) */

static void handle_signal(int signal_number)
{
  int saved_errno;
  /* Save the value of errno (PR#5982). */
  saved_errno = errno;
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(signal_number, handle_signal);
#endif
  caml_record_signal(signal_number);
  errno = saved_errno;
}

static int caml_set_signal_action(int signo, int action)
{
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif

  switch (action) {
  case 0:  act = SIG_DFL; break;
  case 1:  act = SIG_IGN; break;
  default: act = handle_signal; break;
  }

#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = SA_ONSTACK;
  if (sigaction(signo, &sigact, &oldsigact) == -1) return -1;
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(signo, act);
  if (oldact == SIG_ERR) return -1;
#endif
  if (oldact == handle_signal)
    return 2;
  else if (oldact == SIG_IGN)
    return 1;
  else
    return 0;
}

CAMLprim value caml_install_signal_handler(value signal_number, value action)
{
  CAMLparam2 (signal_number, action);
  CAMLlocal2 (res, tmp_signal_handlers);
  int sig, act, oldact;

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig <= 0 || sig >= NSIG)
    caml_invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = 0;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = 1;
    break;
  default:                      /* Signal_handle */
    act = 2;
    break;
  }
  oldact = caml_set_signal_action(sig, act);
  switch (oldact) {
  case 0:                       /* was Signal_default */
    res = Val_int(0);
    break;
  case 1:                       /* was Signal_ignore */
    res = Val_int(1);
    break;
  case 2:                       /* was Signal_handle */
    res = caml_alloc_small (1, 0);
    Field(res, 0) = Field(caml_signal_handlers, sig);
    break;
  default:                      /* error in caml_set_signal_action */
    caml_sys_error(NO_ARG);
  }
  if (Is_block(action)) {
    /* Speculatively allocate this so we don't hold the lock for
       a GC */
    if (caml_signal_handlers == 0) {
      tmp_signal_handlers = caml_alloc(NSIG, 0);
    }
    caml_plat_lock(&signal_install_mutex);
    if (caml_signal_handlers == 0) {
      /* caml_alloc cannot raise asynchronous exceptions from signals
         so this is safe */
      caml_signal_handlers = tmp_signal_handlers;
      caml_register_global_root(&caml_signal_handlers);
    }
    caml_modify(&Field(caml_signal_handlers, sig), Field(action, 0));
    caml_plat_unlock(&signal_install_mutex);
  }
  caml_raise_if_exception(caml_process_pending_signals_exn());
  CAMLreturn (res);
}
