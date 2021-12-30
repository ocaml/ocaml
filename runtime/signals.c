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
#include "caml/signals_machdep.h"
#include "caml/sys.h"
#include "caml/memprof.h"
#include "caml/finalise.h"

#ifndef NSIG
#define NSIG 64
#endif

/* The set of pending signals (received but not yet processed) */

CAMLexport atomic_intnat caml_pending_signals[NSIG];
static caml_plat_mutex signal_install_mutex = CAML_PLAT_MUTEX_INITIALIZER;

int caml_check_for_pending_signals(void)
{
  int i;

  for (i = 0; i < NSIG; i++) {
    if (atomic_load_explicit(&caml_pending_signals[i], memory_order_seq_cst))
      return 1;
  }
  return 0;
}

/* Execute all pending signals */

CAMLexport value caml_process_pending_signals_exn(void)
{
  int i;
  intnat specific_signal_pending;
  value exn;
#ifdef POSIX_SIGNALS
  sigset_t set;
#endif

  /* Check that there is indeed a pending signal before issuing the
      syscall in [pthread_sigmask]. */
  if (!caml_check_for_pending_signals())
    return Val_unit;

#ifdef POSIX_SIGNALS
  pthread_sigmask(/* dummy */ SIG_BLOCK, NULL, &set);
#endif
  for (i = 0; i < NSIG; i++) {
    if ( atomic_load_explicit
          (&caml_pending_signals[i], memory_order_seq_cst) == 0 )
      continue;
#ifdef POSIX_SIGNALS
    if(sigismember(&set, i))
      continue;
#endif
  again:
    specific_signal_pending = atomic_load_explicit
                               (&caml_pending_signals[i], memory_order_seq_cst);
    if( specific_signal_pending > 0 ) {
      if( !atomic_compare_exchange_strong(
            &caml_pending_signals[i],
             &specific_signal_pending, 0) ) {
        /* We failed our CAS because another thread beat us to processing
           this signal. Try again to see if there are more of this signal
           to process. */
        goto again;
      }

      exn = caml_execute_signal_exn(i, 0);
      if (Is_exception_result(exn)) return exn;
    }
  }
  return Val_unit;
}

CAMLexport void caml_process_pending_signals(void) {
  value exn = caml_process_pending_signals_exn();
  caml_raise_if_exception(exn);
}

/* Record the delivery of a signal, and arrange for it to be processed
   as soon as possible:
   - via the pending signal counters, processed in
     caml_process_pending_signals_exn.
   - by playing with the allocation limit, processed in
     caml_garbage_collection
*/

CAMLexport void caml_record_signal(int signal_number)
{
  atomic_store_explicit
    (&caml_pending_signals[signal_number], 1, memory_order_seq_cst);

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
    caml_process_pending_signals();
    caml_enter_blocking_section_hook ();
    /* Check again for pending signals.
       If none, done; otherwise, try again */
    if (!caml_check_for_pending_signals()) break;
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

CAMLextern value caml_process_pending_signals_with_root_exn(value extra_root)
{
  CAMLparam1(extra_root);
  value exn = caml_process_pending_signals_exn();
  if (Is_exception_result(exn))
    CAMLreturn(exn);
  CAMLdrop;
  return extra_root;
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

int caml_init_signal_stack(void)
{
#ifdef POSIX_SIGNALS
  stack_t stk;
  stk.ss_flags = 0;
  stk.ss_size = SIGSTKSZ;
  stk.ss_sp = caml_stat_alloc_noexc(stk.ss_size);
  if(stk.ss_sp == NULL) {
    return -1;
  }
  if (sigaltstack(&stk, NULL) < 0) {
    caml_stat_free(stk.ss_sp);
    return -1;
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
  return 0;
}

void caml_free_signal_stack(void)
{
#ifdef POSIX_SIGNALS
  stack_t stk, disable = {0};
  disable.ss_flags = SS_DISABLE;
  /* POSIX says ss_size is ignored when SS_DISABLE is set,
     but OSX/Darwin fails if the size isn't set. */
  disable.ss_size = SIGSTKSZ;
  if (sigaltstack(&disable, &stk) < 0) {
    caml_fatal_error_arg("Failed to reset signal stack: %s", strerror(errno));
  }
  caml_stat_free(stk.ss_sp);
#endif
}

/* Installation of a signal handler (as per [Sys.signal]) */

CAMLprim value caml_install_signal_handler(value signal_number, value action)
{
  CAMLparam2 (signal_number, action);
  CAMLlocal2 (res, tmp_signal_handlers);
  int sig, act, oldact;

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG)
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
  caml_process_pending_signals();
  CAMLreturn (res);
}
