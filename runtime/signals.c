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
#include "caml/printexc.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/signals_machdep.h"
#include "caml/sys.h"
#include "caml/memprof.h"
#include "caml/finalise.h"

#ifndef NSIG
#define NSIG 64
#endif


/* [action_pending] is the minimum mask level under which there is no
   action to execute; in other words there might be an action to
   execute if action_pending > Caml_state->mask_async_callbacks.

   More specifically:

   - When a non-raising action might be pending, either
     [action_pending] is equal to CAML_MASK_NONPREEMPTIBLE, or it is
     about to be executed.

   - When a possibly-raising asynchronous callback might be pending,
     either [action_pending] is greater or equal to
     CAML_MASK_UNINTERRUPTIBLE, or it is about to be executed.

   In the case where there are two different callbacks (say, a signal
   and a finaliser) arriving at the same time, then the processing of
   one awaits the return of the other. In case of long-running
   callbacks, we may want to run the second one without waiting the
   end of the first one. We do this by provoking an additional polling
   at every minor collection and every major slice. To guarantee a low
   latency for signals, we avoid delaying signal handlers in that case
   by calling them first.

   Races are avoided by never increasing [action_pending] to something
   else than CAML_MASK_NONPREEMTIBLE (at worst, a redundant and
   innocuous polling happens), and only decreasing [action_pending]
   before executing the corresponding actions.

   FIXME: We could get into caml_process_pending_actions when
   action_pending is seen as set but not caml_pending_signals,
   making us miss the signal.
*/
static caml_mask_kind volatile action_pending = CAML_MASK_NONE;

/* The set of pending signals (received but not yet processed) */
CAMLexport intnat volatile caml_pending_signals[NSIG];
static caml_mask_kind pending_signals_mask[NSIG];

#ifdef POSIX_SIGNALS
/* This wrapper makes [sigprocmask] compatible with
   [pthread_sigmask]. Indeed, the latter returns the error code while
   the former sets [errno].
 */
static int sigprocmask_wrapper(int how, const sigset_t *set, sigset_t *oldset) {
  if(sigprocmask(how, set, oldset) != 0) return errno;
  else return 0;
}

CAMLexport int (*caml_sigmask_hook)(int, const sigset_t *, sigset_t *)
  = sigprocmask_wrapper;
#endif

static int check_for_pending_signals(caml_mask_kind mask)
{
  int i;
  for (i = 0; i < NSIG; i++) {
    if (caml_pending_signals[i] && pending_signals_mask[i] > mask) return 1;
  }
  return 0;
}

/* Execute all pending signals for that mask level */

static value process_pending_signals_exn(caml_mask_kind mask)
{
  int i;
#ifdef POSIX_SIGNALS
  sigset_t set;
#endif

  /* Check that there is indeed a pending signal before issuing the
     syscall in [caml_sigmask_hook]. */
  if (!check_for_pending_signals(mask))
    return Val_unit;

#ifdef POSIX_SIGNALS
  caml_sigmask_hook(/* dummy */ SIG_BLOCK, NULL, &set);
#endif
  for (i = 0; i < NSIG; i++) {
    caml_mask_kind signal_level = pending_signals_mask[i];
    if (!caml_pending_signals[i] || signal_level <= mask)
      continue;
#ifdef POSIX_SIGNALS
    if(sigismember(&set, i))
      continue;
#endif
    caml_pending_signals[i] = 0;
    {
      value exn = caml_execute_signal_exn(i, 0);
      if (Is_exception_result(exn)) {
        if (signal_level == CAML_MASK_NONPREEMPTIBLE) {
          caml_fatal_uncaught_exception(exn);
        } else {
          return exn;
        }
      }
    }
  }
  return Val_unit;
}

static void notify_action()
{
  if (!caml_check_pending_actions()) return;
  Caml_state->young_limit = Caml_state->young_alloc_end;
}

CAMLno_tsan /* When called from [caml_record_signal], these memory
               accesses may not be synchronized. */
void caml_set_action_pending(void)
{
  action_pending = CAML_MASK_NONPREEMPTIBLE;

  /* When this function is called without [caml_c_call] (e.g., in
     [caml_modify]), this is only moderately effective on ports that cache
     [Caml_state->young_limit] in a register, so it may take a while before the
     register is reloaded from [Caml_state->young_limit]. */
  notify_action();
}

/* Record the delivery of a signal, and arrange for it to be processed
   as soon as possible:
   - via action_pending, processed in caml_process_pending_actions_exn.
   - by playing with the allocation limit, processed in
     caml_alloc_small_dispatch when called from OCaml.
*/

CAMLno_tsan
CAMLexport void caml_record_signal(int signal_number)
{
  if (signal_number < 0 || signal_number >= NSIG) return;
  caml_pending_signals[signal_number] = 1;
  caml_set_action_pending();
}

/* Management of blocking sections. */

static void caml_enter_blocking_section_default(void)
{
}

static void caml_leave_blocking_section_default(void)
{
}

CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;

CAMLno_tsan /* The read of [action_pending] is not synchronized. */
CAMLexport void caml_enter_blocking_section(void)
{
  while (1){
    /* Process all pending signals now */
    caml_raise_if_exception(caml_process_pending_actions_exn());
    caml_enter_blocking_section_hook ();
    /* Check if some signal arrived meanwhile (in which case
       action_pending is at the max level).
       If none, done; otherwise, try again */
    if (action_pending != CAML_MASK_NONPREEMPTIBLE
        || !caml_check_pending_actions())
      break;
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

  /* Some other thread may have lowered [action_pending] even though
     there are still pending signals (masked in the other thread, in
     the sense of POSIX signal masks). To handle this case, we force
     re-examination of all signals by setting it back to 1.

     Another case where this is necessary (even in a single threaded
     setting) is when the blocking section (POSIX-)unmasks a pending
     signal: If the signal is pending and (POSIX-)masked but has
     already been examined by [caml_process_pending_actions], then
     [action_pending] has been lowered but the signal needs to be
     handled at this point. */
  if (check_for_pending_signals(CAML_MASK_NONE))
    caml_set_action_pending();

  errno = saved_errno;
}

CAMLexport caml_mask_kind caml_mask(caml_mask_kind new_mask)
{
  caml_mask_kind old_mask = Caml_state->mask_async_callbacks;
  if (old_mask < new_mask) {
    Caml_state->mask_async_callbacks = new_mask;
  }
  return old_mask;
}

CAMLexport void caml_unmask(caml_mask_kind old_mask)
{
  CAMLassert(old_mask <= Caml_state->mask_async_callbacks);
  if (Caml_state->mask_async_callbacks != old_mask) {
    Caml_state->mask_async_callbacks = old_mask;
    notify_action();
  }
}

// ML, noalloc
CAMLprim value caml_sys_set_mask(value mask)
{
  return Val_int(caml_mask(Int_val(mask)));
}

// ML, noalloc
CAMLprim value caml_sys_unset_mask(value mask)
{
  caml_unmask(Int_val(mask));
  return Val_unit;
}

/* Execute a signal handler immediately */

static value caml_signal_handlers = 0;

static value execute_with_mask_exn(value handler, int signal_number)
{
  value sig = Val_int(caml_rev_convert_signal_number(signal_number));
  value res;
  CAMLassert(Is_block(handler));
  if (pending_signals_mask[signal_number] > CAML_MASK_UNINTERRUPTIBLE) {
    caml_mask_kind old_mask = caml_mask(CAML_MASK_UNINTERRUPTIBLE);
    res = caml_callback_exn(handler, sig);
    caml_unmask(old_mask);
  } else {
    res = caml_callback_exn(handler, sig);
  }
  return res;
}


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
  caml_sigmask_hook(SIG_BLOCK, &nsigs, &sigs);
#endif
  handler = Field(caml_signal_handlers, signal_number);
  res = execute_with_mask_exn(handler, signal_number);
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    caml_sigmask_hook(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    caml_sigmask_hook(SIG_SETMASK, &sigs, NULL);
  }
#endif
  return res;
}

void caml_update_young_limit (void)
{
  CAMLassert(Caml_state->young_alloc_start <= caml_memprof_young_trigger &&
             caml_memprof_young_trigger <= Caml_state->young_alloc_end);
  CAMLassert(Caml_state->young_alloc_start <= Caml_state->young_trigger &&
             Caml_state->young_trigger < Caml_state->young_alloc_end);

  /* The minor heap grows downwards. The first trigger is the largest one. */
  if (Caml_state->requested_major_slice || Caml_state->requested_minor_gc) {
    Caml_state->young_limit_c = Caml_state->young_alloc_end;
  } else {
    Caml_state->young_limit_c =
      caml_memprof_young_trigger < Caml_state->young_trigger ?
      Caml_state->young_trigger : caml_memprof_young_trigger;
  }

  Caml_state->young_limit = Caml_state->young_limit_c;

  notify_action();
}

/* Arrange for a garbage collection to be performed as soon as possible */

static void set_urgent_gc(void)
{
  action_pending = CAML_MASK_NONPREEMPTIBLE;
  caml_update_young_limit();
}

void caml_request_major_slice (void)
{
  Caml_state->requested_major_slice = 1;
  set_urgent_gc();
}

void caml_request_minor_gc (void)
{
  Caml_state->requested_minor_gc = 1;
  set_urgent_gc();
}

CAMLno_tsan /* The access to [action_pending] is not synchronized. */
int caml_check_pending_actions()
{
  return action_pending > Caml_state->mask_async_callbacks;
}

// Execute actions according to the mask, independently of the value
// of action_pending.
value caml_do_pending_actions_exn(void)
{
  value exn;
  caml_mask_kind mask = Caml_state->mask_async_callbacks;

  action_pending = (action_pending > mask) ? mask : action_pending;

  // Do any pending minor collection or major slice
  caml_check_urgent_gc(Val_unit);
  caml_update_young_limit();
  if (mask == CAML_MASK_NONPREEMPTIBLE) return Val_unit;

  // Execute non-raising signals
  process_pending_signals_exn(CAML_MASK_UNINTERRUPTIBLE);
  if (mask == CAML_MASK_UNINTERRUPTIBLE) return Val_unit;

  // Now execute other async callbacks

  // Call signal handlers first (see documentation of action_pending).
  exn = process_pending_signals_exn(CAML_MASK_NONE);
  if (Is_exception_result(exn)) goto exception;

  // Call memprof callbacks
  exn = caml_memprof_handle_postponed_exn();
  if (Is_exception_result(exn)) goto exception;

  // Call finalisers
  exn = caml_final_do_calls_exn();
  if (Is_exception_result(exn)) goto exception;

  return Val_unit;

exception:
  /* If an exception is raised during an asynchronous callback, then
     it might be the case that we did not run all the callbacks we
     needed. Therefore, we set [action_pending] again in order
     to force reexamination of callbacks. */
  caml_set_action_pending();
  return exn;
}

CAMLno_tsan /* The access to [action_pending] is not synchronized. */
Caml_inline value process_pending_actions_with_root_exn(value extra_root)
{
  if (caml_check_pending_actions()) {
    CAMLparam1(extra_root);
    value exn = caml_do_pending_actions_exn();
    if (Is_exception_result(exn))
      CAMLreturn(exn);
    CAMLdrop;
  }
  return extra_root;
}

value caml_process_pending_actions_with_root_exn(value extra_root)
{
  return process_pending_actions_with_root_exn(extra_root);
}

value caml_process_pending_actions_with_root(value extra_root)
{
  value res = process_pending_actions_with_root_exn(extra_root);
  return caml_raise_if_exception(res);
}

CAMLexport value caml_process_pending_actions_exn(void)
{
  return process_pending_actions_with_root_exn(Val_unit);
}

CAMLexport void caml_process_pending_actions(void)
{
  value exn = process_pending_actions_with_root_exn(Val_unit);
  caml_raise_if_exception(exn);
}

/* [ int -> unit ]. Pretend that the signal [signal_number] is
   received. This is meant for testing. */
CAMLprim value caml_sys_record_signal(value signal_number)
{
  int sig = caml_convert_signal_number(Int_val(signal_number));
  if (caml_signal_handlers == 0 || !Is_block(Field(caml_signal_handlers, sig)))
    caml_invalid_argument("caml_sys_record_signal: no signal handler "
                          "installed for this signal");
  caml_record_signal(sig);
  return Val_unit;
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

/* Installation of a signal handler (as per [Sys.signal]) */

static value install_signal_handler(value signal_number,
                                    value action,
                                    caml_mask_kind mask)
{
  CAMLparam2 (signal_number, action);
  CAMLlocal1 (res);
  int sig, act, oldact;

  sig = caml_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG)
    caml_invalid_argument("Sys.signal: unavailable signal");

  pending_signals_mask[sig] = mask;

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
    if (caml_signal_handlers == 0) {
      caml_signal_handlers = caml_alloc(NSIG, 0);
      caml_register_global_root(&caml_signal_handlers);
    }
    caml_modify(&Field(caml_signal_handlers, sig), Field(action, 0));
  }
  caml_raise_if_exception(caml_process_pending_actions_exn());
  CAMLreturn (res);
}

CAMLprim value caml_install_signal_handler(value signal_number,
                                           value action)
{
  return install_signal_handler(signal_number, action,
                                CAML_MASK_UNINTERRUPTIBLE);
}

CAMLprim value caml_install_nonraising_signal_handler(value signal_number,
                                                      value action)
{
  return install_signal_handler(signal_number, action,
                                CAML_MASK_NONPREEMPTIBLE);
}
