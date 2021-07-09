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

#ifndef CAML_SIGNALS_H
#define CAML_SIGNALS_H

#if defined(CAML_INTERNALS) && defined(POSIX_SIGNALS)
#include<signal.h>
#endif

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern void caml_enter_blocking_section (void);
CAMLextern void caml_enter_blocking_section_no_pending (void);
CAMLextern void caml_leave_blocking_section (void);

CAMLextern void caml_process_pending_actions (void);
/* Checks for pending actions and executes them. This includes pending
   minor and major collections, signal handlers, finalisers, and
   Memprof callbacks. Assumes that the runtime lock is held. Can raise
   exceptions asynchronously into OCaml code. */

CAMLextern int caml_check_pending_actions (void);
/* Returns 1 if there are pending actions, 0 otherwise. */

CAMLextern value caml_process_pending_actions_exn (void);
/* Same as [caml_process_pending_actions], but returns the exception
   if any (otherwise returns [Val_unit]). */

#ifdef CAML_INTERNALS
CAMLextern intnat volatile caml_pending_signals[];

/* When an action is pending, either [caml_something_to_do] is 1, or
   there is a function currently running which will end by either
   executing all actions, or set [caml_something_to_do] back to 1. We
   set it to 0 when starting executing all callbacks.

   In the case there are two different callbacks (say, a signal and a
   finaliser) arriving at the same time, then the processing of one
   awaits the return of the other. In case of long-running callbacks,
   we may want to run the second one without waiting the end of the
   first one. We do this by provoking an additional polling every
   minor collection and every major slice. To guarantee a low latency
   for signals, we avoid delaying signal handlers in that case by
   calling them first.

   FIXME: We could get into caml_process_pending_actions when
   caml_something_to_do is seen as set but not caml_pending_signals,
   making us miss the signal.
*/
CAMLextern int volatile caml_something_to_do;

/* Global variables moved to Caml_state in 4.10 */
#define caml_requested_major_slice (Caml_state_field(requested_major_slice))
#define caml_requested_minor_gc (Caml_state_field(requested_minor_gc))

void caml_update_young_limit(void);
void caml_request_major_slice (void);
void caml_request_minor_gc (void);
CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);
value caml_execute_signal_exn(int signal_number, int in_signal_handler);
CAMLextern void caml_record_signal(int signal_number);
CAMLextern value caml_process_pending_signals_exn(void);
void caml_set_action_pending (void);
value caml_do_pending_actions_exn (void);
value caml_process_pending_actions_with_root (value extra_root); // raises
value caml_process_pending_actions_with_root_exn (value extra_root);
int caml_set_signal_action(int signo, int action);
CAMLextern void caml_setup_stack_overflow_detection(void);

CAMLextern void (*caml_enter_blocking_section_hook)(void);
CAMLextern void (*caml_leave_blocking_section_hook)(void);
#ifdef POSIX_SIGNALS
CAMLextern int (*caml_sigmask_hook)(int, const sigset_t *, sigset_t *);
#endif
#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_SIGNALS_H */
