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

CAMLextern void caml_enter_blocking_section (void); // raises
CAMLextern void caml_enter_blocking_section_no_pending (void);
CAMLextern void caml_leave_blocking_section (void);

/* Asynchronous actions

   We distinguish two kinds of asynchronous actions:

   - non-raising actions, such as non-raising signal handlers (e.g.
     systhread's yield).

   - unrestricted asynchronous callbacks: possibly-raising signal
     handlers, memprof callbacks and finalisers (including GC alarms).

   Signal handlers in particular are defined as one of two kinds,
   non-raising and raising. Non-raising handlers must guarantee that
   they do not raise or fail, but also that they are brief (typically,
   systhread's yield()), because themselves cannot be interrupted.

   In contrast, asynchronous callbacks of the second kind are allowed
   to raise an exception (typically, when handling SIGINT or SIGALRM),
   are not limited in what they can express (typically, finalisers and
   memprof callbacks), and can themselves be interrupted by
   asynchronous callbacks.

   Masking temporarily delays the execution of asynchronous callbacks.
   There are two kinds of masking:

   - The _nonpreemptible mask_ delays execution of all asynchronous
     callbacks.

   - The _uninterruptible mask_ delays the execution of unrestricted
     asynchronous callbacks, whereas

   This gives three possible values for
   [Caml_state->mask_async_callbacks]: CAML_MASK_NONE,
   CAML_MASK_UNINTERRUPTIBLE and CAML_MASK_NONPREEMPTIBLE.

   In particular:

   - a program in a nonpreemptible mask will not run any OCaml code
     asynchronously or concurrently;

   - a program in an uninterruptible mask will not be interrupted by
     asynchronous exceptions.

*/

CAMLextern caml_mask_kind caml_mask (caml_mask_kind new_mask);
CAMLextern           void caml_unmask (caml_mask_kind old_mask);
/* The caml_mask function sets an uninterruptible or nonpreemptible
   mask and returns the previous mask. The caml_unmask function sets
   the previous mask back. Assumes that the runtime lock is held.

   The caml_mask function never undoes a previous mask, that is,
   calling caml_mask(CAML_MASK_UNINTERRUPTIBLE) has no effect inside an
   nonpreemptible mask. Similarly, caml_mask(CAML_MASK_NONE) has no
   effect at all, it thus can be used to query the current mask.

   Calls to caml_mask and caml_unmask must be correctly bracketed and
   caml_unmask must be supplied value returned from the corresponding
   call to caml_mask (the previous mask). It is incorrect to call
   caml_unmask with a different mask, so as to undo a mask one does
   not own.

   Example:

     caml_mask_kind old_mask = caml_mask(CAML_MASK_UNINTERRUPTIBLE);
     ...(no raising of exception)...
     caml_unmask(old_mask);
*/

CAMLextern void caml_process_pending_actions (void);
/* Checks for pending actions and executes them, depending on the
   current mask. Assumes that the runtime lock is held.

   Can raise exceptions asynchronously into OCaml code if the current
   mask is CAML_MASK_NONE.
*/

CAMLextern int caml_check_pending_actions (void);
/* Returns 1 if there are actions to do at current mask level, 0
   otherwise. */

CAMLextern value caml_process_pending_actions_exn (void);
/* Same as [caml_process_pending_actions], but returns the encoded
   exception if any (otherwise returns [Val_unit]). */

#ifdef CAML_INTERNALS
CAMLextern intnat volatile caml_pending_signals[];

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
