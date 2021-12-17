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

#ifdef CAML_INTERNALS
CAMLextern atomic_intnat caml_pending_signals[];

/* Global variables moved to Caml_state in 4.10 */
#define caml_requested_major_slice (Caml_state_field(requested_major_slice))
#define caml_requested_minor_gc (Caml_state_field(requested_minor_gc))

int caml_check_for_pending_signals(void);
void caml_update_young_limit(void);
void caml_request_major_slice (void);
void caml_request_minor_gc (void);
CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);
value caml_execute_signal_exn(int signal_number, int in_signal_handler);
CAMLextern void caml_record_signal(int signal_number);
CAMLextern value caml_process_pending_signals_exn(void);
CAMLextern void caml_process_pending_signals(void);
void caml_set_action_pending (void);
int caml_set_signal_action(int signo, int action);

CAMLextern value caml_process_pending_signals_with_root_exn (value extra_root);
void caml_init_signal_handling(void);
int caml_init_signal_stack(void);
void caml_free_signal_stack(void);

CAMLextern void (*caml_enter_blocking_section_hook)(void);
CAMLextern void (*caml_leave_blocking_section_hook)(void);
#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_SIGNALS_H */
