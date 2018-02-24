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

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS
CAMLdata intnat volatile caml_signals_are_pending;
CAMLdata intnat volatile caml_pending_signals[];
CAMLdata int volatile caml_something_to_do;
CAMLdata int volatile caml_requested_major_slice;
CAMLdata int volatile caml_requested_minor_gc;

void caml_request_major_slice (void);
void caml_request_minor_gc (void);
CAMLpublic int caml_convert_signal_number (int);
CAMLpublic int caml_rev_convert_signal_number (int);
void caml_execute_signal(int signal_number, int in_signal_handler);
CAMLpublic void caml_record_signal(int signal_number);
CAMLpublic void caml_process_pending_signals(void);
void caml_process_event(void);
int caml_set_signal_action(int signo, int action);

CAMLdata void (*caml_enter_blocking_section_hook)(void);
CAMLdata void (*caml_leave_blocking_section_hook)(void);
CAMLdata int (*caml_try_leave_blocking_section_hook)(void);
CAMLdata void (* volatile caml_async_action_hook)(void);
#endif /* CAML_INTERNALS */

CAMLpublic void caml_enter_blocking_section (void);
CAMLpublic void caml_leave_blocking_section (void);

#ifdef __cplusplus
}
#endif

#endif /* CAML_SIGNALS_H */
