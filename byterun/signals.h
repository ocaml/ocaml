/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef CAML_SIGNALS_H
#define CAML_SIGNALS_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

/* <private> */
extern value signal_handlers;
CAMLextern int volatile pending_signal;
CAMLextern int volatile something_to_do;
extern int volatile force_major_slice;
CAMLextern int volatile async_signal_mode;
/* </private> */

CAMLextern void enter_blocking_section (void);
CAMLextern void leave_blocking_section (void);

/* <private> */
void urge_major_slice (void);
CAMLextern int convert_signal_number (int);
void execute_signal(int signal_number, int in_signal_handler);
void process_event(void);

CAMLextern void (*enter_blocking_section_hook)(void);
CAMLextern void (*leave_blocking_section_hook)(void);
CAMLextern void (* volatile async_action_hook)(void);
/* </private> */

#endif /* CAML_SIGNALS_H */
