/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _signals_
#define _signals_

#include "misc.h"
#include "mlvalues.h"

extern value signal_handlers;
extern volatile int pending_signal;
extern volatile int something_to_do;
extern volatile int force_major_slice;
extern volatile int async_signal_mode;

void enter_blocking_section (void);
void leave_blocking_section (void);
void urge_major_slice (void);
int convert_signal_number (int);

extern void (*enter_blocking_section_hook)();
extern void (*leave_blocking_section_hook)();

#endif /* _signals_ */

