/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "signals.h"

void enter_blocking_section()
{
}

void leave_blocking_section()
{
}

value install_signal_handler(signal_number, action) /* ML */
     value signal_number, action;
{
  invalid_argument("Sys.signal: not implemented");
  return Val_unit;
}
