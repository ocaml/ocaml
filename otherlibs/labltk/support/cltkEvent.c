/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of Objective Caml            */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the Objective Caml source tree. */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include "camltk.h"

CAMLprim value camltk_tk_mainloop(void)
{
  CheckInit();

  if (cltk_slave_mode) return Val_unit;

  if (!signal_events) {
    /* Initialise signal handling */
    signal_events = 1;
    Tk_CreateTimerHandler(100, invoke_pending_caml_signals, NULL);
  }
  Tk_MainLoop();
  return Val_unit;
}

/* Note: this HAS to be reported "as-is" in ML source */
static int event_flag_table[] = {
  TK_DONT_WAIT, TK_X_EVENTS, TK_FILE_EVENTS, TK_TIMER_EVENTS, TK_IDLE_EVENTS,
  TK_ALL_EVENTS
};

CAMLprim value camltk_dooneevent(value flags)
{
  int ret;

  CheckInit();

  ret = Tk_DoOneEvent(convert_flag_list(flags, event_flag_table));
  return Val_int(ret);
}
