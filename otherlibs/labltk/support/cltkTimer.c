/*************************************************************************/
/*                                                                       */
/*                Objective Caml LablTk library                          */
/*                                                                       */
/*         Francois Rouaix, Francois Pessaux and Jun Furuse              */
/*               projet Cristal, INRIA Rocquencourt                      */
/*            Jacques Garrigue, Kyoto University RIMS                    */
/*                                                                       */
/*   Copyright 1999 Institut National de Recherche en Informatique et    */
/*   en Automatique and Kyoto University.  All rights reserved.          */
/*   This file is distributed under the terms of the GNU Library         */
/*   General Public License.                                             */
/*                                                                       */
/*************************************************************************/

/* $Id$ */

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <callback.h>
#include "camltk.h"


/* Basically the same thing as FileProc */
void TimerProc (ClientData clientdata)
{
  callback2(*handler_code,Val_long(clientdata),Val_int(0));
}

value camltk_add_timer(value milli, value cbid) /* ML */
{
  CheckInit();
  /* look at tkEvent.c , Tk_Token is an int */ 
  return (Val_int(Tcl_CreateTimerHandler(Int_val(milli), TimerProc, 
                                       (ClientData) (Int_val(cbid)))));
}

value camltk_rem_timer(value token) /* ML */
{
  Tcl_DeleteTimerHandler((Tcl_TimerToken) Int_val(token));
  return Val_unit;
}

