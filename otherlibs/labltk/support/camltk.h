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

/* cltkEval.c */
extern Tcl_Interp *cltclinterp; /* The Tcl interpretor */

/* copy a Caml string to the C heap. Must be deallocated with stat_free */
char *string_to_c();

/* cltkCaml.c */
/* pointers to Caml values */
extern value *tkerror_exn;
extern value *handler_code;
int CamlCBCmd();
void tk_error();

/* cltkMain.c */
extern int signal_events;
void invoke_pending_caml_signals();
extern Tk_Window cltk_mainWindow;
extern int cltk_slave_mode;

/* check that initialisations took place */
#define CheckInit()  if (!cltclinterp) tk_error("Tcl/Tk not initialised")

#define RCNAME ".camltkrc"
#define CAMLCB "camlcb"

