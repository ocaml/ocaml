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
/*   General Public License, with the special exception on linking       */
/*   described in file ../../../LICENSE.                                 */
/*                                                                       */
/*************************************************************************/

/* $Id$ */

/* cltkMisc.c */
/* copy a Caml string to the C heap. Must be deallocated with stat_free */
extern char *string_to_c(value s);

/* cltkUtf.c */
extern value tcl_string_to_caml( char * );
extern char * caml_string_to_tcl( value );

/* cltkEval.c */
extern Tcl_Interp *cltclinterp; /* The Tcl interpretor */
extern value copy_string_list(int argc, char ** argv);

/* cltkCaml.c */
/* pointers to Caml values */
extern value *tkerror_exn;
extern value *handler_code;
extern int CamlCBCmd(ClientData clientdata, Tcl_Interp *interp,
                     int argc, char *argv[]);
extern void tk_error(char * errmsg) Noreturn;

/* cltkMain.c */
extern int signal_events;
extern void invoke_pending_caml_signals(ClientData clientdata);
extern Tk_Window cltk_mainWindow;
extern int cltk_slave_mode;

/* check that initialisations took place */
#define CheckInit()  if (!cltclinterp) tk_error("Tcl/Tk not initialised")

#define RCNAME ".camltkrc"
#define CAMLCB "camlcb"

