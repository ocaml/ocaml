/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of OCaml                     */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the OCaml source tree.          */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include <callback.h>
#include <signals.h>
#include <fail.h>
#ifdef HAS_UNISTD
#include <unistd.h>  /* for R_OK */
#endif
#include "camltk.h"

#ifndef R_OK
#define R_OK 4
#endif

/*
 * Dealing with signals: when a signal handler is defined in OCaml,
 * the actual execution of the signal handler upon reception of the
 * signal is delayed until we are sure we are out of the GC.
 * If a signal occurs during the MainLoop, we would have to wait
 *  the next event for the handler to be invoked.
 * The following function will invoke a pending signal handler if any,
 * and we put in on a regular timer.
 */

#define SIGNAL_INTERVAL 300

int signal_events = 0; /* do we have a pending timer */

void invoke_pending_caml_signals (ClientData clientdata)
{
  signal_events = 0;
  enter_blocking_section(); /* triggers signal handling */
  /* Rearm timer */
  Tk_CreateTimerHandler(SIGNAL_INTERVAL, invoke_pending_caml_signals, NULL);
  signal_events = 1;
  leave_blocking_section();
}

/* Now the real Tk stuff */

Tk_Window cltk_mainWindow;


/* In slave mode, the interpreter *already* exists */
int cltk_slave_mode = 0;

/* Initialisation, based on tkMain.c */
CAMLprim value camltk_opentk(value argv)
{
  CAMLparam1(argv);
  CAMLlocal1(tmp);
  char *argv0;

  /* argv must contain argv[0], the application command name */
  tmp = Val_unit;

  if ( argv == Val_int(0) ){
    failwith("camltk_opentk: argv is empty");
  }
  argv0 = String_val( Field( argv, 0 ) );

  if (!cltk_slave_mode) {
    /* Create an interpreter, dies if error */
#if TCL_MAJOR_VERSION >= 8
    Tcl_FindExecutable(String_val(argv0));
#endif
    cltclinterp = Tcl_CreateInterp();
    {
      /* Register cltclinterp for use in other related extensions */
      value *interp = caml_named_value("cltclinterp");
      if (interp != NULL)
        Store_field(*interp,0,copy_nativeint((intnat)cltclinterp));
    }

    if (Tcl_Init(cltclinterp) != TCL_OK)
      tk_error(Tcl_GetStringResult(cltclinterp));
    Tcl_SetVar(cltclinterp, "argv0", String_val (argv0), TCL_GLOBAL_ONLY);

    { /* Sets argv */
      int argc = 0;

      tmp = Field(argv, 1); /* starts from argv[1] */
      while ( tmp != Val_int(0) ) {
        argc++;
        tmp = Field(tmp, 1);
      }

      if( argc != 0 ){
        int i;
        char *args;
        char **tkargv;
        char argcstr[256]; /* string of argc */

        tkargv = (char**)stat_alloc(sizeof( char* ) * argc );
        tmp = Field(argv, 1); /* starts from argv[1] */
        i = 0;

        while ( tmp != Val_int(0) ) {
          tkargv[i] = String_val(Field(tmp, 0));
          tmp = Field(tmp, 1);
          i++;
        }

        sprintf( argcstr, "%d", argc );
        Tcl_SetVar(cltclinterp, "argc", argcstr, TCL_GLOBAL_ONLY);
        args = Tcl_Merge(argc, (const char *const*)tkargv); /* args must be freed by Tcl_Free */
        Tcl_SetVar(cltclinterp, "argv", args, TCL_GLOBAL_ONLY);
        Tcl_Free(args);
        stat_free( tkargv );
      }
    }
    if (Tk_Init(cltclinterp) != TCL_OK)
      tk_error(Tcl_GetStringResult(cltclinterp));

    /* Retrieve the main window */
    cltk_mainWindow = Tk_MainWindow(cltclinterp);

    if (NULL == cltk_mainWindow)
      tk_error(Tcl_GetStringResult(cltclinterp));

    Tk_GeometryRequest(cltk_mainWindow,200,200);
  }

  /* Create the camlcallback command */
  Tcl_CreateCommand(cltclinterp,
                    CAMLCB, CamlCBCmd,
                    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar(cltclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  /* Our hack for implementing break in callbacks */
  Tcl_SetVar(cltclinterp, "BreakBindingsSequence", "0", TCL_GLOBAL_ONLY);

  /* Load the traditional rc file */
  {
    char *home = getenv("HOME");
    if (home != NULL) {
      char *f = stat_alloc(strlen(home)+strlen(RCNAME)+2);
      f[0]='\0';
      strcat(f, home);
      strcat(f, "/");
      strcat(f, RCNAME);
      if (0 == access(f,R_OK))
        if (TCL_OK != Tcl_EvalFile(cltclinterp,f)) {
          stat_free(f);
          tk_error(Tcl_GetStringResult(cltclinterp));
        };
      stat_free(f);
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value camltk_finalize(value unit) /* ML */
{
  Tcl_Finalize();
  return Val_unit;
}
