#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#ifdef HAS_UNISTD
#include <unistd.h>  /* for R_OK */
#endif
#include "camltk.h"

#ifndef R_OK
#define R_OK 4
#endif

/* 
 * Dealing with signals: when a signal handler is defined in Caml,
 * the actual execution of the signal handler upon reception of the
 * signal is delayed until we are sure we are out of the GC.
 * If a signal occurs during the MainLoop, we would have to wait
 *  the next event for the handler to be invoked.
 * The following function will invoke a pending signal handler if any,
 * and we put in on a regular timer.
 */

#define SIGNAL_INTERVAL 300

int signal_events = 0; /* do we have a pending timer */

void invoke_pending_caml_signals (clientdata) 
     ClientData clientdata;
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
value camltk_opentk(display, name) /* ML */
     value display,name;
{
  if (!cltk_slave_mode) {
    /* Create an interpreter, dies if error */
#if TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION == 1
    Tcl_FindExecutable(String_val(name));
#endif
    cltclinterp = Tcl_CreateInterp();

    if (Tcl_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);
    Tcl_SetVar(cltclinterp, "argv0", String_val (name), TCL_GLOBAL_ONLY);
    { /* Sets display if needed */
      char *args;
      char *tkargv[2];
      if (string_length(display) > 0) {
	Tcl_SetVar(cltclinterp, "argc", "2", TCL_GLOBAL_ONLY);
	tkargv[0] = "-display";
	tkargv[1] = String_val(display);
	args = Tcl_Merge(2, tkargv);
	Tcl_SetVar(cltclinterp, "argv", args, TCL_GLOBAL_ONLY);
	free(args);
      }
    }
    if (Tk_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);

    /* Retrieve the main window */
    cltk_mainWindow = Tk_MainWindow(cltclinterp);

    if (NULL == cltk_mainWindow)
      tk_error(cltclinterp->result);
  
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
	  tk_error(cltclinterp->result);
	};
      stat_free(f);
    }
  }

  return Val_unit;
}

