/* Alternative to tkwait variable */
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "camltk.h"

value camltk_getvar(var) /* ML */
     value var;
{
  char *s;
  char *stable_var = NULL;
  CheckInit();

  stable_var = string_to_c(var);
  s = Tcl_GetVar(cltclinterp,stable_var,
		   TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);
  stat_free(stable_var);

  if (s == NULL)
    tk_error(cltclinterp->result);
  else 
    return(copy_string(s));
}

value camltk_setvar(var,contents) /* ML */
     value var;
     value contents;
{
  char *s;
  char *stable_var = NULL;
  CheckInit();

  /* SetVar makes a copy of the contents. */
  /* In case we have write traces in Caml, it's better to make sure that
     var doesn't move... */
  stable_var = string_to_c(var);
  s = Tcl_SetVar(cltclinterp,stable_var, String_val(contents),
		   TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);
  stat_free(stable_var);

  if (s == NULL)
    tk_error(cltclinterp->result);
  else 
    return(Val_unit);
}


/* The appropriate type is
typedef char *(Tcl_VarTraceProc) _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, char *part1, char *part2, int flags));
 */
static char * tracevar(clientdata, interp, name1, name2, flags)
     ClientData clientdata;
     Tcl_Interp *interp;	/* Interpreter containing variable. */
     char *name1;		/* Name of variable. */
     char *name2;		/* Second part of variable name. */
     int flags;			/* Information about what happened. */
{
  Tcl_UntraceVar2(interp, name1, name2,
		TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		tracevar, clientdata);
  callback2(*handler_code,Val_int(clientdata),Val_unit);
  return (char *)NULL;
}

/* Sets up a callback upon modification of a variable */
value camltk_trace_var(var,cbid) /* ML */
     value var;
     value cbid;
{
  char *cvar = NULL;

  CheckInit();
  /* Make a copy of var, since Tcl will modify it in place, and we
   * don't trust that much what it will do here
   */
  cvar = string_to_c(var);
  if (Tcl_TraceVar(cltclinterp, cvar,
		   TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		   tracevar,
		   (ClientData) (Long_val(cbid)))
		   != TCL_OK) {
    stat_free(cvar);
    tk_error(cltclinterp->result);
  };
  stat_free(cvar);
  return Val_unit;
}

value camltk_untrace_var(var,cbid) /* ML */
     value var;
     value cbid;
{
  char *cvar = NULL;

  CheckInit();
  /* Make a copy of var, since Tcl will modify it in place, and we
   * don't trust that much what it will do here
   */
  cvar = string_to_c(var);
  Tcl_UntraceVar(cltclinterp, cvar,
		 TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		 tracevar,
		 (ClientData) (Long_val(cbid)));
  stat_free(cvar);
  return Val_unit;
}
