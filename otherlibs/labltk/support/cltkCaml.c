#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include "camltk.h"

value * tkerror_exn = NULL;
value * handler_code = NULL;

/* The Tcl command for evaluating callback in Caml */
int CamlCBCmd(clientdata, interp, argc, argv)
     ClientData clientdata;
     Tcl_Interp *interp;
     int argc;
     char *argv[];
{
  CheckInit();

  /* Assumes no result */
  Tcl_SetResult(interp, NULL, NULL);
  if (argc >= 2) {
    int id;
    if (Tcl_GetInt(interp, argv[1], &id) != TCL_OK)
      return TCL_ERROR;
    callback2(*handler_code,Val_int(id),copy_string_list(argc - 2,&argv[2]));
    /* Never fails (Caml would have raised an exception) */
    /* but result may have been set by callback */
    return TCL_OK;
  }
  else
    return TCL_ERROR;
}

/* Callbacks are always of type _ -> unit, to simplify storage
 * But a callback can nevertheless return something (to Tcl) by
 * using the following. TCL_VOLATILE ensures that Tcl will make
 * a copy of the string
 */
value camltk_return (v) /* ML */
     value v;
{
  CheckInit();

  Tcl_SetResult(cltclinterp, String_val(v), TCL_VOLATILE);
  return Val_unit;
}

/* Note: raise_with_string WILL copy the error message */
void tk_error(errmsg)
     char *errmsg;
{
  raise_with_string(*tkerror_exn, errmsg);
}


/* The initialisation of the C global variables pointing to Caml values
   must be made accessible from Caml, so that we are sure that it *always*
   takes place during loading of the protocol module
 */

value camltk_init(v) /* ML */
     value v;
{
  /* Initialize the Caml pointers */
  if (tkerror_exn == NULL)
    tkerror_exn = caml_named_value("tkerror");
  if (handler_code == NULL)
    handler_code = caml_named_value("camlcb");
  return Val_unit;
}
