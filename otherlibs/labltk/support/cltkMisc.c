#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "camltk.h"

/* Parsing results */
value camltk_splitlist (v) /* ML */
     value v;
{
  int argc;
  char **argv;
  int result;

  CheckInit();

  /* argv is allocated by Tcl, to be freed by us */
  result = Tcl_SplitList(cltclinterp,String_val(v),&argc,&argv);
  switch(result) {
  case TCL_OK:
   { value res = copy_string_list(argc,argv);
     free((char *)argv);	/* only one large block was allocated */
     return res;
   }
  case TCL_ERROR:
  default:
    tk_error(cltclinterp->result);
  }
}

/* Copy a Caml string to the C heap. Should deallocate with stat_free */
char *string_to_c(s)
     value s;
{
  int l = string_length(s);
  char *res = stat_alloc(l + 1);
  bcopy(String_val(s),res,l);
  res[l] = '\0';
  return res;
}


