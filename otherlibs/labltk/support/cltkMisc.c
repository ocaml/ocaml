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
#include "camltk.h"

/* Parsing results */
CAMLprim value camltk_splitlist (value v)
{
  int argc;
  char **argv;
  int result;
  char *utf;

  CheckInit();

  utf = caml_string_to_tcl(v);
  /* argv is allocated by Tcl, to be freed by us */
  result = Tcl_SplitList(cltclinterp,utf,&argc,(const char ***)&argv);
  switch(result) {
  case TCL_OK:
   { value res = copy_string_list(argc,argv);
     Tcl_Free((char *)argv);    /* only one large block was allocated */
     /* argv points into utf: utf must be freed after argv are freed */
     stat_free( utf );
     return res;
   }
  case TCL_ERROR:
  default:
    stat_free( utf );
    tk_error(Tcl_GetStringResult(cltclinterp));
  }
}

/* Copy an OCaml string to the C heap. Should deallocate with stat_free */
char *string_to_c(value s)
{
  int l = string_length(s);
  char *res = stat_alloc(l + 1);
  memmove (res, String_val (s), l);
  res[l] = '\0';
  return res;
}
