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

/* $Id: */

#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "camltk.h"

#if (TCL_MAJOR_VERSION > 8 || \
    (TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION >= 1)) /* 8.1 */
# define UTFCONVERSION
#endif
#undef UTFCONVERSION

/* This variable auto_utf_conversion controls the automatic conversion 
   between the coding specified by LANG and UTF. */
int auto_utf_conversion = 0;

value get_auto_utf_conversion( value u )
{
  return Val_int(auto_utf_conversion);
}

value set_auto_utf_conversion( value b )
{
  if (Int_val(b)) {
    auto_utf_conversion = 1;
  } else {
    auto_utf_conversion = 0;
  }
  return Val_unit;
}

#ifdef UTFCONVERSION

char *external_to_utf( char *str ){
  char *res;
  Tcl_DString utf;
  int length;

  Tcl_ExternalToUtfDString(NULL, str, strlen(str), &utf);
  length = Tcl_DStringLength(&utf);
  res = stat_alloc(length + 1);
  memmove( res, Tcl_DStringValue(&utf), length+1);
  Tcl_DStringFree(&utf);

  return res;
}

char *utf_to_external( char *str ){
  char *res;
  Tcl_DString ext;
  int length;

  Tcl_UtfToExternalDString(NULL, str, strlen(str), &ext);
  length = Tcl_DStringLength(&ext);
  res = stat_alloc(length + 1);
  memmove( res, Tcl_DStringValue(&ext), length+1);
  Tcl_DStringFree(&ext);

  return res;
}

char *caml_string_to_tcl( value s )
{
  CAMLparam1(s);
  char *str;
  char *res;
 
  str = string_to_c( s );
  res = external_to_utf( str );
  stat_free(str);
  return res;
}

value tcl_string_to_caml( char *s )
{
  CAMLparam0();
  CAMLlocal1(res);
  char *str;

  str = utf_to_external( s );
  res = copy_string(str);
  stat_free(str);
  return res;
}

#else

char *caml_string_to_tcl(value s){ return string_to_c(s); }
value tcl_string_to_caml(char *s){ return copy_string(s); }

#endif
