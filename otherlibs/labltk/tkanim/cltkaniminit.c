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
#include <tk.h>
#include <mlvalues.h>
#include "camltk.h"

extern int Tkanim_Init(Tcl_Interp *);

CAMLprim value tkanim_init (rien) /* ML */
     value rien;
{
  if (Tkanim_Init(cltclinterp) != TCL_OK)
    tk_error ("Can't initialize TkAnim");
  return Val_unit;
}
