/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <windows.h>

value unix_sleep(t)              /* ML */
     value t;
{
  enter_blocking_section();
  Sleep(Int_val(t) * 1000);
  leave_blocking_section();
  return Val_unit;
}
