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
#include <fail.h>
#include "unixsupport.h"
#include <signal.h>
#include <signals.h>

value unix_kill(value pid, value signal)     /* ML */
{
  int sig;
  sig = convert_signal_number(Int_val(signal));
  if (kill(Int_val(pid), sig) == -1)
    uerror("kill", Nothing);
  return Val_unit;
}
