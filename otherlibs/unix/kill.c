/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <fail.h>
#include "unix.h"
#include <signal.h>

extern int posix_signals[];     /* defined in byterun/signals.c */

value unix_kill(pid, signal)     /* ML */
     value pid, signal;
{
  int sig;
  sig = Int_val(signal);
  if (sig < 0) {
    sig = posix_signals[-sig-1];
    if (sig == 0) invalid_argument("Unix.kill: unavailable signal");
  }
  if (kill(Int_val(pid), sig) == -1)
    uerror("kill", Nothing);
  return Val_unit;
}
