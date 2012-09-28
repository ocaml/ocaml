/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: kill.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"
#include <signal.h>
#include <signals.h>

CAMLprim value unix_kill(value pid, value signal)
{
  int sig;
  sig = convert_signal_number(Int_val(signal));
  if (kill(Int_val(pid), sig) == -1)
    uerror("kill", Nothing);
  return Val_unit;
}
