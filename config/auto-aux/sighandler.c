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

/* $Id: sighandler.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <signal.h>

int main(void)
{
  SIGRETURN (*old)();
  old = signal(SIGQUIT, SIG_DFL);
  return 0;
}
