/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include <signals.h>
#include "unixsupport.h"
#include <process.h>
#include <stdio.h>

CAMLprim value win_system(cmd)
     value cmd;
{
  int ret;
  value st;

  enter_blocking_section();
  _flushall();
  ret = system(String_val(cmd));;
  leave_blocking_section();
  if (ret == -1) uerror("system", Nothing);
  st = alloc_small(1, 0); /* Tag 0: Exited */
  Field(st, 0) = Val_int(ret);
  return st;
}



