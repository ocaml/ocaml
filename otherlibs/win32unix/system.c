/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include <process.h>
#include <stdio.h>
#include "u8tou16.h"

CAMLprim value win_system(value cmd)
{
  int ret;
  value st;
  CRT_STR buf;

  caml_unix_check_path(cmd, "system");
  buf = Crt_str_val(cmd);
  caml_enter_blocking_section();
  _flushall();
  ret = CRT_(system)(buf);;
  caml_leave_blocking_section();
  Crt_str_free(buf);
  if (ret == -1) uerror("system", Nothing);
  st = alloc_small(1, 0); /* Tag 0: Exited */
  Field(st, 0) = Val_int(ret);
  return st;
}
