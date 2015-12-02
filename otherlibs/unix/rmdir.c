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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "u8tou16.h"

CAMLprim value unix_rmdir(value path)
{
  CAMLparam1(path);
  CRT_STR p;
  int ret;
  caml_unix_check_path(path, "rmdir");
  p = Crt_str_val(path);
  caml_enter_blocking_section();
  ret = CRT_(rmdir)(p);
  caml_leave_blocking_section();
  Crt_str_val(p);
  if (ret == -1) uerror("rmdir", path);
  CAMLreturn(Val_unit);
}
