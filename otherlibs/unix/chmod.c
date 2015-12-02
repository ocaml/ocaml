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

#include <sys/types.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "u8tou16.h"

CAMLprim value unix_chmod(value path, value perm)
{
  CAMLparam2(path, perm);
  CRT_STR p;
  int ret;
  caml_unix_check_path(path, "chmod");
  p = Crt_str_val(path);
  caml_enter_blocking_section();
  ret = CRT_(chmod)(p, Int_val(perm));
  caml_leave_blocking_section();
  Crt_str_free(p);
  if (ret == -1) uerror("chmod", path);
  CAMLreturn(Val_unit);
}
