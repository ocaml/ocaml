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

#include <stdio.h>
#include <mlvalues.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

CAMLprim value unix_rename(value path1, value path2)
{
  CAMLparam2(path1, path2);
  char * p1;
  char * p2;
  int ret;
  p1 = caml_strdup(String_val(path1));
  p2 = caml_strdup(String_val(path2));
  caml_enter_blocking_section();
  ret = rename(p1, p2);
  caml_leave_blocking_section();
  caml_stat_free(p2);
  caml_stat_free(p1);
  if (ret == -1)
    uerror("rename", path1);
  CAMLreturn(Val_unit);
}
