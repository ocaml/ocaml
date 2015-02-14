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
#ifdef UTF16
#include "u8tou16.h"
#endif

CAMLprim value win_system(cmd)
     value cmd;
{
  int ret;
  value st;
  char *buf;
  intnat len;
#ifdef UTF16
  char * temp;
  WCHAR * wtemp;
  temp=String_val(cmd);
  wtemp = to_utf16(temp);
#endif

  caml_unix_check_path(cmd, "system");
  len = caml_string_length (cmd);
  buf = caml_stat_alloc (len + 1);
  memmove (buf, String_val (cmd), len + 1);
  enter_blocking_section();
  _flushall();
#ifdef UTF16
  ret = _wsystem(wtemp);;
  free(wtemp);
#else
  ret = system(buf);
#endif
  leave_blocking_section();
  caml_stat_free(buf);
  if (ret == -1) uerror("system", Nothing);
  st = alloc_small(1, 0); /* Tag 0: Exited */
  Field(st, 0) = Val_int(ret);
  return st;
}
