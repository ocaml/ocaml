/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 File contributed by Lionel Fourquaux                */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include "u8tou16.h"

CAMLprim value unix_link(value v_path1, value v_path2)
{
  caml_unix_check_path(v_path1, "link");
  caml_unix_check_path(v_path2, "link");
  CRT_STR path1 = Crt_str_val(v_path1);
  CRT_STR path2 = Crt_str_val(v_path2);
  int ret = WINAPI_(CreateHardLink)(path1, path2, NULL);
  Crt_str_free(path1);
  Crt_str_free(path2);
  if (0 != ret)
  {
    win32_maperr(GetLastError());
    uerror("link", v_path2);
  }
  return Val_unit;
}
