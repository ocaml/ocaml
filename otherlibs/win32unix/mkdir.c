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
#include "unixsupport.h"
#include "u8tou16.h"

CAMLprim value unix_mkdir(value v_path, value v_perm)
{
  caml_unix_check_path(v_path, "mkdir");
  CRT_STR path = Crt_str_val(v_path);
  int ret = CRT_(mkdir)(path);
  Crt_str_free(path);
  if (ret == -1) uerror("mkdir", v_path);
  return Val_unit;
}
