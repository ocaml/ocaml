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

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

CAMLprim value unix_pipe(value unit)
{
  int fd[2];
  value res;
  if (pipe(fd) == -1) uerror("pipe", Nothing);
  res = alloc_small(2, 0);
  Init_field(res, 0, Val_int(fd[0]));
  Init_field(res, 1, Val_int(fd[1]));
  return res;
}
