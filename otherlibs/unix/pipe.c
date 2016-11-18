/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"

CAMLprim value unix_pipe(value unit)
{
  int fd[2];
  value res;
  if (pipe(fd) == -1) uerror("pipe", Nothing);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}
