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
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_GETTIMEOFDAY

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_gettimeofday(value unit)
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) uerror("gettimeofday", Nothing);
  return caml_copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

CAMLprim value unix_gettimeofday_int(value unit)
{
  value res;
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) uerror("gettimeofday_int", Nothing);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = caml_copy_int64(tp.tv_sec);
  Field(res, 1) = caml_copy_int32(tp.tv_usec);
  return res;
}

#else

CAMLprim value unix_gettimeofday(value unit)
{ caml_invalid_argument("gettimeofday not implemented"); }

CAMLprim value unix_gettimeofday_int(value unit)
{ caml_invalid_argument("gettimeofday_int not implemented"); }

#endif
