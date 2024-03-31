/**************************************************************************/
/*        ^o3                                                             */
/* ~/\_/\_|)                       OCaml                                  */
/* |/=_=\|                                                                */
/* "     "                                                                */
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
#include "caml/unixsupport.h"

CAMLprim value caml_unix_setuid(value uid)
{
  if (setuid(Int_val(uid)) == -1) caml_uerror("setuid", Nothing);
  return Val_unit;
}
