/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/misc.h"
#include "caml/fail.h"

CAMLprim value caml_final_register (value unit)
{
  caml_failwith("finalisers unimplemented");
}

CAMLprim value caml_final_release (value unit)
{
  caml_failwith("finalisers unimplemented");
}

CAMLprim value caml_final_register_called_without_value (value f, value v)
{
  caml_failwith("finalisers unimplemented");
}
