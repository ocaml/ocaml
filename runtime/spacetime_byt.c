/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/fail.h"
#include "caml/mlvalues.h"

int caml_ensure_spacetime_dot_o_is_included = 42;

CAMLprim caml_value caml_spacetime_only_works_for_native_code(caml_value foo, ...)
{
  caml_failwith("Spacetime profiling only works for native code");
}

uintnat caml_spacetime_my_profinfo (void)
{
  return 0;
}

CAMLprim caml_value caml_spacetime_enabled (caml_value v_unit)
{
  return Val_false;  /* running in bytecode */
}

CAMLprim caml_value caml_register_channel_for_spacetime (caml_value v_channel)
{
  return Val_unit;
}
