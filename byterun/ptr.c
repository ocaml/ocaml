/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Jeremie Dimino, Jane Street Group, LLC                */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "get_set.h"

#define Ptr_val(v) (unsigned char*)Nativeint_val(v)

CAMLprim value caml_load_int8(value ptr)
{
  return Val_int(Byte_u(Ptr_val(ptr), 0));
}

CAMLprim value caml_load_int16(value ptr)
{
  return mem_get16(Ptr_val(ptr));
}

CAMLprim value caml_load_int32(value ptr)
{
  return mem_get32(Ptr_val(ptr));
}

CAMLprim value caml_load_int64(value ptr)
{
  return mem_get64(Ptr_val(ptr));
}

CAMLprim value caml_store_int8(value ptr, value newval)
{
  Byte_u(Ptr_val(ptr), 0) = Int_val(newval);
  return Val_unit;
}

CAMLprim value caml_store_int16(value ptr, value newval)
{
  mem_set16(Ptr_val(ptr), newval);
  return Val_unit;
}

CAMLprim value caml_store_int32(value ptr, value newval)
{
  mem_set32(Ptr_val(ptr), newval);
  return Val_unit;
}

CAMLprim value caml_store_int64(value ptr, value newval)
{
  mem_set64(Ptr_val(ptr), newval);
  return Val_unit;
}
