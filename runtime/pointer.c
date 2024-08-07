/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Demi Marie Obenour                            */
/*                                                                        */
/*                  Copyright 2024 Demi Marie Obenour                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Raw pointer operations.  Only built for bytecode, as the native code
 * compiler generates inline machine code for these operations. */

#include <stdint.h>
#include <string.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/alloc.h"

CAMLprim value caml_ptr_load8(value ptr, value offset)
{
    return Val_int(*(const uint8_t *)Nativeint_val(ptr) + Int_val(offset));
}

CAMLprim value caml_ptr_load16(value ptr, value offset)
{
    uint16_t v;
    memcpy(&v, (const char *)Nativeint_val(ptr) + Int_val(offset), sizeof v);
    return Val_int(v);
}

CAMLprim value caml_ptr_load32(value ptr, value offset)
{
    uint32_t v;
    memcpy(&v, (const char *)Nativeint_val(ptr) + Int_val(offset), sizeof v);
    return caml_copy_int32(v);
}

CAMLprim value caml_ptr_load64(value ptr, value offset)
{
    uint64_t v;
    memcpy(&v, (const char *)Nativeint_val(ptr) + Int_val(offset), sizeof v);
    return caml_copy_int64(v);
}

CAMLprim value caml_ptr_set8(value ptr, value offset, value v)
{
    *((char *)Nativeint_val(ptr) + Int_val(offset)) = Int_val(v);
    return Val_unit;
}

CAMLprim value caml_ptr_set16(value ptr, value offset, value v)
{
    uint16_t untagged = Int_val(v);
    memcpy((char *)Nativeint_val(ptr) + Int_val(offset),
           &untagged, sizeof untagged);
    return Val_unit;
}

CAMLprim value caml_ptr_set32(value ptr, value offset, value v)
{
    uint32_t unboxed = Int32_val(v);
    memcpy((char *)Nativeint_val(ptr) + Int_val(offset),
           &unboxed, sizeof unboxed);
    return Val_unit;
}

CAMLprim value caml_ptr_set64(value ptr, value offset, value v)
{
    uint64_t unboxed = Int64_val(v);
    memcpy((char *)Nativeint_val(ptr) + Int_val(offset),
           &unboxed, sizeof unboxed);
    return Val_unit;
}
