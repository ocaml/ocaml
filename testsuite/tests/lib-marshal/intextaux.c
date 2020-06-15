/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/intext.h>
#include <caml/custom.h>

#define CAML_INTERNALS

#define BLOCK_SIZE 512
static char marshal_block[BLOCK_SIZE];

value marshal_to_block(value vlen, value v, value vflags)
{
  CAMLassert(Long_val(vlen) <= BLOCK_SIZE);
  caml_output_value_to_block(v, vflags, marshal_block, Long_val(vlen));
  return Val_unit;
}

value marshal_from_block(value vlen)
{
  CAMLassert(Long_val(vlen) <= BLOCK_SIZE);
  return caml_input_value_from_block(marshal_block, Long_val(vlen));
}

static void bad_serialize(value v, uintnat* sz_32, uintnat* sz_64)
{
  caml_serialize_int_4(42);
  *sz_32 = *sz_64 = 100;
}

static uintnat bad_deserialize(void* dst)
{
  return 10;
}

static struct custom_operations buggy_ops = {
  "foo",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  bad_serialize,
  bad_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value init_buggy_custom_ops()
{
  caml_register_custom_operations(&buggy_ops);
  return Val_unit;
}

value value_with_buggy_serialiser()
{
  return caml_alloc_custom(&buggy_ops, 20, 0, 1);
}
