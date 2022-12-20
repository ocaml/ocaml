/* SPDX-License-Identifier: MIT */
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/gc.h>

#include "tagged_out_of_heap.h"

typedef value ref;

ref generational_ref_create(value v)
{
  value b = alloc_tagged_block();
  *(Block_data(b)) = v;
  caml_register_generational_global_root(Block_data(b));
  return b;
}

value generational_ref_get(ref r)
{
  return *Block_data(r);
}

value generational_ref_delete(ref r)
{
  caml_remove_generational_global_root(Block_data(r));
  free_tagged_block(r);
  return Val_unit;
}
