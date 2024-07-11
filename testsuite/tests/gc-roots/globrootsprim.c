/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* For testing global root registration */

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/gc.h>
#include <caml/shared_heap.h>
#include <caml/callback.h>

struct block { value header; value v; };

#define Block_val(v) ((struct block*) &((value*) v)[-1])
#define Val_block(b) ((value) &((b)->v))

value gb_get(value vblock)
{
  return Block_val(vblock)->v;
}

value gb_classic_register(value v)
{
  struct block * b = caml_stat_alloc(sizeof(struct block));
  b->header = Make_header(1, 0, NOT_MARKABLE);
  b->v = v;
  caml_register_global_root(&(b->v));
  return Val_block(b);
}

value gb_classic_set(value vblock, value newval)
{
  Block_val(vblock)->v = newval;
  return Val_unit;
}

value gb_classic_remove(value vblock)
{
  caml_remove_global_root(&(Block_val(vblock)->v));
  return Val_unit;
}

value gb_generational_register(value v)
{
  struct block * b = caml_stat_alloc(sizeof(struct block));
  b->header = Make_header(1, 0, NOT_MARKABLE);
  b->v = v;
  caml_register_generational_global_root(&(b->v));
  return Val_block(b);
}

value gb_generational_set(value vblock, value newval)
{
  caml_modify_generational_global_root(&(Block_val(vblock)->v), newval);
  return Val_unit;
}

value gb_generational_remove(value vblock)
{
  caml_remove_generational_global_root(&(Block_val(vblock)->v));
  return Val_unit;
}

value root;

value gb_young2old(value _dummy) {
  root = caml_alloc_small(1, 0);
  caml_register_generational_global_root(&root);
  caml_modify_generational_global_root(&root, caml_alloc_shr(10, String_tag));
  Field(root, 0) = 0xFFFFFFFF;
  caml_remove_generational_global_root(&root);
  root += sizeof(value);
  return Val_unit;
}

value gb_static2young(value static_value, value full_major) {
  CAMLparam2 (static_value, full_major);
  CAMLlocal1(v);

  root = Val_unit;
  caml_register_generational_global_root(&root);

  /* Write a static value in the root. */
  caml_modify_generational_global_root(&root, static_value);

  /* Overwrite it with a young value. */
  v = caml_alloc_small(1, 0);
  Field(v, 0) = Val_long(0x42);
  caml_modify_generational_global_root(&root, v);

  /* Promote the young value */
  caml_callback(full_major, Val_unit);

  /* Fill the minor heap to make sure the old block is overwritten */
  for (int i = 0; i < 1000000; i++)
    caml_alloc_small(1, 0);

  v = Field(root, 0);
  caml_remove_generational_global_root(&root);

  CAMLreturn(v);
}
