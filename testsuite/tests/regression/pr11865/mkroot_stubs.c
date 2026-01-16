#include <stdlib.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/fail.h>

struct box {
  value nongen_root;
  value gen_root;
};

static void box_finalize(value v)
{
  struct box** r = Data_custom_val(v);
  caml_remove_global_root(&(*r)->nongen_root);
  caml_remove_generational_global_root(&(*r)->gen_root);
  free(*r);
}

static struct custom_operations box_ops = {
  "box",
  box_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value box_make(value v)
{
  CAMLparam1(v);
  CAMLlocal1(ret);
  struct box* r = malloc(sizeof(struct box));
  if (!r) caml_failwith("out of memory?");
  ret = caml_alloc_custom(&box_ops, sizeof(struct box*), 0, 1);
  *((struct box**)Data_custom_val(ret)) = r;
  r->nongen_root = v;
  caml_register_global_root(&r->nongen_root);
  r->gen_root = v;
  caml_register_generational_global_root(&r->gen_root);
  CAMLreturn(ret);
}

value box_deref(value v)
{
  struct box** r = Data_custom_val(v);
  if ((*r)->nongen_root != (*r)->gen_root) caml_failwith("root mismatch");
  return (*r)->gen_root;
}
