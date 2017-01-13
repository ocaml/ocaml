#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

value caml_to_c (value unit) {
  CAMLparam1 (unit);
  printf ("[C] Enter caml_to_c\n");

  static caml_root c_to_caml_closure = NULL;

  if (!c_to_caml_closure)
    c_to_caml_closure = caml_named_root("c_to_caml");

  printf ("[C] Call c_to_caml\n");
  caml_callback(caml_read_root(c_to_caml_closure), Val_unit);
  printf ("[C] Return from c_to_caml\n");

  printf ("[C] Leave caml_to_c\n");
  CAMLreturn (Val_unit);
}
