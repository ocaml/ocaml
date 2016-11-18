#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

value caml_to_c (value unit) {
  CAMLparam1 (unit);
  printf ("[C] Enter caml_to_c\n");

  static value c_to_caml_closure;
  static int found = 0;

  if (!found)
    c_to_caml_closure = caml_get_named_value("c_to_caml", &found);

  printf ("[C] Call c_to_caml\n");
  caml_callback(c_to_caml_closure, Val_unit);
  printf ("[C] Return from c_to_caml\n");

  printf ("[C] Leave caml_to_c\n");
  CAMLreturn (Val_unit);
}
