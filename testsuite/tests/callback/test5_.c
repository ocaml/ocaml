#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

value caml_to_c_native (value a1, value a2, value a3, value a4, value a5,
                        value a6, value a7, value a8, value a9, value a10,
                        value a11)
{
  CAMLparam0 ();
  long l;

  printf ("[C] Enter caml_to_c\n");

  static const value* c_to_caml_closure = NULL;
  if (!c_to_caml_closure)
    c_to_caml_closure = caml_named_value("c_to_caml");

  l = Long_val (a1) + Long_val (a2) + Long_val (a3) + Long_val (a4)
    + Long_val (a5) + Long_val (a6) + Long_val (a7) + Long_val (a8)
    + Long_val (a9) + Long_val (a10) + Long_val (a11);

  printf ("[C] Call c_to_caml\n");
  fflush(stdout);
  caml_callback(*c_to_caml_closure, Val_long(l));
  printf ("[C] Return from c_to_caml\n");

  printf ("[C] Leave caml_to_c\n");
  fflush(stdout);
  CAMLreturn (Val_unit);
}

value caml_to_c_bytecode (value * argv, int argn) {
  return caml_to_c_native (argv[0], argv[1], argv[2], argv[3], argv[4],
                           argv[5], argv[6], argv[7], argv[8], argv[9],
                           argv[10]);
}
