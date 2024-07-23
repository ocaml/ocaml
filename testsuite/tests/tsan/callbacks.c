#include <stdio.h>
#include <time.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>

value print_and_call_ocaml_h(value unit)
{
  (void)unit;

  fprintf(stderr, "Hello from print_and_call_ocaml_h\n");
  caml_callback(*caml_named_value("ocaml_h"), Val_unit);
  fprintf(stderr, "Leaving print_and_call_ocaml_h\n");
  return Val_unit;
}

value print_and_raise(value unit)
{
  (void)unit;

  fprintf(stderr, "Hello from print_and_raise\n");
  caml_failwith("test");
  return Val_unit; /* Unreachable */
}
