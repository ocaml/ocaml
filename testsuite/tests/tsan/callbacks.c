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

value print_and_raise_many_args(value arg1, value arg2, value arg3, value arg4,
                                value arg5, value arg6, value arg7, value arg8,
                                value arg9)
{
  (void)arg1;
  (void)arg2;
  (void)arg3;
  (void)arg4;
  (void)arg5;
  (void)arg6;
  (void)arg7;
  (void)arg8;
  (void)arg9;

  fprintf(stderr, "Hello from print_and_raise_many_args\n");
  caml_raise_not_found();
  return Val_unit; /* Unreachable */
}
