#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

/* Functions callable directly from C */

int fib(int n)
{
  value * fib_closure = caml_named_value("fib");
  return Int_val(callback(*fib_closure, Val_int(n)));
}

char * format_result(int n)
{
  value * format_result_closure = caml_named_value("format_result");
  return strdup(String_val(callback(*format_result_closure, Val_int(n))));
}
