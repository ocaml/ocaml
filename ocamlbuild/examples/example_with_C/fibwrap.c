                                                            /* -*- C -*- */
#include <caml/mlvalues.h>
#include <caml/callback.h>
int fib(int n)
{
  return Int_val(caml_callback(*caml_named_value("fib"), Val_int(n)));
}
