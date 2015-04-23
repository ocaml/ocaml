#include <stdio.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value add_stub(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal1(r);
  int i;

  for (i = 0; i < 10; i++)
    r = caml_copy_double(Double_val(a) + Double_val(b));
  CAMLreturn(r);
}
