#include <math.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value floor_stub(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_double(floor(Double_val(v))));
}

value ceil_stub(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_double(ceil(Double_val(v))));
}
