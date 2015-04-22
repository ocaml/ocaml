#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value add_int_stub(value a, value b)
{
  CAMLparam2(a, b);
  Store_field(b, 0, Field(b, 0) + a - 1);
  CAMLreturn(Val_unit);
}

value add_float_stub(value a, value b)
{
  CAMLparam2(a, b);
  Store_field(b, 0, caml_copy_double(Double_val(Field(b, 0)) + Double_val(a)));
  CAMLreturn(Val_unit);
}

value add_int64_stub(value a, value b)
{
  CAMLparam2(a, b);
  Store_field(b, 0, caml_copy_int64(Int64_val(Field(b, 0)) + Int64_val(a)));
  CAMLreturn(Val_unit);
}
