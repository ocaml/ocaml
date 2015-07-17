#include <cpuid.h>
#include <caml/alloc.h>
#include <caml/memory.h>

value ocaml___cpuid_stub(value level)
{
  CAMLparam1(level);
  CAMLlocal1(t);
  int a, b, c, d;
  __cpuid(Int_val(level), a, b, c, d);
  t = caml_alloc_tuple(4);
  Store_field(t, 0, Val_int(a));
  Store_field(t, 1, Val_int(b));
  Store_field(t, 2, Val_int(c));
  Store_field(t, 3, Val_int(d));
  CAMLreturn(t);
}
