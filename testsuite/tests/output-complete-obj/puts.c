#include <caml/mlvalues.h>
#include <stdio.h>

value caml_puts(value s)
{
  puts(String_val(s));
  return Val_unit;
}
