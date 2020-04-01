#include <caml/mlvalues.h>
#include <locale.h>

value ml_setlocale(value v_locale)
{
  setlocale(LC_ALL,String_val(v_locale));
  return Val_unit;
}
