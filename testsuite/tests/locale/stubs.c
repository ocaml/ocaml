#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <locale.h>

value ml_setlocale(value v_locale)
{
  CAMLparam1(v_locale);
  CAMLlocal2(str, ret);
  char *res = setlocale(LC_ALL,String_val(v_locale));
  if (res) {
    str = caml_copy_string(res);
    ret = caml_alloc_some(str);
  } else {
    ret = Val_none;
  }
  CAMLreturn(ret);
}
