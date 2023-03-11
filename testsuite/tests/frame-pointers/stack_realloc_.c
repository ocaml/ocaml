#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/callback.h"

value c_fun(void)
{
  const value *callback = caml_named_value("callback");

  value v = caml_callback(*callback, Val_unit);

  if (Is_exception_result(v))
    return Val_long(-1);

  return Val_long(Long_val(v) + 1);
}
