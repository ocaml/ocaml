#include "caml/mlvalues.h"
#include "caml/signals.h"

value with_lock(value unit)
{
  return Val_bool(caml_thread_has_lock());
}

value without_lock(value unit)
{
  int res;
  caml_enter_blocking_section();
  res = caml_thread_has_lock();
  caml_leave_blocking_section();
  return Val_bool(res);
}
