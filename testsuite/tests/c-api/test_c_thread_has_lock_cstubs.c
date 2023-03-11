#include "caml/mlvalues.h"
#include "caml/domain_state.h"
#include "caml/signals.h"

value with_lock(value unit)
{
  return Val_bool(Caml_state_opt != NULL);
}

value without_lock(value unit)
{
  int res;
  caml_enter_blocking_section();
  res = (Caml_state_opt != NULL);
  caml_leave_blocking_section();
  return Val_bool(res);
}
