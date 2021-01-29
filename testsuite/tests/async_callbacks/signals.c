#include <caml/signals.h>

value process_uninterrupting (value unit)
{
  caml_mask_kind oldmask = caml_mask(CAML_MASK_UNINTERRUPTIBLE);
  caml_process_pending_actions();
  caml_unmask(oldmask);
  return Val_unit;
}

value blocking_section (value unit)
{
  caml_enter_blocking_section();
  caml_leave_blocking_section();
  return Val_unit;
}
