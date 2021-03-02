#include <stdio.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"

void fatal_error_hook_exit_3 (char *msg, va_list args) {
  fprintf(stderr, "Fatal error hook: ");
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");
  exit(42);
}


value install_fatal_error_hook (value unit) {
  caml_fatal_error_hook = fatal_error_hook_exit_3;
  return Val_unit;
}
