#include <caml/mlvalues.h>
#include <caml/callback.h>

value caml_to_c (value f) {
  return caml_callback(f, Val_unit);
}
