#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/signals.h>

value request_minor_gcs(value u) {
  caml_alloc(1,0);
  caml_request_minor_gc();
  caml_alloc(1,0);
  caml_request_minor_gc();
  return Val_unit;
}
