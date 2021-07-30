#define CAML_NAME_SPACE
#define CAML_INTERNALS

#include <caml/domain_state.h>
#include <caml/signals.h>

CAMLprim value request_minor_gc(value v) {
  Caml_state->requested_minor_gc = 1;
  Caml_state->requested_major_slice = 1;
  caml_something_to_do = 1;
  Caml_state->young_limit = Caml_state->young_alloc_end;

  return Val_unit;
}

CAMLprim value minor_gcs(value v) {
  return Val_long(Caml_state->stat_minor_collections);
}
