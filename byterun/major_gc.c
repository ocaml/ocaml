#include "mlvalues.h"
#include "memory.h"
#include "fail.h"


header_t atoms[256];

CAMLexport value caml_atom(tag_t tag) {
  return Val_hp(&atoms[tag]);
}

void caml_init_major_gc () {
  int i;
  for (i=0; i<256; i++) atoms[i] = Make_header(0, i, 0);
}

intnat caml_major_collection_slice (intnat work) {

  return 100;
}
void caml_finish_major_cycle () {
}

