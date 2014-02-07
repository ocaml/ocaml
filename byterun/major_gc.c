#include "mlvalues.h"
#include "memory.h"
#include "fail.h"

void caml_init_major_gc () {
}

intnat caml_major_collection_slice (intnat work) {

  return 100;
}
void caml_finish_major_cycle () {
}

