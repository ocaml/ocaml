#include "caml/alloc.h"

value alloc_stub(value v) {
  return caml_alloc(1, 0);
}
