#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/fiber.h"

value cont_clone_stub(value k)
{
  return caml_continuation_clone(k);
}
