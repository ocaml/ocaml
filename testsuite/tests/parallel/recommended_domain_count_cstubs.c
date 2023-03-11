#define CAML_INTERNALS

#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/domain.h"

CAMLprim value
caml_get_max_domains(value nada)
{
  CAMLparam0();

  CAMLreturn(Val_long(Max_domains));
}
