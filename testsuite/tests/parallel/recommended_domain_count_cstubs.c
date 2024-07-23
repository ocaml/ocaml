#define CAML_INTERNALS

#include <caml/domain.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/startup_aux.h>

CAMLprim value
caml_get_max_domains(value nada)
{
  CAMLparam0();

  CAMLreturn(Val_long(caml_params->max_domains));
}
