#include "misc.h"
#include "fail.h"

/* Put (f,v) in the recent set. */
CAMLprim value caml_final_register (value f, value v)
{
  caml_failwith("finalisers unimplemented");
}

CAMLprim value caml_final_release (value unit)
{
  caml_failwith("finalisers unimplemented");
}
