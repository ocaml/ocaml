#define CAML_INTERNALS
#include "caml/domain.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/alloc.h"
#include <stdio.h>

CAMLprim value thread_getname()
{
  CAMLparam0();
  CAMLlocal1(result);
  char tmp[MAX_THREAD_NAME_LENGTH];
  caml_thread_getname(tmp);
  result = caml_copy_string(tmp);
  CAMLreturn(result);
}
