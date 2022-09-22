#define CAML_INTERNALS
#include "all-includes.h"

extern "C" {
#include <stdio.h>

value test_cpp3(value v)
{
  CAMLparam1(v);

  printf("In test C++ 3, v = %d\n", Int_val(v));
  fflush(stdout);

  CAMLreturn (Val_unit);
}

}
