#include "all-includes.h"

extern "C" {
#include <stdio.h>

value test_cpp1(value v)
{
  CAMLparam1(v);

  printf("In test C++ 1, v = %d\n", Int_val(v));
  fflush(stdout);

  CAMLreturn (Val_unit);
}

}
