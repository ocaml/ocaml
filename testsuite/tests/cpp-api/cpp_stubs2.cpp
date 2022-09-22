extern "C" {

#include "all-includes.h"

#include <stdio.h>

value test_cpp2(value v)
{
  CAMLparam1(v);

  printf("In test C++ 2, v = %d\n", Int_val(v));
  fflush(stdout);

  CAMLreturn (Val_unit);
}

}
