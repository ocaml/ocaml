#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <stdio.h>

value factorial(value n){
  CAMLparam1(n);
  CAMLlocal1(s);

  static char buf[256];
  int x = 1;
  int i;
  int m = Int_val(n);
  for (i = 1; i <= m; i++) x *= i;
  sprintf(buf,"%i",x);
  s = copy_string(buf);
  CAMLreturn (s);
}
