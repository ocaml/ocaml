#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <stdio.h>

value stub1() {
  CAMLlocal1(x);
  printf("This is stub1!\n"); fflush(stdout);
  x = caml_copy_string("ABCDEF");
  return x;
}
