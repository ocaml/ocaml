#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>

intnat test(intnat b,intnat c,intnat d) {
  return(b+c+d);
}

intnat tests(const char * str) {
  intnat r = 0;
  //printf("===========> %s\n", str);
  while (*str) r += *str++;
  return(r);
}

intnat testf(const float * str, intnat size) {
  double r = 0;
  for (int i=0; i < size; i++) r += (double) str[i];
  return(r);
}
