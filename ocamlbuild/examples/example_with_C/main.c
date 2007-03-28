                                                            /* -*- C -*- */
#include <stdio.h>
#include <caml/callback.h>
extern int fib(int);
int main(int argc, char** argv)
{
  caml_startup(argv);
  printf("fib(12) = %d\n", fib(12));
  return 0;
}
