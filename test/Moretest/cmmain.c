/* Main program -- in C */

#include <stdlib.h>
#include <caml/callback.h>

extern int fib(int n);
extern char * format_result(int n);

int main(int argc, char ** argv)
{
  printf("Initializing Caml code...\n");
#ifdef NO_BYTECODE_FILE
  caml_startup(argv);
#else
  caml_main(argv);
#endif
  printf("Back in C code...\n");
  printf("Computing fib(20)...\n");
  printf("%s\n", format_result(fib(20)));
  return 0;
}
