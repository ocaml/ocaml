#include "../byterun/mlvalues.h"

value manyargs(a,b,c,d,e,f,g,h,i,j,k)
     value a,b,c,d,e,f,g,h,i,j,k;
{
  printf("a = %d\n", Int_val(a));
  printf("b = %d\n", Int_val(b));
  printf("c = %d\n", Int_val(c));
  printf("d = %d\n", Int_val(d));
  printf("e = %d\n", Int_val(e));
  printf("f = %d\n", Int_val(f));
  printf("g = %d\n", Int_val(g));
  printf("h = %d\n", Int_val(h));
  printf("i = %d\n", Int_val(i));
  printf("j = %d\n", Int_val(j));
  printf("k = %d\n", Int_val(k));
  return Val_unit;
}

value manyargs_argv(argv, argc)
     value * argv;
     int argc;
{
  return manyargs(argv[0], argv[1], argv[2], argv[3], argv[4],
                  argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}
