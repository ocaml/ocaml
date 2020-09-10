#include <caml/mlvalues.h>

value external_int_ref(value init)
{
  char* allocation;
  intnat* ref;
  allocation = malloc(sizeof(intnat) + 1);
  /* Align the allocation to 2 byte boundary -- maybe unnecessary */
  if((uintnat)allocation & 1) {
    ref = (intnat*) (allocation + 1);
  } else {
    ref = (intnat*) allocation;
  }
  *ref = init;
  return ((uintnat)ref) + 1;
}

value external_float_ref(value init)
{
  char* allocation;
  double* ref;
  allocation = malloc(sizeof(double) + 1);
  /* Align the allocation to 2 byte boundary -- maybe unnecessary */
  if((uintnat)allocation & 1) {
    ref = (double*) (allocation + 1);
  } else {
    ref = (double*) allocation;
  }
  *ref = Double_val(init);
  return ((uintnat)ref) + 1;
}
