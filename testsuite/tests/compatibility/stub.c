#include <stdio.h>

#include <caml/minor_gc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/address_class.h>
/* see PR#8892 */
typedef char * addr;

CAMLprim value retrieve_young_limit(value v)
{
  CAMLparam1(v);
  printf("v is%s young\n", (Is_young(v) ? "" : " not"));
#ifdef CAML_NAME_SPACE
  CAMLreturn(caml_copy_nativeint((intnat)caml_young_limit));
#else
  CAMLreturn(copy_nativeint((intnat)young_limit));
#endif
}
