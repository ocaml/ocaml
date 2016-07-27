#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value caml_malloc(value size)
{
  return caml_copy_nativeint((intnat)malloc(Long_val(size)));
}
