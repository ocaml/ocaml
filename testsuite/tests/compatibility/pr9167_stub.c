#define CAML_NAME_SPACE
#include <caml/custom.h>
#include <caml/mlvalues.h>

/* see PR#9167 */

CAMLprim value retrieve_compare_unordered(value unit)
{
  return Bool_val(caml_compare_unordered);
}
