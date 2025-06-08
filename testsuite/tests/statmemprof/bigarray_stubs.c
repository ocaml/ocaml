#include <stdlib.h>
#include <caml/bigarray.h>

static char buf[10000];
value static_bigstring(value unit)
{
  intnat dim[] = { sizeof(buf) };
  return caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
                       1, buf, dim);
}

value new_bigstring(value unit)
{
  intnat dim[] = { 5000 };
  return caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT,
                       1, NULL, dim);
}

value malloc_bigstring(value unit)
{
  intnat dim[] = { 5000 };
  return caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                       1, malloc(dim[0]), dim);
}
