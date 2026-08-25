#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include <string.h>
#include <stdio.h>

CAMLprim value stub_sub_right_copy(value vba, value vyoffset, value vylen)
{
  CAMLparam3(vba, vyoffset, vylen);
  CAMLlocal1(res);

  struct caml_ba_array *ba_src = Caml_ba_array_val(vba);
  long yoffset = Long_val(vyoffset);
  long ylen = Long_val(vylen);

  if (ba_src->num_dims != 2)
    caml_invalid_argument("stub_sub_right_copy(num_dims)");
  long len = ba_src->dim[1];

  if (ylen < 0 || yoffset < 0 || yoffset > len || ylen > len ||
      yoffset + ylen > len)
    caml_invalid_argument("stub_sub_right_copy(offset; len)");

  intnat dim[2] = {
    ba_src->dim[0],
    ylen
  };

  uintnat src_row_size = caml_ba_byte_size(ba_src) / dim[0];
  uintnat src_element_size = src_row_size / ba_src->dim[1];
  const char* src = ba_src->data;

  res = caml_ba_alloc(ba_src->flags, sizeof(dim)/sizeof(*dim), NULL, dim);
  /* can't use ba_src anymore, may have moved */
  ba_src = NULL;

  struct caml_ba_array *ba_dst = Caml_ba_array_val(res);
  uintnat dst_row_size = caml_ba_byte_size(ba_dst) / dim[0];

  if (dst_row_size > src_row_size)
    caml_invalid_argument("stub_sub_right_copy(offset; len; row_size)");

  uintnat row_offset_byte = yoffset * src_element_size;

  src += row_offset_byte;
  char* dst = ba_dst->data;
  for(unsigned i = 0; i < dim[0]; i++) {
    memcpy(dst, src, dst_row_size);
    src += src_row_size;
    dst += dst_row_size;
  }

  CAMLreturn(res);
}
