#include <string.h>
#include "caml/mlvalues.h"
#include "caml/gc.h"
#include "caml/memory.h"

static int colors[4] = { Caml_white, Caml_gray, Caml_blue, Caml_black };

value make_block(value header_size, value color, value size)
{
  intnat sz = Nativeint_val(size);
  value * p = caml_stat_alloc((1 + sz) * sizeof(value));
  p[0] = Make_header(Nativeint_val(header_size), 0, colors[Int_val(color)]);
  memset(p + 1, 0x80, sz * sizeof(value));
  return (value) (p + 1);
}

value make_raw_pointer (value v)
{
  return (value) Nativeint_val(v);
}
