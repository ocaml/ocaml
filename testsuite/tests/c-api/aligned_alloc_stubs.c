#include <stdio.h>
#include <stdlib.h>
#include <caml/alloc.h>

CAMLprim value caml_atomic_is_aligned(value val)
{
  if ((uintptr_t)Hp_val(val) % Cache_line_bsize == 0) {
    return Val_true;
  } else {
    return Val_false;
  }
}
