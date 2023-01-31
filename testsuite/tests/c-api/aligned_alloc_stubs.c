#include <caml/mlvalues.h>
#include <caml/gc.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <inttypes.h>

mlsize_t alignment = 128;

value caml_is_aligned(value v) {
  long orig_ptr = v - 24 /* adjust for large block */; 
  long misalignment = orig_ptr % alignment;

  // printf("\n------ ptr: %p, misaligned by %lu", (void*)orig_ptr, misalignment);

  if (misalignment == 0) {
      return Val_true;
  } else {
      return Val_false;
  }
}

value caml_make_aligned(value val) 
{
  value var = caml_alloc_shr_aligned(8, 0, alignment);
  caml_initialize(&Field(var, 0), val);

  caml_is_aligned(var);
  return var;
}