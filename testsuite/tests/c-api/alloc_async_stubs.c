#include <stdio.h>
#include <stdlib.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#define CAML_INTERNALS
#include <caml/gc_ctrl.h>


void print_status(const char *str, int n)
{
  printf("%s: %d\n", str, n);
  fflush(stdout);
}

value print_status_caml(value str, value n)
{
  print_status(String_val(str), Int_val(n));
  return Val_unit;
}

const char* strs[] = { "foo", "bar", 0 };
value stub(value ref)
{
  CAMLparam1(ref);
  CAMLlocal2(x, y);
  char* s; intnat coll_before;

  print_status("C, before", Int_val(Field(ref, 0)));

  /* First, do enough major allocations to do a full major collection cycle */
  coll_before = caml_stat_major_collections;
  while (caml_stat_major_collections <= coll_before+1) {
    caml_alloc(10000, 0);
  }

  /* Now, call lots of allocation functions */

  /* Small allocations */
  caml_alloc(10, 0);
  x = caml_alloc_small(2, 0);
  Field(x, 0) = Val_unit;
  Field(x, 1) = Val_unit;
  caml_alloc_tuple(3);
  caml_alloc_float_array(10);
  caml_alloc_string(42);
  caml_alloc_initialized_string(10, "abcdeabcde");
  caml_copy_string("asoidjfa");
  caml_copy_string_array(strs);
  caml_copy_double(42.0);
  caml_copy_int32(100);
  caml_copy_int64(100);
  caml_alloc_array(caml_copy_string, strs);
  caml_alloc_sprintf("[%d]", 42);

  /* Large allocations */
  caml_alloc(1000, 0);
  caml_alloc_shr(1000, String_tag);
  caml_alloc_tuple(1000);
  caml_alloc_float_array(1000);
  caml_alloc_string(10000);
  s = calloc(10000, 1);
  caml_alloc_initialized_string(10000, s);
  free(s);


  print_status("C, after", Int_val(Field(ref, 0)));
  CAMLreturn (Val_unit);
}
