#include<stdio.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/weak.h"

/* C version of ephetest.ml */

void is_true(const char* test, const char* s, int b) {
  if(b) printf("%s %s: OK\n", test, s);
  else printf("%s %s: FAIL\n", test, s);
}

void is_false(const char* test, const char* s, int b) {
  is_true(test, s, !b);
}

void is_data_value(const char* test, value eph, intnat v) {
  CAMLparam1(eph);
  CAMLlocal1(x);

  if(caml_ephemeron_get_data_copy(eph, &x))
    if(Long_val(Field(x, 0)) == v) printf("%s data set: OK\n", test);
    else printf("%s data set: FAIL(bad value %li)\n", test,
                (long int)Long_val(Field(x, 0)));
  else
    printf("%s data set: FAIL\n", test);

  CAMLreturn0;
}

void is_key_value(const char* test, value eph, intnat v) {
  CAMLparam1(eph);
  CAMLlocal1(x);

  if(caml_ephemeron_get_key_copy(eph, 0, &x))
    if(Long_val(Field(x, 0)) == v) printf("%s key set: OK\n", test);
    else printf("%s key set: FAIL(bad value %li)\n", test,
                (long int)Long_val(Field(x, 0)));
  else
    printf("%s key unset: FAIL\n", test);

  CAMLreturn0;
}

void is_key_unset(const char* test, value eph) {
  is_false(test, "key unset", caml_ephemeron_key_is_set(eph, 0));
}

void is_data_unset(const char* test, value eph) {
  is_false(test, "data unset", caml_ephemeron_data_is_set(eph));
}

extern value caml_gc_minor(value);
extern value caml_gc_full_major(value);

CAMLprim value test1(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal1(eph);
  value x;

  const char* test = "test1";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  caml_ephemeron_set_key(eph, 0, Field(ra, 0));
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(42);
  caml_ephemeron_set_data(eph, x);
  is_key_value(test, eph, 1);
  is_data_value(test, eph, 42);
  caml_gc_minor(Val_unit);
  is_key_value(test, eph, 1);
  is_data_value(test, eph, 42);
  caml_gc_full_major(Val_unit);
  is_key_value(test, eph, 1);
  is_data_value(test, eph, 42);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(12);
  caml_modify(&Field(ra, 0), x);
  caml_gc_full_major(Val_unit);
  is_key_unset(test, eph);
  is_data_unset(test, eph);

  CAMLreturn(Val_unit);
}

CAMLprim value test2(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal1(eph);
  value x;

  const char* test = "test2";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(125);
  caml_ephemeron_set_key(eph, 0, x);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(42);
  caml_ephemeron_set_data(eph, x);
  is_key_value(test, eph, 125);
  is_data_value(test, eph, 42);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(13);
  caml_modify(&Field(ra, 0), x);
  caml_gc_minor(Val_unit);
  is_key_unset(test, eph);
  is_data_unset(test, eph);

  CAMLreturn(Val_unit);
}

CAMLprim value test3(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal1(eph);
  value x;

  const char* test = "test3";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(125);
  caml_ephemeron_set_key(eph, 0, x);
  caml_ephemeron_set_data(eph, Field(ra, 0));
  is_key_value(test, eph, 125);
  is_data_value(test, eph, 13);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(14);
  caml_modify(&Field(ra, 0), x);
  caml_gc_minor(Val_unit);
  is_key_unset(test, eph);
  is_data_unset(test, eph);

  CAMLreturn(Val_unit);
}

CAMLprim value test4(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal2(eph, y);
  value x;

  const char* test = "test4";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  y = caml_alloc(1, 0);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = y;
  caml_modify(&Field(y, 0), Val_long(3));
  caml_modify(&Field(rb, 0), x);
  y = Val_unit;
  caml_ephemeron_set_key(eph, 0, Field(Field(rb, 0), 0));
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(43);
  caml_ephemeron_set_data(eph, x);
  is_key_value(test, eph, 3);
  is_data_value(test, eph, 43);
  caml_gc_minor(Val_unit);
  caml_gc_minor(Val_unit);
  is_key_value(test, eph, 3);
  is_data_value(test, eph, 43);

  CAMLreturn(Val_unit);
}

CAMLprim value test5(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal2(eph, y);
  value x;

  const char* test = "test5";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  y = caml_alloc(1, 0);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = y;
  caml_modify(&Field(y, 0), Val_long(3));
  caml_modify(&Field(rb, 0), x);
  y = Val_unit;
  caml_ephemeron_set_key(eph, 0, Field(Field(rb, 0), 0));
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(43);
  caml_ephemeron_set_data(eph, x);
  is_key_value(test, eph, 3);
  is_data_value(test, eph, 43);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(4);
  caml_modify(&Field(rb, 0), x);
  caml_gc_minor(Val_unit);
  caml_gc_minor(Val_unit);
  is_key_unset(test, eph);
  is_data_unset(test, eph);

  CAMLreturn(Val_unit);
}

CAMLprim value test6(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal2(eph, y);
  value x;

  const char* test = "test6";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  eph = caml_ephemeron_create(1);
  y = caml_alloc(1, 0);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = y;
  caml_modify(&Field(y, 0), Val_long(3));
  caml_modify(&Field(rb, 0), x);
  y = Val_unit;
  caml_ephemeron_set_key(eph, 0, Field(Field(rb, 0), 0));
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Field(Field(rb, 0), 0);
  caml_ephemeron_set_data(eph, x);
  caml_gc_minor(Val_unit);
  is_key_value(test, eph, 3);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(4);
  caml_modify(&Field(rb, 0), x);
  caml_gc_full_major(Val_unit);
  is_key_unset(test, eph);
  is_data_unset(test, eph);

  CAMLreturn(Val_unit);
}

CAMLprim value test7(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal4(eph, weak, y, rc);
  value x;

  const char* test = "test7";
  caml_gc_minor(Val_unit);
  caml_gc_full_major(Val_unit);
  x = caml_alloc_small(1, 0);
  Field(x, 0) = Val_long(42);
  caml_modify(&Field(ra, 0), x);
  weak = caml_weak_array_create(1);
  y = caml_ephemeron_create(1);
  eph = caml_alloc_small(1, 0);
  Field(eph, 0) = y;
  y = Val_unit;
  rc = caml_alloc_small(1, 0);
  Field(rc, 0) = Field(eph, 0);
  caml_weak_array_set(weak, 0, Field(rc, 0));
  caml_ephemeron_set_key(Field(eph, 0), 0, Field(ra, 0));
  caml_ephemeron_set_data(Field(eph, 0), Field(rc, 0));
  caml_gc_minor(Val_unit);
  is_true(test, "before", caml_weak_array_check(weak, 0));
  caml_modify(&Field(eph, 0), caml_ephemeron_create(1));
  caml_modify(&Field(rc, 0), Val_unit);
  caml_gc_full_major(Val_unit);
  caml_gc_full_major(Val_unit);
  caml_gc_full_major(Val_unit);
  is_false(test, "after", caml_weak_array_check(weak, 0));

  CAMLreturn(Val_unit);
}

CAMLprim value test8(value ra, value rb) {
  CAMLparam2(ra, rb);
  CAMLlocal3(x, y, z);

  const char* test = "test8";

  x = caml_ephemeron_create(15);
  z = caml_ephemeron_create(3);
  is_true(test, "eph length=15", caml_ephemeron_num_keys(x) == 15);
  is_true(test, "eph length=3", caml_ephemeron_num_keys(z) == 3);

  is_false(test, "eph get empty nonull", caml_ephemeron_get_key(x, 5, &y));
  is_false(test, "eph get copy empty nonnull", caml_ephemeron_get_key_copy(x, 5, &y));
  caml_ephemeron_set_key(x, 5, ra);
  is_true(test, "eph get nonull", caml_ephemeron_get_key(x, 5, &y));
  is_true(test, "eph get eq", y == ra);
  is_true(test, "eph get copy nonnull", caml_ephemeron_get_key_copy(x, 5, &y));
  is_true(test, "eph get copy eq", y != ra);
  caml_ephemeron_blit_key(x, 4, z, 0, 3);
  caml_ephemeron_unset_key(x, 5);
  is_false(test, "eph get unset nonull", caml_ephemeron_get_key(x, 5, &y));
  is_false(test, "eph get copy unset nonnull", caml_ephemeron_get_key_copy(x, 5, &y));
  is_true(test, "eph get nonull z", caml_ephemeron_get_key(z, 1, &y));
  is_true(test, "eph get eq z", y == ra);
  is_false(test, "eph get empty z", caml_ephemeron_get_key(z, 0, &y));

  is_false(test, "eph get data empty nonull", caml_ephemeron_get_data(x, &y));
  is_false(test, "eph get data copy empty nonnull", caml_ephemeron_get_data_copy(x, &y));
  caml_ephemeron_set_data(x, ra);
  is_true(test, "eph get data nonull", caml_ephemeron_get_data(x, &y));
  is_true(test, "eph get data eq", y == ra);
  is_true(test, "eph get data copy nonnull", caml_ephemeron_get_data_copy(x, &y));
  is_true(test, "eph get data copy eq", y != ra);
  caml_ephemeron_blit_data(x, z);
  caml_ephemeron_unset_data(x);
  is_false(test, "eph get data unset nonull", caml_ephemeron_get_data(x, &y));
  is_false(test, "eph get data copy unset nonnull", caml_ephemeron_get_data_copy(x, &y));
  is_true(test, "eph get nonull z", caml_ephemeron_get_data(z, &y));
  is_true(test, "eph get eq z", y == ra);

  x = caml_weak_array_create(15);
  z = caml_weak_array_create(3);
  is_true(test, "eph length=15", caml_weak_array_length(x) == 15);
  is_true(test, "eph length=3", caml_weak_array_length(z) == 3);

  is_false(test, "eph get empty nonull", caml_weak_array_get(x, 5, &y));
  is_false(test, "eph get copy empty nonnull", caml_weak_array_get_copy(x, 5, &y));
  caml_weak_array_set(x, 5, ra);
  is_true(test, "eph get nonull", caml_weak_array_get(x, 5, &y));
  is_true(test, "eph get eq", y == ra);
  is_true(test, "eph get copy nonnull", caml_weak_array_get_copy(x, 5, &y));
  is_true(test, "eph get copy eq", y != ra);
  caml_weak_array_blit(x, 4, z, 0, 3);
  caml_weak_array_unset(x, 5);
  is_false(test, "eph get unset nonull", caml_weak_array_get(x, 5, &y));
  is_false(test, "eph get copy unset nonnull", caml_weak_array_get_copy(x, 5, &y));
  is_true(test, "eph get nonull z", caml_weak_array_get(z, 1, &y));
  is_true(test, "eph get eq z", y == ra);
  is_false(test, "eph get empty z", caml_weak_array_get(z, 0, &y));

  CAMLreturn(Val_unit);
}
