/***********************************************************************/
/*                                                                     */
/*                           OCaml                                     */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on arrays */

#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* returns number of elements */
CAMLexport mlsize_t caml_array_length(value array)
{
  Assert (Tag_val(array) != Double_array_tag);
  return Wosize_val(array);
}

CAMLprim value caml_array_get(value array, value index)
{
  intnat idx = Long_val(index);
  Assert (Tag_val(array) != Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

CAMLprim value caml_array_set(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  Assert (Tag_val(array) != Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

CAMLprim value caml_array_unsafe_get(value array, value index)
{
  Assert (Tag_val(array) != Double_array_tag);
  return Field(array, Long_val(index));
}

CAMLprim value caml_array_unsafe_set(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  Assert (Tag_val(array) != Double_array_tag);
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [len] is a [value] representing number of words or floats */
CAMLprim value caml_make_vect(value len, value init)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, i;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  } else  {
    if (size > Max_wosize) caml_invalid_argument("Array.make");
    if (size < Max_young_wosize) {
      res = caml_alloc_small(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
    else if (Is_block(init) && Is_young(init)) {
      caml_minor_collection();
      res = caml_alloc_shr(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
      res = caml_check_urgent_gc (res);
    }
    else {
      res = caml_alloc_shr(size, 0);
      for (i = 0; i < size; i++) caml_initialize(&Field(res, i), init);
      res = caml_check_urgent_gc (res);
    }
  }
  CAMLreturn (res);
}

/* Blitting */

CAMLprim value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
  value * src, * dst;
  intnat count;

  Assert (Tag_val(a1) != Double_array_tag);
  Assert (Tag_val(a2) != Double_array_tag);
  if (Is_young(a2)) {
    /* Arrays of values, destination is in young generation.
       Here we can do a direct copy since this cannot create
       old-to-young pointers, nor mess up with the incremental major GC.
       Again, memmove takes care of overlap. */
    memmove(&Field(a2, Long_val(ofs2)),
            &Field(a1, Long_val(ofs1)),
            Long_val(n) * sizeof(value));
    return Val_unit;
  }
  /* Array of values, destination is in old generation.
     We must use caml_modify.  */
  count = Long_val(n);
  if (a1 == a2 && Long_val(ofs1) < Long_val(ofs2)) {
    /* Copy in descending order */
    for (dst = &Field(a2, Long_val(ofs2) + count - 1),
           src = &Field(a1, Long_val(ofs1) + count - 1);
         count > 0;
         count--, src--, dst--) {
      caml_modify(dst, *src);
    }
  } else {
    /* Copy in ascending order */
    for (dst = &Field(a2, Long_val(ofs2)), src = &Field(a1, Long_val(ofs1));
         count > 0;
         count--, src++, dst++) {
      caml_modify(dst, *src);
    }
  }
  /* Many caml_modify in a row can create a lot of old-to-young refs.
     Give the minor GC a chance to run if it needs to. */
  caml_check_urgent_gc(Val_unit);
  return Val_unit;
}

/* A generic function for extraction and concatenation of sub-arrays */

static value caml_array_gather(intnat num_arrays,
                               value arrays[/*num_arrays*/],
                               intnat offsets[/*num_arrays*/],
                               intnat lengths[/*num_arrays*/])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */
  mlsize_t i, size, count, pos;
  value * src;

  /* Determine total size */
  size = 0;
  for (i = 0; i < num_arrays; i++) {
    size += lengths[i];
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.concat");
  }
  else if (size < Max_young_wosize) {
    /* Array of values, small enough to fit in young generation.
       We can use memcpy directly. */
    res = caml_alloc_small(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy(&Field(res, pos),
             &Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    Assert(pos == size);
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    pos = 0;
    for (i = 0, pos = 0; i < num_arrays; i++) {
      for (src = &Field(arrays[i], offsets[i]), count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    Assert(pos == size);

    /* Many caml_initialize in a row can create a lot of old-to-young
       refs.  Give the minor GC a chance to run if it needs to. */
    res = caml_check_urgent_gc(res);
  }
  CAMLreturn (res);
}

CAMLprim value caml_array_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  Assert (Tag_val(a) != Double_array_tag);
  return caml_array_gather(1, arrays, offsets, lengths);
}

CAMLprim value caml_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  Assert (Tag_val(a1) != Double_array_tag);
  Assert (Tag_val(a2) != Double_array_tag);
  return caml_array_gather(2, arrays, offsets, lengths);
}

CAMLprim value caml_array_concat(value al)
{
#define STATIC_SIZE 16
  value static_arrays[STATIC_SIZE], * arrays;
  intnat static_offsets[STATIC_SIZE], * offsets;
  intnat static_lengths[STATIC_SIZE], * lengths;
  intnat n, i;
  value l, res;

  /* Length of list = number of arrays */
  for (n = 0, l = al; l != Val_int(0); l = Field(l, 1)) n++;
  /* Allocate extra storage if too many arrays */
  if (n <= STATIC_SIZE) {
    arrays = static_arrays;
    offsets = static_offsets;
    lengths = static_lengths;
  } else {
    arrays = caml_stat_alloc(n * sizeof(value));
    offsets = caml_stat_alloc(n * sizeof(intnat));
    lengths = caml_stat_alloc(n * sizeof(value));
  }
  /* Build the parameters to caml_array_gather */
  for (i = 0, l = al; l != Val_int(0); l = Field(l, 1), i++) {
    arrays[i] = Field(l, 0);
    Assert (Tag_val(arrays[i]) != Double_array_tag);
    offsets[i] = 0;
    lengths[i] = caml_array_length(Field(l, 0));
  }
  /* Do the concatenation */
  res = caml_array_gather(n, arrays, offsets, lengths);
  /* Free the extra storage if needed */
  if (n > STATIC_SIZE) {
    caml_stat_free(arrays);
    caml_stat_free(offsets);
    caml_stat_free(lengths);
  }
  return res;
}
