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
CAMLexport mlsize_t caml_double_array_length(value array)
{
  Assert (Tag_val(array) == Double_array_tag);
  return Wosize_val(array) / Double_wosize;
}

CAMLexport int caml_is_double_array(value array)
{
  Assert (Tag_val(array) == Double_array_tag);
  return (Tag_val(array) == Double_array_tag);
}

CAMLprim value caml_double_array_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  Assert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value caml_double_array_set(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  Assert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_field(array, idx, Double_val(newval));
  return Val_unit;
}

CAMLprim value caml_double_array_unsafe_get(value array, value index)
{
  double d;
  value res;

  Assert (Tag_val(array) == Double_array_tag);
  d = Double_field(array, Long_val(index));
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value caml_double_array_unsafe_set(value array,value index,value newval)
{
  Assert (Tag_val(array) == Double_array_tag);
  Store_double_field(array, Long_val(index), Double_val(newval));
  return Val_unit;
}

/* [len] is a [value] representing number of floats */
CAMLprim value caml_create_double_array(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  value result;
  if (wosize == 0)
    return Atom(0);
  else if (wosize <= Max_young_wosize){
#define Setup_for_gc
#define Restore_after_gc
    Alloc_small (result, wosize, Double_array_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  }else if (wosize > Max_wosize)
    caml_invalid_argument("FloatArray.create");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  return result;
}

/* [len] is a [value] representing number of floats */
CAMLprim value caml_make_double_array(value len, value init)
{
  mlsize_t i;
  mlsize_t size = Long_val(len);
  mlsize_t wosize = size * Double_wosize;
  double d = Double_val(init);
  value result;
  if (wosize == 0)
    return Atom(0);
  else if (wosize <= Max_young_wosize){
#define Setup_for_gc
#define Restore_after_gc
    Alloc_small (result, wosize, Double_array_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  }else if (wosize > Max_wosize)
    caml_invalid_argument("FloatArray.make");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  for (i = 0; i < size; i++) Double_field(result, i) = d;
  return result;
}

/* Blitting */

CAMLprim value caml_double_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
  Assert (Tag_val(a1) == Double_array_tag);
  Assert (Tag_val(a2) == Double_array_tag);
  /* Arrays of floats.  The values being copied are floats, not
     pointer, so we can do a direct copy.  memmove takes care of
     potential overlap between the copied areas. */
  memmove((double *)a2 + Long_val(ofs2),
            (double *)a1 + Long_val(ofs1),
            Long_val(n) * sizeof(double));
    return Val_unit;

}

CAMLprim value caml_double_array_sub(value a, value ofs, value len)
{
  CAMLparam1(a);
  value res;                    /* no need to register it as a root */
  mlsize_t size, wsize, offset;

  Assert (Tag_val(a) == Double_array_tag);
  /* Determine total size */
  size = Long_val(len);
  offset = Long_val(ofs);
  if (size == 0) {
    /* If size = 0, just return empty array */
    res = Atom(0);
  }
  else {
    /* We use memcpy directly. */
    wsize = size * Double_wosize;
    res = caml_alloc(wsize, Double_array_tag);
    memcpy((double *)res,
           (double *)a + offset,
           size * sizeof(double));
  }
  CAMLreturn (res);
}

/* A generic function for concatenation of sub-arrays */

static value caml_double_array_gather(intnat num_arrays,
                                      value arrays[/*num_arrays*/],
                                      intnat lengths[/*num_arrays*/])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */
  mlsize_t i, size, wsize, pos;

  /* Determine total size */
  size = 0;
  for (i = 0; i < num_arrays; i++) {
    size += lengths[i];
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
  else {
    /* We use memcpy directly. */
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("FloatArray.concat");
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy((double *)res + pos,
             (double *)arrays[i],
             lengths[i] * sizeof(double));
      pos += lengths[i];
    }
    Assert(pos == size);
  }
  CAMLreturn (res);
}

CAMLprim value caml_double_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat lengths[2] = { caml_double_array_length(a1),
                        caml_double_array_length(a2) };
  Assert (Tag_val(a1) == Double_array_tag);
  Assert (Tag_val(a2) == Double_array_tag);
  return caml_double_array_gather(2, arrays, lengths);
}

CAMLprim value caml_double_array_concat(value al)
{
#define STATIC_SIZE 16
  value static_arrays[STATIC_SIZE], * arrays;
  intnat static_lengths[STATIC_SIZE], * lengths;
  intnat n, i;
  value l, res;

  /* Length of list = number of arrays */
  for (n = 0, l = al; l != Val_int(0); l = Field(l, 1)) n++;
  /* Allocate extra storage if too many arrays */
  if (n <= STATIC_SIZE) {
    arrays = static_arrays;
    lengths = static_lengths;
  } else {
    arrays = caml_stat_alloc(n * sizeof(value));
    lengths = caml_stat_alloc(n * sizeof(value));
  }
  /* Build the parameters to caml_array_gather */
  for (i = 0, l = al; l != Val_int(0); l = Field(l, 1), i++) {
    arrays[i] = Field(l, 0);
    Assert (Tag_val(arrays[i]) == Double_array_tag);
    lengths[i] = caml_double_array_length(Field(l, 0));
  }
  /* Do the concatenation */
  res = caml_double_array_gather(n, arrays, lengths);
  /* Free the extra storage if needed */
  if (n > STATIC_SIZE) {
    caml_stat_free(arrays);
    caml_stat_free(lengths);
  }
  return res;
}
