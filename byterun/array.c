/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on arrays */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

CAMLexport mlsize_t caml_array_length(value array){
  tag_t tag = Tag_val(array);
  if (tag == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array){
  return (Tag_val(array) == Double_array_tag);
}

CAMLprim value caml_array_get_addr(value array, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

CAMLprim value caml_array_get_float(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

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

CAMLprim value caml_array_get(value array, value index)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_get_float(array, index);
  else
    return caml_array_get_addr(array, index);
}

CAMLprim value caml_array_set_addr(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

CAMLprim value caml_array_set_float(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_field(array, idx, Double_val(newval));
  return Val_unit;
}

CAMLprim value caml_array_set(value array, value index, value newval)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_set_float(array, index, newval);
  else
    return caml_array_set_addr(array, index, newval);
}

CAMLprim value caml_array_unsafe_get_float(value array, value index)
{
  double d;
  value res;

  d = Double_field(array, Long_val(index));
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value caml_array_unsafe_get(value array, value index)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_get_float(array, index);
  else
    return Field(array, Long_val(index));
}

CAMLprim value caml_array_unsafe_set_addr(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

CAMLprim value caml_array_unsafe_set_float(value array,value index,value newval)
{
  Store_double_field(array, Long_val(index), Double_val(newval));
  return Val_unit;
}

CAMLprim value caml_array_unsafe_set(value array, value index, value newval)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_set_float(array, index, newval);
  else
    return caml_array_unsafe_set_addr(array, index, newval);
}

CAMLprim value caml_make_vect(value len, value init)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, wsize, i;
  double d;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  }
  else if (Is_block(init)
           && Is_in_value_area(init)
           && Tag_val(init) == Double_tag) {
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("Array.make");
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_field(res, i, d);
    }
  } else {
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

CAMLprim value caml_make_array(value init)
{
  CAMLparam1 (init);
  mlsize_t wsize, size, i;
  CAMLlocal2 (v, res);

  size = Wosize_val(init);
  if (size == 0) {
    CAMLreturn (init);
  } else {
    v = Field(init, 0);
    if (Is_long(v)
        || ! Is_in_value_area(v)
        || Tag_val(v) != Double_tag) {
      CAMLreturn (init);
    } else {
      Assert(size < Max_young_wosize);
      wsize = size * Double_wosize;
      res = caml_alloc_small(wsize, Double_array_tag);
      for (i = 0; i < size; i++) {
        Store_double_field(res, i, Double_val(Field(init, i)));
      }
      CAMLreturn (res);
    }
  }
}
