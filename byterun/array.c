/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on arrays */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

#ifndef NATIVE_CODE

value array_get_addr(value array, value index) /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.get");
  return Field(array, idx);
}

value array_get_float(value array, value index) /* ML */
{
  long idx = Long_val(index);
  double d;
  value res;

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    invalid_argument("Array.get");
  d = Double_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

value array_get(value array, value index)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_get_float(array, index);
  else
    return array_get_addr(array, index);
}

value array_set_addr(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.set");
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

value array_set_float(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    invalid_argument("Array.set");
  Store_double_field(array, idx, Double_val(newval));
  return Val_unit;
}

value array_set(value array, value index, value newval)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_set_float(array, index, newval);
  else
    return array_set_addr(array, index, newval);
}

value array_unsafe_get_float(value array, value index) /* ML */
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

value array_unsafe_get(value array, value index)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_unsafe_get_float(array, index);
  else
    return Field(array, Long_val(index));
}

value array_unsafe_set_addr(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

value array_unsafe_set_float(value array, value index, value newval)   /* ML */
{
  Store_double_field(array, Long_val(index), Double_val(newval));
  return Val_unit;
}

value array_unsafe_set(value array, value index, value newval)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_unsafe_set_float(array, index, newval);
  else
    return array_unsafe_set_addr(array, index, newval);
}

#endif

value make_vect(value len, value init) /* ML */
{
  value res;
  mlsize_t size, wsize, i;
  double d;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  }
  else if (Is_block(init) && Tag_val(init) == Double_tag) {
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) invalid_argument("Array.make");
    if (wsize < Max_young_wosize) {
      res = alloc(wsize, Double_array_tag);
    } else {
      res = alloc_shr(wsize, Double_array_tag);
      res = check_urgent_gc (res);
    }
    for (i = 0; i < size; i++) {
      Store_double_field(res, i, d);
    }
  } else {
    if (size > Max_wosize) invalid_argument("Array.make");
    Begin_root(init);
      if (size < Max_young_wosize) {
	res = alloc(size, 0);
	for (i = 0; i < size; i++) Field(res, i) = init;
      }
      else if (Is_block(init) && Is_young(init)) {
	minor_collection();
	res = alloc_shr(size, 0);
	for (i = 0; i < size; i++) Field(res, i) = init;
	res = check_urgent_gc (res);
      }
      else {
	res = alloc_shr(size, 0);
	for (i = 0; i < size; i++) initialize(&Field(res, i), init);
	res = check_urgent_gc (res);
      }
    End_roots();
  }
  return res;
}

value make_array(value init)    /* ML */
{
  mlsize_t wsize, size, i;
  value v, res;

  size = Wosize_val(init);
  if (size == 0) {
    return init;
  } else {
    v = Field(init, 0);
    if (Is_long(v) || Tag_val(v) != Double_tag) {
      return init;
    } else {
      Assert(size < Max_young_wosize);
      wsize = size * Double_wosize;
      Begin_root(init);
        res = alloc(wsize, Double_array_tag);
	for (i = 0; i < size; i++) {
	  Store_double_field(res, i, Double_val(Field(init, i)));
	}
      End_roots();
      return res;
    }
  }
}

#ifdef NATIVE_CODE

void array_bound_error(void)
{
  fatal_error("Fatal error: out-of-bound access in array or string\n");
}

#endif
