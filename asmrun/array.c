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

value make_vect(len, init)
     value len, init;
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
    if (wsize > Max_wosize) invalid_argument("Array.new");
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
    if (size > Max_wosize) invalid_argument("Array.new");
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

value make_array(init)
     value init;
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
      wsize = size * Double_wosize;
      if (wsize > Max_wosize) invalid_argument("Array.new");
      Begin_root(init);
        if (wsize < Max_young_wosize) {
	  res = alloc(wsize, Double_array_tag);
	} else {
	  res = alloc_shr(wsize, Double_array_tag);
	  res = check_urgent_gc (res);
	}
	for (i = 0; i < size; i++) {
	  Store_double_field(res, i, Double_val(Field(init, i)));
	}
      End_roots();
      return res;
    }
  }
}

void array_bound_error()
{
  fatal_error("Fatal error: out-of-bound access in array or string\n");
}
