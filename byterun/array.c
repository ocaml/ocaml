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

value array_get(value array, value index)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.get");
  return Field(array, idx);
}

value array_set(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.set");
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

value make_vect(value len, value init)      /* ML */
{
  value res;
  mlsize_t size, i;

  size = Long_val(len);
  if (size > Max_wosize) invalid_argument("Array.new");

  Begin_root(init);
    if (size == 0) {
      res = Atom(0);
    }
    else if (size < Max_young_wosize) {
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
  return res;
}
