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

#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "str.h"

/* Structural comparison on trees.
   May loop on cyclic structures. */

static long compare_val(v1, v2)
     value v1,v2;
{
  tag_t t1, t2;

 tailcall:
  if (v1 == v2) return 0;
  if (Is_long(v1)) {
    if (Is_long(v2))
      return Long_val(v1) - Long_val(v2);
    else
      return -1;
  }
  if (Is_long(v2)) return 1;
  /* If one of the objects is outside the heap (but is not an atom),
     use address comparison. Since both addresses are 2-aligned,
     shift lsb off to avoid overflow in subtraction. */
  if ((!Is_atom(v1) && !Is_young(v1) && !Is_in_heap(v1)) ||
      (!Is_atom(v2) && !Is_young(v2) && !Is_in_heap(v2)))
      return (v1 >> 1) - (v2 >> 1);
  t1 = Tag_val(v1);
  t2 = Tag_val(v2);
  if (t1 != t2) return (long)t1 - (long)t2;
  switch(t1) {
  case String_tag: {
    mlsize_t len1, len2, len;
    unsigned char * p1, * p2;
    len1 = string_length(v1);
    len2 = string_length(v2);
    for (len = (len1 <= len2 ? len1 : len2),
         p1 = (unsigned char *) String_val(v1),
         p2 = (unsigned char *) String_val(v2);
         len > 0;
         len--, p1++, p2++)
      if (*p1 != *p2) return (long)*p1 - (long)*p2;
    return len1 - len2;
  }
  case Double_tag: {
    double d1 = Double_val(v1);
    double d2 = Double_val(v2);
    if (d1 < d2) return -1; else if (d1 > d2) return 1; else return 0;
  }
  case Double_array_tag: {
    mlsize_t sz1 = Wosize_val(v1) / Double_wosize;
    mlsize_t sz2 = Wosize_val(v2) / Double_wosize;
    mlsize_t i;
    if (sz1 != sz2) return sz1 - sz2;
    for (i = 0; i < sz1; i++) {
      double d1 = Double_field(v1, i);
      double d2 = Double_field(v2, i);
      if (d1 < d2) return -1; else if (d1 > d2) return 1;
    }
    return 0;
  }
  case Abstract_tag:
  case Final_tag:
    invalid_argument("equal: abstract value");
  case Closure_tag:
  case Infix_tag:
    invalid_argument("equal: functional value");
  case Object_tag:
    return (Oid_val(v1) - Oid_val(v2));
  default: {
    mlsize_t sz1 = Wosize_val(v1);
    mlsize_t sz2 = Wosize_val(v2);
    value * p1, * p2;
    long res;
    if (sz1 != sz2) return sz1 - sz2;
    if (sz1 == 0) return 0;
    for(p1 = Op_val(v1), p2 = Op_val(v2);
        sz1 > 1;
        sz1--, p1++, p2++) {
      res = compare_val(*p1, *p2);
      if (res != 0) return res;
    }
    v1 = *p1;
    v2 = *p2;
    goto tailcall;
  }
  }
}

value compare(v1, v2)           /* ML */
     value v1, v2;
{
  long res = compare_val(v1, v2);
  if (res < 0) 
    return Val_int(-1);
  else if (res > 0)
    return Val_int(1);
  else
    return Val_int(0);
}

value equal(v1, v2)            /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) == 0);
}

value notequal(v1, v2)            /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) != 0);
}

value lessthan(v1, v2)            /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) < 0);
}

value lessequal(v1, v2)          /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) <= 0);
}

value greaterthan(v1, v2)        /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) > 0);
}

value greaterequal(v1, v2)       /* ML */
     value v1, v2;
{
  return Val_int(compare_val(v1, v2) >= 0);
}

