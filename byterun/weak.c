/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on weak arrays */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

value weak_list_head = 0;

value weak_create (len)        /* ML */
     value len;
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1;
  if (size > Max_wosize) invalid_argument ("Weak.create");
  res = alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = 0;
  Field (res, 0) = weak_list_head;
  weak_list_head = res;
  return res;
}

#define None_val 1
#define Some_tag 0

value weak_set (ar, n, el)     /* ML */
     value ar, n, el;
{
  mlsize_t offset = Long_val (n) + 1;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.set");
  Field (ar, offset) = 0;
  if (el != None_val){                  Assert (Wosize_val (el) == 1);
    Modify (&Field (ar, offset), Field (el, 0));
  }
  return Val_unit;
}

#define Setup_for_gc
#define Restore_after_gc

value weak_get (ar, n)        /* ML */
     value ar, n;
{
  mlsize_t offset = Long_val (n) + 1;
  value res;
  value elt;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.get");
  if (Field (ar, offset) == 0){
    res = None_val;
  }else{
    elt = Field (ar, offset);
    darken (elt, NULL);
    Begin_root(elt);
      res = alloc (1, Some_tag);
    End_roots ();
    Field (res, 0) = elt;
  }
  return res;
}

#undef Setup_for_gc
#undef Restore_after_gc
