/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Fabrice Le Fessant & Natacha Duval                      */
/*                projet Para, INRIA Rocquencourt                      */
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
#include "minor_gc.h"
#include <stdio.h>

extern value weak_list_head;


/* if a weak set with a bigger size is full, it triggers Major GC */
#define WEAKSET_TRIGGER_GC 8192
#define WEAKSET_THRESHOLD  32

#define None_val 1
#define Some_tag 0

value weakarray_create (value len)        /* ML */
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 2;
  if (size > Max_wosize) invalid_argument ("Weakarray.create");
  res = alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++)
    Field (res, i) = Val_int(i+1);

  Field (res, 0) = weak_list_head;
  weak_list_head = res;
  return res;
}

value weakarray_realloc(value ar,value new_size)   /* ML */
{
  value res;
  mlsize_t size, i;
  mlsize_t sz = Wosize_val(ar);
      
  size = 2+Int_val(new_size);
  if (size > Max_wosize || size <= sz) 
    invalid_argument ("Weakarray.realloc: invalid size");

  Begin_root(ar); /* (ar) */
  res = alloc_shr (size, Abstract_tag);
  End_roots(); /* (ar) */

  for (i = 1; i < sz; i++) /* copy the first set */
    initialize(&Field (res, i),Field(ar,i));
  for (; i < size; i++) 
    Field (res, i) = Val_int(i+1);

  Field(res,0) = weak_list_head;
  weak_list_head = res;

  return res;
}


value weakarray_add(value ar,value el) /* ML */
{
  mlsize_t free = Int_val(Field(ar,1));
  mlsize_t sz = Wosize_val(ar);
  value tmp_free;

  if(free == sz){/* the set is full */
    if(Wosize_val(ar) > WEAKSET_TRIGGER_GC){
      Begin_roots2(ar,el); /* (ar,el) */
      minor_collection();
      finish_major_cycle();
      End_roots(); /* (ar,el) */
      free = Int_val(Field(ar,1));
    }
    if(free == sz) /* the set is still full */
      failwith("Weakarray.add: set full");
  }
  tmp_free = Field (ar, free);
  if(el == None_val)
    Field(ar,free) = 0;
  else {
    el = Field(el,0);
    if(Is_long(el)) 
      invalid_argument("Can't add integers in weak arrays");
    Modify (&Field (ar, free), el);
  }
  Field(ar,1) = tmp_free;

  return Val_int(free-2);
}

value weakarray_free(value ar,value n) /* ML */
{
  mlsize_t offset = Long_val (n) + 2;
  value tmp_free;

  tmp_free = Field(ar,offset);
  if(Is_long(tmp_free)) return Val_unit;
  
  Field(ar,offset) = Field(ar,1);
  Field(ar,1) = Val_int(offset);

  return Val_unit;
}

value weakarray_set (value ar, value n, value el)     /* ML */
{
  mlsize_t offset = Long_val (n) + 2;
  value tmp_free;
  mlsize_t free;
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)) invalid_argument ("Weakarray.set");
  tmp_free = Field (ar, offset);
  if(Is_long(tmp_free)){ /* an empty slot */
    if(el == None_val) /* that's OK */
      Field (ar, offset) = 0;
    else
      Modify (&Field (ar, offset), Field (el, 0));
    for(free=1;Field(ar,free)!=Val_int(offset);free = Int_val(Field(ar,free)));
    Field(ar,free) = tmp_free;
    return Val_unit;
  }
  Field(ar,offset) = 0;
  if (el != None_val){                  Assert (Wosize_val (el) == 1);
    Modify (&Field (ar, offset), Field (el, 0));
  }
  
  return Val_unit;
}


#define Setup_for_gc
#define Restore_after_gc

value weakarray_get (value ar, value n)        /* ML */
{
  mlsize_t offset = Long_val (n) + 2;
  value res;
  value elt;

  Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)) invalid_argument ("Weakarray.get");
  elt = Field (ar, offset);
  if (Is_long(elt) || elt==0){
    res = None_val;
  }else{
    if (gc_phase == Phase_mark) darken (elt, NULL);
    Begin_root(elt);
      res = alloc (1, Some_tag);
    End_roots ();
    Field (res, 0) = elt;
  }
  return res;
}

#undef Setup_for_gc
#undef Restore_after_gc
 

