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
#include <stdio.h>

extern value weak_list_head;


/* if a weak set with a bigger size is full, it triggers Major GC */
#define WEAKSET_TRIGGER_GC 8192
#define WEAKSET_THRESHOLD  32

#define None_val 1
#define Some_tag 0

value weakset_create (value len)        /* ML */
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 2;
  if (size > Max_wosize) invalid_argument ("Weakset.create");
  res = alloc_shr (size, Abstract_tag);
  for (i = 2; i < size; i++) Field (res, i) = 0;
  Field (res, 1) = Val_int(2);
  Field (res, 0) = weak_list_head;
  weak_list_head = res;
  return res;
}

value weakset_add(value ar,value el) /* ML */
{
  mlsize_t free = Int_val(Field(ar,1));
  mlsize_t sz = Wosize_val(ar);

  if(free == sz){/* the set is full */
    if(Wosize_val(ar) > WEAKSET_TRIGGER_GC){
      Begin_roots2(ar,el); /* (ar,el) */
      minor_collection();      
      finish_major_cycle();
      End_roots(); /* (ar,el) */
      free = Int_val(Field(ar,1));
    }
    if(free >= sz-(sz/WEAKSET_THRESHOLD)){/* the set should be reallocated */
      value res;
      mlsize_t size, i;
      
      size = 2*sz+2;
      if (size > Max_wosize) invalid_argument ("Weak set full");
      Begin_roots2(ar,el);  /* (ar,el) */
      res = alloc_shr (size, Abstract_tag);
      End_roots(); /* (ar,el) */

      for (i = 2; i < free; i++) /* copy the first set */
	initialize(&Field (res, i),Field(ar,i));
      for (; i < size; i++) Field (res, i) = 0;

      Field(ar,1) = Val_int(2); /* now, ar is empty */
      Field(res,0) = weak_list_head;
      weak_list_head = res;
      ar = res;
    }
  }
  Modify (&Field (ar, free), el);
  free++;
  Field(ar,1) = Val_int(free);

  return ar;
}

#define Setup_for_gc
#define Restore_after_gc

value weakset_array (value ar) /* ML */
{
  value res,elt;
  mlsize_t i,len;

  len = Int_val(Field(ar,1));

  if(len==2)
    return Atom(0);
  else 
    if(len<Max_young_wosize){
      Begin_root(ar);
      res = alloc(len-2,0);
      End_roots();
      for(i=2;i<len;i++){
	elt = Field (ar, i);
	if (gc_phase == Phase_mark)  {
	  darken (elt, NULL);
	}
	Field(res,i-2) = elt;
      }
    } else {
      Begin_root(ar);
      res = alloc_shr(len-2,0);
      End_roots();
      
      for(i=2;i<len;i++){
	elt = Field (ar, i);
	if (gc_phase == Phase_mark){
	  darken (elt, NULL);
	}
	initialize(&Field(res,i-2),elt);
      }
    }
  return res;
}

value weakset_remove (value ar, value el) /* ML */
{
  mlsize_t i,len;
  value elt;

  len = Int_val(Field(ar,1));
  for(i=2;i<len;i++){
    elt=Field (ar, i);
    if (gc_phase == Phase_mark) {
    darken (elt, NULL);}
    if(elt == el){
      Modify(&Field (ar, i),Field (ar, len-1));
      Field(ar,1) = Val_int(len-1);
      return Val_true;
    }
  }
  return Val_false;
}

#undef Setup_for_gc
#undef Restore_after_gc 

value weakset_info (value ar) /* ML */
{
  value res;

  Begin_root(ar);
  res = alloc(2,0);
  End_roots();

  Field(res,0) = Val_int(Wosize_val(ar));
  Field(res,1) = Val_int(Int_val(Field(ar,1))-2);

  return res;
}

