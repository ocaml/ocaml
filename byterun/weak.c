/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on weak arrays */

#include <string.h>

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

value weak_list_head = 0;

static value weak_dummy = 0;
value weak_none = (value) &weak_dummy;

CAMLprim value weak_create (value len)
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1;
  if (size <= 0 || size > Max_wosize) invalid_argument ("Weak.create");
  res = alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = weak_none;
  Field (res, 0) = weak_list_head;
  weak_list_head = res;
  return res;
}

#define None_val (Val_int(0))
#define Some_tag 0

CAMLprim value weak_set (value ar, value n, value el)
{
  mlsize_t offset = Long_val (n) + 1;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.set");
  Field (ar, offset) = weak_none;
  if (el != None_val){
    value v;                                  Assert (Wosize_val (el) == 1);
    v = Field (el, 0);
    if (Is_block (v) && (Is_young (v) || Is_in_heap (v))){
      Modify (&Field (ar, offset), v);
    }
  }
  return Val_unit;
}

#define Setup_for_gc
#define Restore_after_gc

CAMLprim value weak_get (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 1;
  CAMLlocal2 (res, elt);
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.get");
  if (Field (ar, offset) == weak_none){
    res = None_val;
  }else{
    elt = Field (ar, offset);
    if (gc_phase == Phase_mark && Is_block (elt) && Is_in_heap (elt)){
      darken (elt, NULL);
    }
    res = alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn (res);
}

#undef Setup_for_gc
#undef Restore_after_gc

CAMLprim value weak_get_copy (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 1;
  CAMLlocal2 (res, elt);
  value v;  /* Caution: this is NOT a local root. */
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.get");

  v = Field (ar, offset);
  if (v == weak_none) CAMLreturn (None_val);
  if (Is_block (v) && (Is_young (v) || Is_in_heap (v))){
    elt = alloc (Wosize_val (v), Tag_val (v)); /* The GC may erase or move v. */
    v = Field (ar, offset);
    if (v == weak_none) CAMLreturn (None_val);
    if (Tag_val (v) < No_scan_tag){
      mlsize_t i;
      for (i = 0; i < Wosize_val (v); i++){
        Modify (&Field (elt, i), Field (v, i));
      }
    }else{
      memmove (Bp_val (elt), Bp_val (v), Bosize_val (v));
    }
  }else{
    elt = v;
  }
  res = alloc_small (1, Some_tag);
  Field (res, 0) = elt;

  CAMLreturn (res);
}

CAMLprim value weak_check (value ar, value n)
{
  mlsize_t offset = Long_val (n) + 1;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.get");
  return Val_bool (Field (ar, offset) != weak_none);
}
