/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stacks.h"

#define Setup_for_gc
#define Restore_after_gc

#define caml_alloc_shr(size, tag) \
  (caml_allocation_profiling \
    ? caml_alloc_shr_with_profinfo(size, tag, MY_PROFINFO) \
    : caml_alloc_shr(size, tag))

CAMLexport value caml_alloc_with_profinfo (mlsize_t wosize, tag_t tag, intnat profinfo)
{
  value result;
  mlsize_t i;

  Assert (tag < 256);
  Assert (tag != Infix_tag);
  if (wosize == 0){
    result = Atom (tag);
  }else if (wosize <= Max_young_wosize){
    ALLOCATION_ENTRY_POINT;
    Alloc_small_with_profinfo (result, wosize, tag, profinfo);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
    }
  }else{
    ALLOCATION_ENTRY_POINT;
    result = caml_alloc_shr_with_profinfo (wosize, tag, profinfo);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
    }
    result = caml_check_urgent_gc (result);
  }
  return result;
}

CAMLexport value caml_alloc (mlsize_t wosize, tag_t tag)
{
  return caml_alloc_with_profinfo (wosize, tag, MY_PROFINFO);
}

CAMLexport value caml_alloc_small_with_profinfo (mlsize_t wosize, tag_t tag,
                                                 intnat profinfo)
{
  value result;

  Assert (wosize > 0);
  Assert (wosize <= Max_young_wosize);
  Assert (tag < 256);

  ALLOCATION_ENTRY_POINT;

  Alloc_small_with_profinfo (result, wosize, tag, profinfo);

  return result;
}

CAMLexport value caml_alloc_small (mlsize_t wosize, tag_t tag)
{
  return caml_alloc_small_with_profinfo (wosize, tag, MY_PROFINFO);
}

CAMLexport value caml_alloc_tuple(mlsize_t n)
{
  ALLOCATION_ENTRY_POINT;

  return caml_alloc_with_profinfo(n, 0, MY_PROFINFO);
}

CAMLexport value caml_alloc_string_with_profinfo (mlsize_t len, intnat profinfo)
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  ALLOCATION_ENTRY_POINT;

  if (wosize <= Max_young_wosize) {
    Alloc_small_with_profinfo (result, wosize, String_tag, profinfo);
  }else{
    result = caml_alloc_shr (wosize, String_tag);
    result = caml_alloc_shr_with_profinfo (wosize, String_tag, profinfo);
    result = caml_check_urgent_gc (result);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

CAMLexport value caml_alloc_string (mlsize_t len)
{
  return caml_alloc_string_with_profinfo (len, MY_PROFINFO);
}

CAMLexport value caml_alloc_final (mlsize_t len, final_fun fun,
                                   mlsize_t mem, mlsize_t max)
{
  ALLOCATION_ENTRY_POINT;

  return caml_alloc_custom(caml_final_custom_operations(fun),
                           len * sizeof(value), mem, max);
}

CAMLexport value caml_copy_string(char const *s)
{
  int len;
  value res;

  ALLOCATION_ENTRY_POINT;

  len = strlen(s);
  res = caml_alloc_string_with_profinfo(len, MY_PROFINFO);
  memmove(String_val(res), s, len);
  return res;
}

CAMLexport value caml_alloc_array_with_profinfo(value (*funct)(char const *),
                                                char const ** arr, intnat profinfo)
{
  CAMLparam0 ();
  mlsize_t nbr, n;
  CAMLlocal2 (v, result);

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    CAMLreturn (Atom(0));
  } else {
    ALLOCATION_ENTRY_POINT;
    result = caml_alloc_with_profinfo (nbr, 0, profinfo);
    for (n = 0; n < nbr; n++) {
      /* The two statements below must be separate because of evaluation
         order (don't take the address &Field(result, n) before
         calling funct, which may cause a GC and move result). */
      v = funct(arr[n]);
      caml_modify(&Field(result, n), v);
    }
    CAMLreturn (result);
  }
}

CAMLexport value caml_alloc_array(value (*funct)(char const *),
                                  char const ** arr)
{
  return caml_alloc_array_with_profinfo (funct, arr, MY_PROFINFO);
}

CAMLexport value caml_copy_string_array(char const ** arr)
{
  ALLOCATION_ENTRY_POINT;
  return caml_alloc_array_with_profinfo(caml_copy_string, arr, MY_PROFINFO);
}

CAMLexport int caml_convert_flag_list(value list, int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}

/* For compiling let rec over values */

CAMLprim value caml_alloc_dummy(value size)
{
  mlsize_t wosize = Int_val(size);

  if (wosize == 0) return Atom(0);
  ALLOCATION_ENTRY_POINT;
  return caml_alloc_with_profinfo (wosize, 0, MY_PROFINFO);
}

CAMLprim value caml_alloc_dummy_float (value size)
{
  mlsize_t wosize = Int_val(size) * Double_wosize;

  if (wosize == 0) return Atom(0);
  ALLOCATION_ENTRY_POINT;
  return caml_alloc_with_profinfo (wosize, 0, MY_PROFINFO);
}

CAMLprim value caml_update_dummy(value dummy, value newval)
{
  mlsize_t size, i;
  tag_t tag;

  size = Wosize_val(newval);
  tag = Tag_val (newval);
  Assert (size == Wosize_val(dummy));
  Assert (tag < No_scan_tag || tag == Double_array_tag);

  Tag_val(dummy) = tag;
  if (tag == Double_array_tag){
    size = Wosize_val (newval) / Double_wosize;
    for (i = 0; i < size; i++){
      Store_double_field (dummy, i, Double_field (newval, i));
    }
  }else{
    for (i = 0; i < size; i++){
      caml_modify (&Field(dummy, i), Field(newval, i));
    }
  }
  return Val_unit;
}
