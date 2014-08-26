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

/* Operations on objects */

#include <string.h>
#include "alloc.h"
#include "fail.h"
#include "gc.h"
#include "interp.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"

/* all uses of this are bugs */
CAMLprim value caml_static_alloc(value size)
{
  return (value) caml_stat_alloc((asize_t) Long_val(size));
}

CAMLprim value caml_static_free(value blk)
{
  return Val_unit;
}

/* signal to the interpreter machinery that a bytecode is no more
   needed (before freeing it) - this might be useful for a JIT
   implementation */

CAMLprim value caml_static_release_bytecode(value blk, value size)
{
#ifndef NATIVE_CODE
  caml_release_bytecode((code_t) blk, (asize_t) Long_val(size));
#else
  caml_failwith("Meta.static_release_bytecode impossible with native code");
#endif
  return Val_unit;
}


CAMLprim value caml_static_resize(value blk, value new_size)
{
  return (value) caml_stat_resize((char *) blk, (asize_t) Long_val(new_size));
}

CAMLprim value caml_obj_is_block(value arg)
{
  return Val_bool(Is_block(arg));
}

CAMLprim value caml_obj_tag(value arg)
{
  if (Is_long (arg)){
    return Val_int (1000);   /* int_tag */
  }else if ((long) arg & (sizeof (value) - 1)){
    return Val_int (1002);   /* unaligned_tag */
  }else{
    return Val_int(Tag_val(arg));
  }
}

CAMLprim value caml_obj_set_tag (value arg, value new_tag)
{
  Tag_val (arg) = Int_val (new_tag);
  return Val_unit;
}

CAMLprim value caml_obj_block(value tag, value size)
{
  return caml_alloc(Long_val(size), Long_val(tag));
}

CAMLprim value caml_obj_dup(value arg)
{
  CAMLparam1 (arg);
  CAMLlocal1 (res);
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  if (sz == 0) CAMLreturn (arg);
  tg = Tag_val(arg);
  if (tg >= No_scan_tag) {
    res = caml_alloc(sz, tg);
    memcpy(Bp_val(res), Bp_val(arg), sz * sizeof(value));
  } else if (sz <= Max_young_wosize) {
    res = caml_alloc_small(sz, tg);
    for (i = 0; i < sz; i++) Init_field(res, i, Field(arg, i));
  } else {
    res = caml_alloc_shr(sz, tg);
    for (i = 0; i < sz; i++) caml_initialize_field(res, i, Field(arg, i));
  }
  CAMLreturn (res);
}

CAMLprim value caml_obj_truncate (value v, value newsize)
{
  caml_failwith("Obj.truncate not supported");
}

CAMLprim value caml_obj_add_offset (value v, value offset)
{
  return v + (unsigned long) Int32_val (offset);
}

/* The following functions are used in stdlib/lazy.ml.
   They are not written in OCaml because they must be atomic with respect
   to the GC.
 */

CAMLprim value caml_lazy_follow_forward (value v)
{
  if (Is_block (v) && Tag_val (v) == Forward_tag){
    return Forward_val (v);
  }else{
    return v;
  }
}

CAMLprim value caml_lazy_make_forward (value v)
{
  CAMLparam1 (v);
  CAMLlocal1 (res);

  res = caml_alloc_small (1, Forward_tag);
  Init_field (res, 0, v);
  CAMLreturn (res);
}

/* For mlvalues.h and camlinternalOO.ml
   See also GETPUBMET in interp.c
 */

CAMLprim value caml_get_public_method (value obj, value tag)
{
  value meths = Field (obj, 0);
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return (tag == Field(meths,li) ? Field (meths, li-1) : 0);
}

/* these two functions might be useful to an hypothetical JIT */

#ifdef CAML_JIT
#ifdef NATIVE_CODE
#define MARK 1
#else
#define MARK 0
#endif
value caml_cache_public_method (value meths, value tag, value *cache)
{
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  *cache = (li-3)*sizeof(value) + MARK;
  return Field (meths, li-1);
}

value caml_cache_public_method2 (value *meths, value tag, value *cache)
{
  value ofs = *cache & meths[1];
  if (*(value*)(((char*)(meths+3)) + ofs - MARK) == tag)
    return *(value*)(((char*)(meths+2)) + ofs - MARK);
  {
    int li = 3, hi = meths[0], mi;
    while (li < hi) {
      mi = ((li+hi) >> 1) | 1;
      if (tag < meths[mi]) hi = mi-2;
      else li = mi;
    }
    *cache = (li-3)*sizeof(value) + MARK;
    return meths[li-1];
  }
}
#endif /*CAML_JIT*/

static value oo_last_id = Val_int(0);

CAMLprim value caml_set_oo_id (value obj) {
  Op_val(obj)[1] = oo_last_id;
  oo_last_id += 2;
  return obj;
}
