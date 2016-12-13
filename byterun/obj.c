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
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/platform.h"

/* all uses of this are bugs */
CAMLprim value caml_static_alloc(value size)
{
  return (value) caml_stat_alloc((asize_t) Long_val(size));
}

CAMLprim value caml_static_free(value blk)
{
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
    for (i = 0; i < sz; i++) caml_initialize_field(res, i, Field(arg, i));
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

CAMLprim value caml_obj_compare_and_swap (value v, value f, value oldv, value newv)
{
  int res = caml_atomic_cas_field(v, Int_val(f), oldv, newv);
  caml_check_urgent_gc(Val_unit);
  return Val_int(res);
}

/* caml_promote_to(obj, upto) promotes obj to be as least as shared as upto */
CAMLprim value caml_obj_promote_to (value obj, value upto)
{
  if (Is_block(upto) && Is_minor(upto)) {
    /* upto is local, obj is already as shared as upto is */
    return obj;
  } else {
    return caml_promote(caml_domain_self(), obj);
  }
}

CAMLprim value caml_obj_is_shared (value obj)
{
  return Val_int(Is_long(obj) || !Is_minor(obj));
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
  caml_initialize_field (res, 0, v);
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

/* Allocate OO ids in chunks, to avoid contention */
#define Id_chunk 1024

static atomic_uintnat oo_next_id;
static __thread uintnat oo_next_id_local;

CAMLprim value caml_set_oo_id (value obj) {
  if (oo_next_id_local % Id_chunk == 0) {
    oo_next_id_local = atomic_fetch_add(&oo_next_id, Id_chunk);
  }
  Op_val(obj)[1] = Val_long(oo_next_id_local++);
  return obj;
}

CAMLprim value caml_int_as_pointer (value n) {
  return n - 1;
}
