/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

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
#include "caml/signals.h"

CAMLprim value caml_obj_tag(value arg)
{
  if (Is_long (arg)){
    return Val_int (1000);   /* int_tag */
  }else if ((long) arg & (sizeof (value) - 1)){
    return Val_int (1002);   /* unaligned_tag */
  }else if (Is_in_value_area (arg)){
    return Val_int(Tag_val(arg));
  }else{
    return Val_int (1001);   /* out_of_heap_tag */
  }
}

CAMLprim value caml_obj_set_tag (value arg, value new_tag)
{
  Tag_val (arg) = Int_val (new_tag);
  return Val_unit;
}

CAMLprim value caml_obj_raw_field(value arg, value pos)
{
  /* Represent field contents as a native integer */
  return caml_copy_nativeint((intnat) Field(arg, Long_val(pos)));
}

CAMLprim value caml_obj_set_raw_field(value arg, value pos, value bits)
{
  Field(arg, Long_val(pos)) = (value) Nativeint_val(bits);
  return Val_unit;
}

CAMLprim value caml_obj_make_forward (value blk, value fwd)
{
  caml_modify(&Field(blk, 0), fwd);
  Tag_val (blk) = Forward_tag;
  return Val_unit;
}

/* [size] is a value encoding a number of blocks */
CAMLprim value caml_obj_block(value tag, value size)
{
  value res;
  mlsize_t sz;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);

  /* When [tg < No_scan_tag], [caml_alloc] returns an object whose fields are
   * initialised to [Val_unit]. Otherwise, the fields are uninitialised. We aim
   * to avoid inconsistent states in other cases, on a best-effort basis --
   * by default there is no initialization. */
  switch (tg) {
  default: {
      res = caml_alloc(sz, tg);
      break;
  }
  case Abstract_tag:
  case Double_tag:
  case Double_array_tag: {
    /* In these cases, the initial content is irrelevant,
       no specific initialization needed. */
    res = caml_alloc(sz, tg);
    break;
  }
  case Closure_tag: {
    /* [Closure_tag] is below [no_scan_tag], but closures have more
       structure with in particular a "closure information" that
       indicates where the environment starts. We initialize this to
       a sane value, as it may be accessed by runtime functions. */
    /* Closinfo_val is the second field, so we need size at least 2 */
    if (sz < 2) caml_invalid_argument ("Obj.new_block");
    res = caml_alloc(sz, tg);
    Closinfo_val(res) = Make_closinfo(0, 2); /* does not allocate */
    break;
  }
  case String_tag: {
    /* For [String_tag], the initial content does not matter. However,
       the length of the string is encoded using the last byte of the
       block. For this reason, the blocks with [String_tag] cannot be
       of size [0]. We initialise the last byte to [0] such that the
       length returned by [String.length] and [Bytes.length] is
       a non-negative number. */
    if (sz == 0) caml_invalid_argument ("Obj.new_block");
    res = caml_alloc(sz, tg);
    Field (res, sz - 1) = 0;
    break;
  }
  case Custom_tag: {
    /* It is difficult to correctly use custom objects allocated
       through [Obj.new_block], so we disallow it completely. The
       first field of a custom object must contain a valid pointer to
       a block of custom operations. Without initialisation, hashing,
       finalising or serialising this custom object will lead to
       crashes.  See #9513 for more details. */
    caml_invalid_argument ("Obj.new_block");
  }
  }

  return res;
}

CAMLprim value caml_obj_with_tag(value new_tag_v, value arg)
{
  CAMLparam2 (new_tag_v, arg);
  CAMLlocal1 (res);
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  tg = (tag_t)Long_val(new_tag_v);
  if (sz == 0) CAMLreturn (Atom(tg));
  if (tg >= No_scan_tag) {
    res = caml_alloc(sz, tg);
    memcpy(Bp_val(res), Bp_val(arg), sz * sizeof(value));
  } else if (sz <= Max_young_wosize) {
    res = caml_alloc_small(sz, tg);
    for (i = 0; i < sz; i++) Field(res, i) = Field(arg, i);
  } else {
    res = caml_alloc_shr(sz, tg);
    /* It is safe to use [caml_initialize] even if [tag == Closure_tag]
       and some of the "values" being copied are actually code pointers.
       That's because the new "value" does not point to the minor heap. */
    for (i = 0; i < sz; i++) caml_initialize(&Field(res, i), Field(arg, i));
    /* Give gc a chance to run, and run memprof callbacks */
    caml_process_pending_actions();
  }
  CAMLreturn (res);
}

CAMLprim value caml_obj_dup(value arg)
{
  return caml_obj_with_tag(Val_long(Tag_val(arg)), arg);
}

/* Shorten the given block to the given size and return void.
   Raise Invalid_argument if the given size is less than or equal
   to 0 or greater than the current size.

   algorithm:
   Change the length field of the header.  Make up a black object
   with the leftover part of the object: this is needed in the major
   heap and harmless in the minor heap. The object cannot be white
   because there may still be references to it in the ref table. By
   using a black object we ensure that the ref table will be emptied
   before the block is reallocated (since there must be a minor
   collection within each major cycle).

   [newsize] is a value encoding a number of fields (words, except
   for float arrays on 32-bit architectures).
*/
CAMLprim value caml_obj_truncate (value v, value newsize)
{
  mlsize_t new_wosize = Long_val (newsize);
  header_t hd = Hd_val (v);
  tag_t tag = Tag_hd (hd);
  color_t color = Color_hd (hd);
  color_t frag_color = Is_young(v) ? 0 : Caml_black;
  mlsize_t wosize = Wosize_hd (hd);
  mlsize_t i;

  if (tag == Double_array_tag) new_wosize *= Double_wosize;  /* PR#2520 */

  if (new_wosize <= 0 || new_wosize > wosize){
    caml_invalid_argument ("Obj.truncate");
  }
  if (new_wosize == wosize) return Val_unit;
  /* PR#2400: since we're about to lose our references to the elements
     beyond new_wosize in v, erase them explicitly so that the GC
     can darken them as appropriate. */
  if (tag < No_scan_tag) {
    for (i = new_wosize; i < wosize; i++){
      caml_modify(&Field(v, i), Val_unit);
#ifdef DEBUG
      Field (v, i) = Debug_free_truncate;
#endif
    }
  }
  /* We must use an odd tag for the header of the leftovers so it does not
     look like a pointer because there may be some references to it in
     ref_table. */
  Field (v, new_wosize) =
    Make_header (Wosize_whsize (wosize-new_wosize), Abstract_tag, frag_color);
  Hd_val (v) =
    Make_header_with_profinfo (new_wosize, tag, color, Profinfo_val(v));
  return Val_unit;
}

CAMLprim value caml_obj_add_offset (value v, value offset)
{
  return v + (unsigned long) Int32_val (offset);
}

/* The following function is used in stdlib/lazy.ml.
   It is not written in OCaml because it must be atomic with respect
   to the GC.
 */

CAMLprim value caml_lazy_make_forward (value v)
{
  CAMLparam1 (v);
  CAMLlocal1 (res);

  res = caml_alloc_small (1, Forward_tag);
  Field (res, 0) = v;
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

static value oo_last_id = Val_int(0);

CAMLprim value caml_set_oo_id (value obj) {
  Field(obj, 1) = oo_last_id;
  oo_last_id += 2;
  return obj;
}

CAMLprim value caml_fresh_oo_id (value v) {
  v = oo_last_id;
  oo_last_id += 2;
  return v;
}

CAMLprim value caml_int_as_pointer (value n) {
  return n - 1;
}

/* Compute how many words in the heap are occupied by blocks accessible
   from a given value */

#define ENTRIES_PER_QUEUE_CHUNK 4096
struct queue_chunk {
  struct queue_chunk *next;
  value entries[ENTRIES_PER_QUEUE_CHUNK];
};
