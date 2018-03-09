/**************************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*     en Automatique.                                                    */
/*                                                                     */
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
#include "caml/platform.h"

/* all uses of this are bugs */
#include "spacetime.h"

/* [size] is a value encoding a number of bytes */
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

/* unused since GPR#427 */
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

/* [size] is a value encoding a number of blocks */
CAMLprim value caml_obj_block(value tag, value size)
{
  return caml_alloc(Long_val(size), Long_val(tag));
}

/* Spacetime profiling assumes that this function is only called from OCaml. */
CAMLprim value caml_obj_dup(value arg)
{
  CAMLparam1 (arg);
  CAMLlocal2 (res, x);
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  if (sz == 0) CAMLreturn (arg);
  tg = Tag_val(arg);
  if (tg >= No_scan_tag) {
    res = caml_alloc(sz, tg);
    memcpy(Bp_val(res), Bp_val(arg), sz * sizeof(value));
  } else if (sz <= Max_young_wosize) {
    uintnat profinfo;
    Get_my_profinfo_with_cached_backtrace(profinfo, sz);
    res = caml_alloc_small_with_my_or_given_profinfo(sz, tg, profinfo);
    for (i = 0; i < sz; i++) {
      caml_read_field(arg, i, &x);
      caml_initialize_field(res, i, x);
    }
  } else {
    res = caml_alloc(sz, tg);
    for (i = 0; i < sz; i++) {
      caml_read_field(arg, i, &x);
      caml_initialize_field(res, i, x);
    }
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
  value meths = Field_imm(obj, 0);
  int li = 3, hi = Field_imm(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field_imm(meths,mi)) hi = mi-2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return (tag == Field_imm(meths,li) ? Field_imm (meths, li-1) : 0);
}

/* Allocate OO ids in chunks, to avoid contention */
#define Id_chunk 1024

static atomic_uintnat oo_next_id;

CAMLprim value caml_fresh_oo_id (value v) {
  if (Caml_state->oo_next_id_local % Id_chunk == 0) {
    Caml_state->oo_next_id_local =
      atomic_fetch_add(&oo_next_id, Id_chunk);
  }
  v = Val_long(Caml_state->oo_next_id_local++);
  return v;
}

CAMLprim value caml_set_oo_id (value obj) {
  value v = Val_unit;
  Op_val(obj)[1] = caml_fresh_oo_id(v);
  return obj;
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


CAMLprim value caml_obj_reachable_words(value v)
{
  return Val_int(0);
#if 0
  static struct queue_chunk first_chunk;
  struct queue_chunk *read_chunk, *write_chunk;
  int write_pos, read_pos, i;

  intnat size = 0;
  header_t hd;
  mlsize_t sz;

  if (Is_long(v) || !Is_in_heap_or_young(v)) return Val_int(0);
  if (Tag_hd(Hd_val(v)) == Infix_tag) v -= Infix_offset_hd(Hd_val(v));
  hd = Hd_val(v);
  sz = Wosize_hd(hd);

  read_chunk = write_chunk = &first_chunk;
  read_pos = 0;
  write_pos = 1;
  write_chunk->entries[0] = v | Colornum_hd(hd);
  Hd_val(v) = Bluehd_hd(hd);

  /* We maintain a queue of "interesting" blocks that have been seen.
     An interesting block is a block in the heap which does not
     represent an infix pointer. Infix pointers are normalized to the
     beginning of their block.  Blocks in the static data area are excluded.

     The function maintains a queue of block pointers.  Concretely,
     the queue is stored as a linked list of chunks, each chunk
     holding a number of pointers to interesting blocks.  Initially,
     it contains only the "root" value.  The first chunk of the queue
     is allocated statically.  More chunks can be allocated as needed
     and released before this function exits.

     When a block is inserted in the queue, it is marked as blue.
     This mark is used to avoid a second visit of the same block.
     The real color is stored in the last 2 bits of the pointer in the
     queue.  (Same technique as in extern.c.)

     Note: we make the assumption that there is no pointer
     from the static data area to the heap.
  */

  /* First pass: mark accessible blocks and compute their total size */
  while (read_pos != write_pos || read_chunk != write_chunk) {
    /* Pop the next element from the queue */
    if (read_pos == ENTRIES_PER_QUEUE_CHUNK) {
      read_pos = 0;
      read_chunk = read_chunk->next;
    }
    v = read_chunk->entries[read_pos++] & ~3;

    hd = Hd_val(v);
    sz = Wosize_hd(hd);

    size += Whsize_wosize(sz);

    if (Tag_hd(hd) < No_scan_tag) {
      /* Push the interesting fields on the queue */
      for (i = 0; i < sz; i++) {
        value v2 = Field(v, i);
        if (Is_block(v2) && Is_in_heap_or_young(v2)) {
          if (Tag_hd(Hd_val(v2)) == Infix_tag){
            v2 -= Infix_offset_hd(Hd_val(v2));
          }
          hd = Hd_val(v2);
          if (Color_hd(hd) != Caml_blue) {
            if (write_pos == ENTRIES_PER_QUEUE_CHUNK) {
              struct queue_chunk *new_chunk =
                malloc(sizeof(struct queue_chunk));
              if (new_chunk == NULL) {
                size = (-1);
                goto release;
              }
              write_chunk->next = new_chunk;
              write_pos = 0;
              write_chunk = new_chunk;
            }
            write_chunk->entries[write_pos++] = v2 | Colornum_hd(hd);
            Hd_val(v2) = Bluehd_hd(hd);
          }
        }
      }
    }
  }

  /* Second pass: restore colors and free extra queue chunks */
 release:
  read_pos = 0;
  read_chunk = &first_chunk;
  while (read_pos != write_pos || read_chunk != write_chunk) {
    color_t colornum;
    if (read_pos == ENTRIES_PER_QUEUE_CHUNK) {
      struct queue_chunk *prev = read_chunk;
      read_pos = 0;
      read_chunk = read_chunk->next;
      if (prev != &first_chunk) free(prev);
    }
    v = read_chunk->entries[read_pos++];
    colornum = v & 3;
    v &= ~3;
    Hd_val(v) = Coloredhd_hd(Hd_val(v), colornum);
  }
  if (read_chunk != &first_chunk) free(read_chunk);

  if (size < 0)
    caml_raise_out_of_memory();
  return Val_int(size);
#endif /* 0 */
}
