/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/weak.h"

/* Pointers into the minor heap.
   [caml_young_base]
       The [malloc] block that contains the heap.
   [caml_young_start] ... [caml_young_end]
       The whole range of the minor heap: all young blocks are inside
       this interval.
   [caml_young_alloc_start]...[caml_young_alloc_end]
       The allocation arena: newly-allocated blocks are carved from
       this interval.
   [caml_young_alloc_mid] is the mid-point of this interval.
   [caml_young_ptr], [caml_young_trigger], [caml_young_limit]
       These pointers are all inside the allocation arena.
       - [caml_young_ptr] is where the next allocation will take place.
       - [caml_young_trigger] is how far we can allocate before triggering
         [caml_gc_dispatch]. Currently, it is either [caml_young_alloc_start]
         or the mid-point of the allocation arena.
       - [caml_young_limit] is the pointer that is compared to
         [caml_young_ptr] for allocation. It is either
         [caml_young_alloc_end] if a signal is pending and we are in
         native code, or [caml_young_trigger].
*/

asize_t caml_minor_heap_size;  /* bytes */
static void *caml_young_base = NULL;
CAMLexport value *caml_young_start = NULL, *caml_young_end = NULL;
CAMLexport value *caml_young_alloc_start = NULL,
                 *caml_young_alloc_mid = NULL,
                 *caml_young_alloc_end = NULL;
CAMLexport value *caml_young_ptr = NULL, *caml_young_limit = NULL;
CAMLexport value *caml_young_trigger = NULL;

CAMLexport struct caml_ref_table
  caml_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0},
  caml_weak_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};

int caml_in_minor_collection = 0;

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  value **new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (value **) caml_stat_alloc ((tbl->size + tbl->reserve)
                                          * sizeof (value *));
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = new_table;
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + tbl->size + tbl->reserve;
}

static void reset_table (struct caml_ref_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

static void clear_table (struct caml_ref_table *tbl)
{
    tbl->ptr = tbl->base;
    tbl->limit = tbl->threshold;
}

void caml_set_minor_heap_size (asize_t bsz)
{
  char *new_heap;
  void *new_heap_base;

  Assert (bsz >= Bsize_wsize(Minor_heap_min));
  Assert (bsz <= Bsize_wsize(Minor_heap_max));
  Assert (bsz % sizeof (value) == 0);
  if (caml_young_ptr != caml_young_alloc_end){
    CAML_INSTR_INT ("force_minor/set_minor_heap_size@", 1);
    caml_requested_minor_gc = 0;
    caml_young_trigger = caml_young_alloc_mid;
    caml_young_limit = caml_young_trigger;
    caml_empty_minor_heap ();
  }
  CAMLassert (caml_young_ptr == caml_young_alloc_end);
#ifdef MMAP_INTERVAL
  {
    static uintnat minor_heap_mapped_bsz = 0;
    uintnat new_mapped_bsz;
    new_mapped_bsz = Round_mmap_size (bsz);
    void *block;

    CAMLassert (caml_young_start != NULL);
    if (new_mapped_bsz > minor_heap_mapped_bsz){
      uintnat addbsz = new_mapped_bsz - minor_heap_mapped_bsz;
      new_heap = (char *) caml_young_start - addbsz;
      block = caml_mmap_heap (new_heap, addbsz, PROT_READ | PROT_WRITE,
                              MAP_FIXED);
      if (block != new_heap){
        if (minor_heap_mapped_bsz == 0){
          caml_fatal_error ("cannot initialize minor heap: mmap failed\n");
        }else{
          caml_raise_out_of_memory ();
        }
      }
      new_heap_base = new_heap;
    }else if (new_mapped_bsz < minor_heap_mapped_bsz){
      uintnat subbsz = minor_heap_mapped_bsz - new_mapped_bsz;
      (void) caml_mmap_heap (caml_young_start, subbsz, PROT_NONE,
                             MAP_FIXED | MAP_NORESERVE);
      new_heap_base = new_heap = (char *) caml_young_start + subbsz;
    }else{
      new_heap_base = new_heap = caml_young_base;
    }
  }
#else
  new_heap = caml_aligned_malloc(bsz, 0, &new_heap_base);
  if (new_heap == NULL) caml_raise_out_of_memory();
  if (caml_page_table_add(In_young, new_heap, new_heap + bsz) != 0)
    caml_raise_out_of_memory();

  if (caml_young_start != NULL){
    caml_page_table_remove(In_young, caml_young_start, caml_young_end);
    free (caml_young_base);
  }
#endif
  caml_young_base = new_heap_base;
  caml_young_start = (value *) new_heap;
  caml_young_end = (value *) (new_heap + bsz);
  caml_young_alloc_start = caml_young_start;
  caml_young_alloc_mid = caml_young_alloc_start + Wsize_bsize (bsz) / 2;
  caml_young_alloc_end = caml_young_end;
  caml_young_trigger = caml_young_alloc_start;
  caml_young_limit = caml_young_trigger;
  caml_young_ptr = caml_young_alloc_end;
  caml_minor_heap_size = bsz;

  reset_table (&caml_ref_table);
  reset_table (&caml_weak_ref_table);
}

static value oldify_todo_list = 0;

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */

void caml_oldify_one (value v, value *p)
{
  value result;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert ((value *) Hp_val (v) >= caml_young_ptr);
    hd = Hd_val (v);
    if (hd == 0){         /* If already forwarded */
      *p = Field (v, 0);  /*  then forward pointer is first field. */
    }else{
      tag = Tag_hd (hd);
      if (tag < Infix_tag){
        value field0;

        sz = Wosize_hd (hd);
        result = caml_alloc_shr (sz, tag);
        *p = result;
        field0 = Field (v, 0);
        Hd_val (v) = 0;            /* Set forward flag */
        Field (v, 0) = result;     /*  and forward pointer. */
        if (sz > 1){
          Field (result, 0) = field0;
          Field (result, 1) = oldify_todo_list;    /* Add this block */
          oldify_todo_list = v;                    /*  to the "to do" list. */
        }else{
          Assert (sz == 1);
          p = &Field (result, 0);
          v = field0;
          goto tail_call;
        }
      }else if (tag >= No_scan_tag){
        sz = Wosize_hd (hd);
        result = caml_alloc_shr (sz, tag);
        for (i = 0; i < sz; i++) Field (result, i) = Field (v, i);
        Hd_val (v) = 0;            /* Set forward flag */
        Field (v, 0) = result;     /*  and forward pointer. */
        *p = result;
      }else if (tag == Infix_tag){
        mlsize_t offset = Infix_offset_hd (hd);
        caml_oldify_one (v - offset, p);   /* Cannot recurse deeper than 1. */
        *p += offset;
      }else{
        value f = Forward_val (v);
        tag_t ft = 0;
        int vv = 1;

        Assert (tag == Forward_tag);
        if (Is_block (f)){
          if (Is_young (f)){
            vv = 1;
            ft = Tag_val (Hd_val (f) == 0 ? Field (f, 0) : f);
          }else{
            vv = Is_in_value_area(f);
            if (vv){
              ft = Tag_val (f);
            }
          }
        }
        if (!vv || ft == Forward_tag || ft == Lazy_tag || ft == Double_tag){
          /* Do not short-circuit the pointer.  Copy as a normal block. */
          Assert (Wosize_hd (hd) == 1);
          result = caml_alloc_shr (1, Forward_tag);
          *p = result;
          Hd_val (v) = 0;             /* Set (GC) forward flag */
          Field (v, 0) = result;      /*  and forward pointer. */
          p = &Field (result, 0);
          v = f;
          goto tail_call;
        }else{
          v = f;                        /* Follow the forwarding */
          goto tail_call;               /*  then oldify. */
        }
      }
    }
  }else{
    *p = v;
  }
}

/* Finish the work that was put off by [caml_oldify_one].
   Note that [caml_oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
void caml_oldify_mopup (void)
{
  value v, new_v, f;
  mlsize_t i;

  while (oldify_todo_list != 0){
    v = oldify_todo_list;                /* Get the head. */
    Assert (Hd_val (v) == 0);            /* It must be forwarded. */
    new_v = Field (v, 0);                /* Follow forward pointer. */
    oldify_todo_list = Field (new_v, 1); /* Remove from list. */

    f = Field (new_v, 0);
    if (Is_block (f) && Is_young (f)){
      caml_oldify_one (f, &Field (new_v, 0));
    }
    for (i = 1; i < Wosize_val (new_v); i++){
      f = Field (v, i);
      if (Is_block (f) && Is_young (f)){
        caml_oldify_one (f, &Field (new_v, i));
      }else{
        Field (new_v, i) = f;
      }
    }
  }
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/
void caml_empty_minor_heap (void)
{
  value **r;
  uintnat prev_alloc_words;

  if (caml_young_ptr != caml_young_alloc_end){
    if (caml_minor_gc_begin_hook != NULL) (*caml_minor_gc_begin_hook) ();
    CAML_INSTR_SETUP (tmr, "minor");
    prev_alloc_words = caml_allocated_words;
    caml_in_minor_collection = 1;
    caml_gc_message (0x02, "<", 0);
    caml_oldify_local_roots();
    CAML_INSTR_TIME (tmr, "minor/local_roots");
    for (r = caml_ref_table.base; r < caml_ref_table.ptr; r++){
      caml_oldify_one (**r, *r);
    }
    CAML_INSTR_TIME (tmr, "minor/ref_table");
    caml_oldify_mopup ();
    CAML_INSTR_TIME (tmr, "minor/copy");
    for (r = caml_weak_ref_table.base; r < caml_weak_ref_table.ptr; r++){
      if (Is_block (**r) && Is_young (**r)){
        if (Hd_val (**r) == 0){
          **r = Field (**r, 0);
        }else{
          **r = caml_weak_none;
        }
      }
    }
    CAML_INSTR_TIME (tmr, "minor/update_weak");
    CAMLassert (caml_young_ptr >= caml_young_alloc_start);
    caml_stat_minor_words += caml_young_alloc_end - caml_young_ptr;
    caml_gc_clock += (double) (caml_young_alloc_end - caml_young_ptr)
                     / Wsize_bsize (caml_minor_heap_size);
    caml_young_ptr = caml_young_alloc_end;
    clear_table (&caml_ref_table);
    clear_table (&caml_weak_ref_table);
    caml_gc_message (0x02, ">", 0);
    caml_in_minor_collection = 0;
    caml_final_empty_young ();
    CAML_INSTR_TIME (tmr, "minor/finalized");
    caml_stat_promoted_words += caml_allocated_words - prev_alloc_words;
    CAML_INSTR_INT ("minor/promoted#", caml_allocated_words - prev_alloc_words);
    ++ caml_stat_minor_collections;
    if (caml_minor_gc_end_hook != NULL) (*caml_minor_gc_end_hook) ();
  }else{
    caml_final_empty_young ();
  }
#ifdef DEBUG
  {
    value *p;
    for (p = caml_young_alloc_start; p < caml_young_alloc_end; ++p){
      *p = Debug_free_minor;
    }
  }
#endif
}

#ifdef CAML_INSTR
extern uintnat caml_instr_alloc_jump;
#endif

/* Do a minor collection or a slice of major collection, call finalisation
   functions, etc.
   Leave enough room in the minor heap to allocate at least one object.
*/
CAMLexport void caml_gc_dispatch (void)
{
  value *trigger = caml_young_trigger; /* save old value of trigger */
#ifdef CAML_INSTR
  CAML_INSTR_SETUP(tmr, "dispatch");
  CAML_INSTR_TIME (tmr, "overhead");
  CAML_INSTR_INT ("alloc/jump#", caml_instr_alloc_jump);
  caml_instr_alloc_jump = 0;
#endif

  if (trigger == caml_young_alloc_start || caml_requested_minor_gc){
    /* The minor heap is full, we must do a minor collection. */
    /* reset the pointers first because the end hooks might allocate */
    caml_requested_minor_gc = 0;
    caml_young_trigger = caml_young_alloc_mid;
    caml_young_limit = caml_young_trigger;
    caml_empty_minor_heap ();
    if (caml_gc_phase == Phase_idle) caml_major_collection_slice (-1);
    CAML_INSTR_TIME (tmr, "dispatch/minor");

    caml_final_do_calls ();
    CAML_INSTR_TIME (tmr, "dispatch/finalizers");

    while (caml_young_ptr - caml_young_alloc_start < Max_young_whsize){
      /* The finalizers or the hooks have filled up the minor heap, we must
         repeat the minor collection. */
      caml_requested_minor_gc = 0;
      caml_young_trigger = caml_young_alloc_mid;
      caml_young_limit = caml_young_trigger;
      caml_empty_minor_heap ();
      if (caml_gc_phase == Phase_idle) caml_major_collection_slice (-1);
      CAML_INSTR_TIME (tmr, "dispatch/finalizers_minor");
    }
  }
  if (trigger != caml_young_alloc_start || caml_requested_major_slice){
    /* The minor heap is half-full, do a major GC slice. */
    caml_requested_major_slice = 0;
    caml_young_trigger = caml_young_alloc_start;
    caml_young_limit = caml_young_trigger;
    caml_major_collection_slice (-1);
    CAML_INSTR_TIME (tmr, "dispatch/major");
  }
}

/* For backward compatibility with Lablgtk: do a minor collection to
   ensure that the minor heap is empty.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_requested_minor_gc = 1;
  caml_gc_dispatch ();
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  if (caml_requested_major_slice || caml_requested_minor_gc){
    CAML_INSTR_INT ("force_minor/check_urgent_gc@", 1);
    caml_gc_dispatch();
  }
  CAMLreturn (extra_root);
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{                                           Assert (tbl->ptr == tbl->limit);
                                            Assert (tbl->limit <= tbl->end);
                                      Assert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    caml_alloc_table (tbl, caml_minor_heap_size / sizeof (value) / 8, 256);
  }else if (tbl->limit == tbl->threshold){
    CAML_INSTR_INT ("request_minor/realloc_ref_table@", 1);
    caml_gc_message (0x08, "ref_table threshold crossed\n", 0);
    tbl->limit = tbl->end;
    caml_request_minor_gc ();
  }else{
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;
    CAMLassert (caml_requested_minor_gc);

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * sizeof (value *);
    caml_gc_message (0x08, "Growing ref_table to %"
                           ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
                     (intnat) sz/1024);
    tbl->base = (value **) realloc ((char *) tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("Fatal error: ref_table overflow\n");
    }
    tbl->end = tbl->base + tbl->size + tbl->reserve;
    tbl->threshold = tbl->base + tbl->size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}
