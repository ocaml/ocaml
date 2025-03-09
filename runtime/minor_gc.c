/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
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

#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "caml/config.h"
#include "caml/custom.h"
#include "caml/domain.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/roots.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/startup_aux.h"
#include "caml/weak.h"

extern value caml_ephe_none; /* See weak.c */
struct generic_table CAML_TABLE_STRUCT(char);

CAMLexport atomic_uintnat caml_minor_collections_count;
CAMLexport atomic_uintnat caml_major_slice_epoch;

static caml_plat_barrier minor_gc_end_barrier = CAML_PLAT_BARRIER_INITIALIZER;

static atomic_uintnat caml_minor_cycles_started = 0;

/* [sz] and [rsv] are numbers of entries */
static void alloc_generic_table (struct generic_table *tbl, asize_t sz,
                                 asize_t rsv, asize_t element_size)
{
  void *new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (void *) caml_stat_alloc_noexc((tbl->size + tbl->reserve) *
                                             element_size);
  if (new_table == NULL) caml_fatal_error ("not enough memory");
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = new_table;
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size * element_size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + (tbl->size + tbl->reserve) * element_size;
}

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  alloc_generic_table ((struct generic_table *) tbl, sz, rsv, sizeof (value *));
}

static void reset_table (struct generic_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

static void clear_table (struct generic_table *tbl)
{
    tbl->ptr = tbl->base;
    tbl->limit = tbl->threshold;
}

struct caml_minor_tables* caml_alloc_minor_tables(void)
{
  struct caml_minor_tables *r =
      caml_stat_alloc_noexc(sizeof(struct caml_minor_tables));
  if(r != NULL)
    memset(r, 0, sizeof(*r));
  return r;
}

static void reset_minor_tables(struct caml_minor_tables* r)
{
  reset_table((struct generic_table *)&r->major_ref);
  reset_table((struct generic_table *)&r->ephe_ref);
  reset_table((struct generic_table *)&r->custom);
}

void caml_free_minor_tables(struct caml_minor_tables* r)
{
  CAMLassert(r->major_ref.ptr == r->major_ref.base);

  reset_minor_tables(r);
  caml_stat_free(r);
}

#ifdef DEBUG
extern int caml_debug_is_minor(value val) {
  return Is_young(val);
}

extern int caml_debug_is_major(value val) {
  return Is_block(val) && !Is_young(val);
}
#endif

void caml_set_minor_heap_size (asize_t wsize)
{
  caml_domain_state* domain_state = Caml_state;
  struct caml_minor_tables *r = domain_state->minor_tables;

  if (domain_state->young_ptr != domain_state->young_end) {
    CAML_EV_COUNTER (EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE, 1);
    caml_minor_collection();
  }

  if(caml_reallocate_minor_heap(wsize) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap");
  }

  reset_minor_tables(r);
}

/*****************************************************************************/

struct oldify_state {
  value todo_list;
  uintnat live_bytes;
  caml_domain_state* domain;
};

static value alloc_shared(caml_domain_state* d,
                          mlsize_t wosize, tag_t tag, reserved_t reserved)
{
  void* mem = caml_shared_try_alloc(d->shared_heap, wosize, tag,
                                    reserved);
  d->allocated_words += Whsize_wosize(wosize);
  if (mem == NULL) {
    caml_fatal_error("allocation failure during minor GC");
  }
  return Val_hp(mem);
}

/* in progress updates are zeros except for the lowest color bit set to 1
   that is, reserved == wosize == tag == 0, color == 1 */
#define In_progress_update_val Make_header(0, 0, 1 << HEADER_COLOR_SHIFT)
#define Is_update_in_progress(hd) ((hd) == In_progress_update_val)

static void spin_on_header(value v) {
  SPIN_WAIT {
    if (atomic_load(Hp_atomic_val(v)) == 0)
      return;
  }
}

CAMLno_tsan_for_perf
Caml_inline header_t get_header_val(value v) {
  header_t hd = atomic_load_acquire(Hp_atomic_val(v));
  if (!Is_update_in_progress(hd))
    return hd;

  spin_on_header(v);
  return 0;
}

header_t caml_get_header_val(value v) {
  return get_header_val(v);
}


static int try_update_object_header(value v, volatile value *p, value result,
                                    mlsize_t infix_offset) {
  int success = 0;

  if( caml_domain_alone() ) {
    *Hp_val (v) = 0;
    Field(v, 0) = result;
    success = 1;
  } else {
    header_t hd = atomic_load(Hp_atomic_val(v));
    if( hd == 0 ) {
      /* in this case this has been updated by another domain, throw away result
         and return the one in the object */
      result = Field(v, 0);
    } else if( Is_update_in_progress(hd) ) {
      /* here we've caught a domain in the process of moving a minor heap object
         we need to wait for it to finish */
      spin_on_header(v);
      /* Also throw away result and use the one from the other domain */
      result = Field(v, 0);
    } else {
      /* Here the header is neither zero nor an in-progress update */
      header_t desired_hd = In_progress_update_val;
      if( atomic_compare_exchange_strong(Hp_atomic_val(v), &hd, desired_hd) ) {
        /* Success. Now we can write the forwarding pointer. */
        atomic_store_relaxed(Op_atomic_val(v), result);
        /* And update header ('release' ensures after update of fwd pointer) */
        atomic_store_release(Hp_atomic_val(v), 0);
        /* Let the caller know we were responsible for the update */
        success = 1;
      } else {
        /* Updated by another domain. Spin for that update to complete and
           then throw away the result and use the one from the other domain. */
        spin_on_header(v);
        result = Field(v, 0);
      }
    }
  }

  *p = result + infix_offset;
  return success;
}

/* oldify_one is a no-op outside the minor heap. */
static scanning_action_flags oldify_scanning_flags =
  SCANNING_ONLY_YOUNG_VALUES;

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */
static void oldify_one (void* st_v, value v, volatile value *p)
{
  struct oldify_state* st = st_v;
  value result;
  header_t hd;
  mlsize_t sz;
  mlsize_t infix_offset;
  tag_t tag;

  tail_call:
  if (!(Is_block(v) && Is_young(v))) {
    /* not a minor block */
    *p = v;
    return;
  }

  infix_offset = 0;
  do {
    hd = get_header_val(v);
    if (hd == 0) {
      /* already forwarded, another domain is likely working on this. */
      *p = Field(v, 0) + infix_offset;
      return;
    }
    tag = Tag_hd (hd);
    if (tag == Infix_tag) {
      /* Infix header, retry with the real block */
      CAMLassert (infix_offset == 0);
      infix_offset = Infix_offset_hd (hd);
      CAMLassert(infix_offset > 0);
      v -= infix_offset;
    }
  } while (tag == Infix_tag);

  if (tag == Cont_tag) {
    value stack_value = Field(v, 0);
    CAMLassert(Wosize_hd(hd) == 2);
    CAMLassert(infix_offset == 0);
    result = alloc_shared(st->domain, 2, Cont_tag, Reserved_hd(hd));
    if( try_update_object_header(v, p, result, 0) ) {
      struct stack_info* stk = Ptr_val(stack_value);
      Field(result, 0) = stack_value;
      Field(result, 1) = Field(v, 1);
      if (stk != NULL) {
        caml_scan_stack(&oldify_one, oldify_scanning_flags, st,
                        stk, 0);
      }
    }
    else
    {
      /* Conflict - fix up what we allocated on the major heap */
      *Hp_val(result) = Make_header(1, No_scan_tag,
                                    caml_global_heap_state.MARKED);
      #ifdef DEBUG
      Field(result, 0) = Val_long(1);
      Field(result, 1) = Val_long(1);
      #endif
    }
  } else if (tag < Infix_tag) {
    value field0;
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared(st->domain, sz, tag, Reserved_hd(hd));
    field0 = Field(v, 0);
    if( try_update_object_header(v, p, result, infix_offset) ) {
      if (sz > 1){
        Field(result, 0) = field0;
        Field(result, 1) = st->todo_list;
        st->todo_list = v;
      } else {
        CAMLassert (sz == 1);
        p = Op_val(result);
        v = field0;
        goto tail_call;
      }
    } else {
      /* Conflict - fix up what we allocated on the major heap */
      *Hp_val(result) = Make_header(sz, No_scan_tag,
                                    caml_global_heap_state.MARKED);
      #ifdef DEBUG
      {
        for (int c = 0; c < sz; c++) {
          Field(result, c) = Val_long(1);
        }
      }
      #endif
    }

  } else if (tag >= No_scan_tag) {
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared(st->domain, sz, tag, Reserved_hd(hd));
    for (mlsize_t i = 0; i < sz; i++) {
      Field(result, i) = Field(v, i);
    }
    CAMLassert (infix_offset == 0);
    if( !try_update_object_header(v, p, result, 0) ) {
      /* Conflict */
      *Hp_val(result) = Make_header(sz, No_scan_tag,
                                    caml_global_heap_state.MARKED);
      #ifdef DEBUG
      for(mlsize_t i = 0; i < sz; i++) {
        Field(result, i) = Val_long(1);
      }
      #endif
    }
  } else {
    value f;
    tag_t ft;
    CAMLassert (tag == Forward_tag);
    CAMLassert (infix_offset == 0);

    f = Forward_val (v);
    ft = 0;

    if (Is_block (f)) {
      ft = Tag_val (get_header_val(f) == 0 ? Field(f, 0) : f);
    }

    if (ft == Forward_tag || ft == Lazy_tag ||
        ft == Forcing_tag || ft == Double_tag) {
      /* Do not short-circuit the pointer.  Copy as a normal block. */
      CAMLassert (Wosize_hd (hd) == 1);
      st->live_bytes += Bhsize_hd(hd);
      result = alloc_shared(st->domain, 1, Forward_tag, Reserved_hd(hd));
      if( try_update_object_header(v, p, result, 0) ) {
        p = Op_val (result);
        v = f;
        goto tail_call;
      } else {
        *Hp_val(result) = Make_header(1, No_scan_tag,
                                      caml_global_heap_state.MARKED);
        #ifdef DEBUG
        Field(result, 0) = Val_long(1);
        #endif
      }
    } else {
      v = f;                        /* Follow the forwarding */
      goto tail_call;               /*  then oldify. */
    }
  }
}

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
CAMLno_tsan_for_perf
static void oldify_mopup (struct oldify_state* st, int do_ephemerons)
{
  value v, new_v, f;
  caml_domain_state* domain_state = st->domain;
  struct caml_ephe_ref_table ephe_ref_table =
                                    domain_state->minor_tables->ephe_ref;
  struct caml_ephe_ref_elt *re;
  int redo;

again:
  redo = 0;

  while (st->todo_list != 0) {
    v = st->todo_list;                   /* Get the head. */
    CAMLassert (get_header_val(v) == 0); /* It must be forwarded. */
    new_v = Field(v, 0);                 /* Follow forward pointer. */
    st->todo_list = Field (new_v, 1);    /* Remove from list. */

    f = Field(new_v, 0);
    CAMLassert (!Is_debug_tag(f));
    if (Is_block (f) && Is_young(f)) {
      oldify_one (st, f, Op_val (new_v));
    }
    for (mlsize_t i = 1; i < Wosize_val (new_v); i++){
      f = Field(v, i);
      CAMLassert (!Is_debug_tag(f));
      if (Is_block (f) && Is_young(f)) {
        oldify_one (st, f, Op_val (new_v) + i);
      } else {
        Field(new_v, i) = f;
      }
    }
    CAMLassert (Wosize_val(new_v));
  }

  /* Oldify the key and data in the minor heap of all ephemerons touched in this
     cycle. We are doing this to avoid introducing a barrier for the end of all
     domains promoting reachable objects and having to handle the complexity
     of determining which ephemerons are dead when they link across domains */
  if( do_ephemerons ) {
    for (re = ephe_ref_table.base;
         re < ephe_ref_table.ptr; re++) {
      volatile value *data = re->offset == CAML_EPHE_DATA_OFFSET
                           ? &Ephe_data(re->ephe)
                           : &Field(re->ephe, re->offset);
      value v = *data;
      if (v != caml_ephe_none && Is_block(v) && Is_young(v) ) {
        mlsize_t offs = Tag_val(v) == Infix_tag ? Infix_offset_val(v) : 0;
        v -= offs;
        if (get_header_val(v) == 0) { /* Value copied to major heap */
          *data = Field(v, 0) + offs;
        } else {
          oldify_one(st, *data, data);
          redo = 1; /* oldify_todo_list can still be 0 */
        }
      }
    }
  }

  if (redo) goto again;
}

void caml_empty_minor_heap_domain_clear(caml_domain_state* domain)
{
  struct caml_minor_tables *minor_tables = domain->minor_tables;

  caml_final_empty_young(domain);

  clear_table ((struct generic_table *)&minor_tables->major_ref);
  clear_table ((struct generic_table *)&minor_tables->ephe_ref);
  clear_table ((struct generic_table *)&minor_tables->custom);

  domain->extra_heap_resources_minor = 0.0;
}

/* Try to do a major slice, returns nonzero if there was any work available,
   used as useful spin work while waiting for synchronisation. The return type
   is [int] and not [bool] since it is passed as a parameter to
   [caml_try_run_on_all_domains_with_spin_work]. */
int caml_do_opportunistic_major_slice
  (caml_domain_state* domain_unused, void* unused);
static void minor_gc_leave_barrier
  (caml_domain_state* domain, int participating_count);

void caml_empty_minor_heap_promote(caml_domain_state* domain,
                                   int participating_count,
                                   caml_domain_state** participating)
{
  const struct caml_minor_tables *self_minor_tables = domain->minor_tables;
  value* young_ptr = domain->young_ptr;
  value* young_end = domain->young_end;
  uintnat minor_allocated_bytes = (uintnat)young_end - (uintnat)young_ptr;
  uintnat prev_alloc_words;
  struct oldify_state st = {0};
  value **r;
  intnat c, curr_idx;
  int remembered_roots = 0;
  scan_roots_hook scan_roots_hook;

  st.domain = domain;

  prev_alloc_words = domain->allocated_words;

  caml_gc_log ("Minor collection of domain %d starting", domain->id);
  CAML_EV_BEGIN(EV_MINOR);
  call_timing_hook(&caml_minor_gc_begin_hook);

  CAMLassert(domain == Caml_state);

  if( participating[0] == domain ) {
    CAML_EV_BEGIN(EV_MINOR_GLOBAL_ROOTS);
    caml_scan_global_young_roots(oldify_one, &st);
    CAML_EV_END(EV_MINOR_GLOBAL_ROOTS);
  }

 CAML_EV_BEGIN(EV_MINOR_REMEMBERED_SET);

  if( participating_count > 1 ) {
    int participating_idx = -1;

    for( int i = 0; i < participating_count ; i++ ) {
      if( participating[i] == domain ) {
        participating_idx = i;
        break;
      }
    }

    CAMLassert(participating_idx != -1);

    /* We use this rather odd scheme because it better smoothes the remainder */
    for( curr_idx = 0, c = participating_idx;
         curr_idx < participating_count; curr_idx++) {
      caml_domain_state* foreign_domain = participating[c];

      struct caml_minor_tables* foreign_minor_tables =
                                                 foreign_domain->minor_tables;

      struct caml_ref_table* foreign_major_ref =
                                              &foreign_minor_tables->major_ref;

      /* calculate the size of the remembered set */
      intnat major_ref_size = foreign_major_ref->ptr - foreign_major_ref->base;

      /* number of remembered set entries each domain takes here */
      intnat refs_per_domain = (major_ref_size / participating_count);

      /* where to start in the remembered set */
      value** ref_start = foreign_major_ref->base
                          + (curr_idx * refs_per_domain);

      /* where to end in the remembered set */
      value** ref_end = foreign_major_ref->base
                        + ((curr_idx+1) * refs_per_domain);

      /* if we're the last domain this time, cover all the remaining refs */
      if( curr_idx == participating_count-1 ) {
        caml_gc_log("taking remainder");
        ref_end = foreign_major_ref->ptr;
      }

      caml_gc_log("idx: %d, foreign_domain: %d, ref_size: %"
        ARCH_INTNAT_PRINTF_FORMAT"d, refs_per_domain: %"
        ARCH_INTNAT_PRINTF_FORMAT"d, ref_base: %p, ref_ptr: %p, ref_start: %p"
        ", ref_end: %p",
        participating_idx, foreign_domain->id, major_ref_size, refs_per_domain,
        foreign_major_ref->base, foreign_major_ref->ptr, ref_start, ref_end);

      for( r = ref_start ; r < foreign_major_ref->ptr && r < ref_end ; r++ )
      {
        /* Because the work on the remembered set is shared, other threads may
           attempt to promote the same value; this is fine, but we need the
           writes and reads (here, `*pr`) to be at least `volatile`. */
        value_ptr pr = *r;
        oldify_one (&st, *pr, pr);
        remembered_roots++;
      }

      c = (c+1) % participating_count;
    }
  }
  else
  {
    /* If we're alone, we just do our own remembered set */
    for( r = self_minor_tables->major_ref.base ;
      r < self_minor_tables->major_ref.ptr ; r++ )
    {
      oldify_one (&st, **r, *r);
      remembered_roots++;
    }
  }

  #ifdef DEBUG
    caml_global_barrier(participating_count);
    /* At this point all domains should have gone through all remembered set
       entries. We need to verify that all our remembered set entries are now in
       the major heap or promoted */
    for( r = self_minor_tables->major_ref.base ;
         r < self_minor_tables->major_ref.ptr ; r++ ) {
      /* Everything should be promoted */
      CAMLassert(!(Is_block(**r)) || !(Is_young(**r)));
    }
  #endif

  CAML_EV_BEGIN(EV_MINOR_FINALIZERS_OLDIFY);
  /* promote the finalizers unconditionally as we want to avoid barriers */
  caml_final_do_young_roots (&oldify_one, oldify_scanning_flags, &st,
                             domain, 0);
  CAML_EV_END(EV_MINOR_FINALIZERS_OLDIFY);

  CAML_EV_BEGIN(EV_MINOR_MEMPROF_ROOTS);
  caml_memprof_scan_roots(&oldify_one, oldify_scanning_flags, &st,
                          domain, false);
  CAML_EV_END(EV_MINOR_MEMPROF_ROOTS);

  CAML_EV_BEGIN(EV_MINOR_REMEMBERED_SET_PROMOTE);
  oldify_mopup (&st, 1); /* ephemerons promoted here */
  CAML_EV_END(EV_MINOR_REMEMBERED_SET_PROMOTE);
  CAML_EV_END(EV_MINOR_REMEMBERED_SET);
  caml_gc_log("promoted %d roots, %" ARCH_INTNAT_PRINTF_FORMAT "u bytes",
              remembered_roots, st.live_bytes);

#ifdef DEBUG
  caml_global_barrier(participating_count);
  caml_gc_log("ref_base: %p, ref_ptr: %p",
    self_minor_tables->major_ref.base, self_minor_tables->major_ref.ptr);
  for (r = self_minor_tables->major_ref.base;
       r < self_minor_tables->major_ref.ptr; r++) {
    value vnew = **r;
    CAMLassert (!Is_block(vnew)
            || (get_header_val(vnew) != 0 && !Is_young(vnew)));
  }
#endif

  CAML_EV_BEGIN(EV_MINOR_LOCAL_ROOTS);
  caml_do_local_roots(
    &oldify_one, oldify_scanning_flags, &st,
    domain->local_roots, domain->current_stack, domain->gc_regs);

  scan_roots_hook = atomic_load(&caml_scan_roots_hook);
  if (scan_roots_hook != NULL)
    (*scan_roots_hook)(&oldify_one, oldify_scanning_flags, &st, domain);

  CAML_EV_BEGIN(EV_MINOR_LOCAL_ROOTS_PROMOTE);
  oldify_mopup (&st, 0);
  CAML_EV_END(EV_MINOR_LOCAL_ROOTS_PROMOTE);
  CAML_EV_END(EV_MINOR_LOCAL_ROOTS);

  CAML_EV_BEGIN(EV_MINOR_MEMPROF_CLEAN);
  caml_memprof_after_minor_gc(domain);
  CAML_EV_END(EV_MINOR_MEMPROF_CLEAN);

  domain->young_ptr = domain->young_end;
  /* Trigger a GC poll when half of the minor heap is filled. At that point, a
   * major slice is scheduled. */
  domain->young_trigger = domain->young_start
    + (domain->young_end - domain->young_start) / 2;
  caml_memprof_set_trigger(domain);
  caml_reset_young_limit(domain);

  domain->stat_minor_words += Wsize_bsize (minor_allocated_bytes);
  domain->stat_promoted_words += domain->allocated_words - prev_alloc_words;

  /* Must be called during the STW section -- before any mutators
     start running, so before arriving at the barrier. */
  caml_collect_gc_stats_sample_stw(domain);

  /* The code above is synchronised with other domains by the barrier below,
     which is split into two steps, "arriving" and "leaving". When the final
     domain arrives at the barrier, all other domains are free to leave, after
     which they finish running the STW callback and may, depending on the
     specific STW section, begin executing mutator code.

     Leaving the barrier synchronises (only) with the arrivals of other domains,
     so that all writes performed by a domain before arrival "happen-before" any
     domain leaves the barrier. However, any code after arrival, including the
     code between the two steps, can potentially race with mutator code.
  */

  /* arrive at the barrier */
  if( participating_count > 1 ) {
    if (caml_plat_barrier_arrive(&minor_gc_end_barrier)
        == participating_count) {
      caml_plat_barrier_release(&minor_gc_end_barrier);
    }
  }
  /* other domains may be executing mutator code from this point, but
     not before */

  call_timing_hook(&caml_minor_gc_end_hook);
  CAML_EV_COUNTER(EV_C_MINOR_PROMOTED,
                  Bsize_wsize(domain->allocated_words - prev_alloc_words));

  CAML_EV_COUNTER(EV_C_MINOR_ALLOCATED, minor_allocated_bytes);

  CAML_EV_END(EV_MINOR);
  if (minor_allocated_bytes == 0)
    caml_gc_log ("Minor collection of domain %d completed:"
                 " no minor bytes allocated",
                 domain->id);
  else
    caml_gc_log ("Minor collection of domain %d completed:"
                 " %2.0f%% of %u KB live",
                 domain->id,
                 100.0 * (double)st.live_bytes / (double)minor_allocated_bytes,
                 (unsigned)(minor_allocated_bytes + 512)/1024);

  /* leave the barrier */
  if( participating_count > 1 ) {
    CAML_EV_BEGIN(EV_MINOR_LEAVE_BARRIER);
    minor_gc_leave_barrier(domain, participating_count);
    CAML_EV_END(EV_MINOR_LEAVE_BARRIER);
  }
}

/* Finalize dead custom blocks and do the accounting for the live
   ones. This must be done right after leaving the barrier. At this
   point, all domains have finished minor GC, but this domain hasn't
   resumed running OCaml code. Other domains may have resumed OCaml
   code, but they cannot have any pointers into our minor heap. */
static void custom_finalize_minor (caml_domain_state * domain)
{
  for (struct caml_custom_elt *elt = domain->minor_tables->custom.base;
       elt < domain->minor_tables->custom.ptr;
       elt++) {
    value *v = &elt->block;
    if (Is_block(*v) && Is_young(*v)) {
      if (get_header_val(*v) == 0) { /* value copied to major heap */
        caml_adjust_gc_speed(elt->mem, elt->max);
      } else {
        void (*final_fun)(value) = Custom_ops_val(*v)->finalize;
        if (final_fun != NULL) final_fun(*v);
      }
    }
  }
}

/* Increment the counter non-atomically, when it is already known that this
   thread is alone in trying to increment it. */
static void nonatomic_increment_counter(atomic_uintnat* counter) {
  atomic_store_relaxed(counter, 1 + atomic_load_relaxed(counter));
}

static void minor_gc_leave_barrier
  (caml_domain_state* domain, int participating_count)
{
  /* Spin while we have major work available */
  SPIN_WAIT_BOUNDED {
    if (caml_plat_barrier_is_released(&minor_gc_end_barrier)) {
      return;
    }

    if (!caml_do_opportunistic_major_slice(domain, 0)) {
      break;
    }
  }

  /* Spin a bit longer, which is far less fruitful if we're waiting on
     more than one thread */
  unsigned spins =
    participating_count == 2 ? Max_spins_long : Max_spins_medium;
  SPIN_WAIT_NTIMES(spins) {
    if (caml_plat_barrier_is_released(&minor_gc_end_barrier)) {
      return;
    }
  }

  /* If there's nothing to do, block */
  caml_plat_barrier_wait(&minor_gc_end_barrier);
}

int caml_do_opportunistic_major_slice
  (caml_domain_state* domain_state, void* unused)
{
  int work_available = caml_opportunistic_major_work_available(domain_state);
  if (work_available) {
    /* NB: need to put guard around the ev logs to prevent spam when we poll */
    uintnat log_events =
        atomic_load_relaxed(&caml_verb_gc) & CAML_GC_MSG_SLICESIZE;
    if (log_events) CAML_EV_BEGIN(EV_MAJOR_MARK_OPPORTUNISTIC);
    caml_opportunistic_major_collection_slice(Major_slice_work_min);
    if (log_events) CAML_EV_END(EV_MAJOR_MARK_OPPORTUNISTIC);
  }
  return work_available;
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/
void caml_empty_minor_heap_setup(caml_domain_state* domain_unused) {
  /* Increment the total number of minor collections done in the program */
  nonatomic_increment_counter (&caml_minor_collections_count);
  caml_plat_barrier_reset(&minor_gc_end_barrier);
}

/* must be called within a STW section */
static void
caml_stw_empty_minor_heap_no_major_slice(caml_domain_state* domain,
                                         void* unused,
                                         int participating_count,
                                         caml_domain_state** participating)
{
#ifdef DEBUG
  uintnat* initial_young_ptr = (uintnat*)domain->young_ptr;
  CAMLassert(caml_domain_is_in_stw());
#endif

  if( participating[0] == domain ) {
    nonatomic_increment_counter(&caml_minor_cycles_started);
  }

  caml_gc_log("running stw empty_minor_heap_promote");
  caml_empty_minor_heap_promote(domain, participating_count, participating);

  CAML_EV_BEGIN(EV_MINOR_FINALIZED);
  caml_gc_log("finalizing dead minor custom blocks");
  custom_finalize_minor(domain);
  CAML_EV_END(EV_MINOR_FINALIZED);

  CAML_EV_BEGIN(EV_MINOR_FINALIZERS_ADMIN);
  caml_gc_log("running finalizer data structure book-keeping");
  caml_final_update_last_minor(domain);
  CAML_EV_END(EV_MINOR_FINALIZERS_ADMIN);

  CAML_EV_BEGIN(EV_MINOR_CLEAR);
  caml_gc_log("running stw empty_minor_heap_domain_clear");
  caml_empty_minor_heap_domain_clear(domain);

#ifdef DEBUG
  {
    for (uintnat *p = initial_young_ptr; p < (uintnat*)domain->young_end; ++p)
      *p = Debug_free_minor;
  }
#endif

  CAML_EV_END(EV_MINOR_CLEAR);
  caml_gc_log("finished stw empty_minor_heap");
}

static void caml_stw_empty_minor_heap (caml_domain_state* domain, void* unused,
                                       int participating_count,
                                       caml_domain_state** participating)
{
  caml_stw_empty_minor_heap_no_major_slice(domain, unused,
                                           participating_count, participating);
}

/* must be called within a STW section  */
void caml_empty_minor_heap_no_major_slice_from_stw(
  caml_domain_state* domain,
  void* unused,
  int participating_count,
  caml_domain_state** participating)
{
  Caml_global_barrier_if_final(participating_count) {
    caml_empty_minor_heap_setup(domain);
  }

  /* if we are entering from within a major GC STW section then
     we do not schedule another major collection slice */
  caml_stw_empty_minor_heap_no_major_slice(domain, (void*)0,
                                           participating_count, participating);
}

/* must be called outside a STW section */
int caml_try_empty_minor_heap_on_all_domains (void)
{
  #ifdef DEBUG
  CAMLassert(!caml_domain_is_in_stw());
  #endif

  caml_gc_log("requesting stw empty_minor_heap");
  return caml_try_run_on_all_domains_with_spin_work(
    1, /* synchronous */
    &caml_stw_empty_minor_heap, 0, /* stw handler */
    &caml_empty_minor_heap_setup, /* leader setup */
    &caml_do_opportunistic_major_slice, 0 /* enter spin work */);
    /* leaves when done by default*/
}

/* must be called outside a STW section, will retry until we have emptied our
   minor heap */
void caml_empty_minor_heaps_once (void)
{
  uintnat saved_minor_cycle = atomic_load_relaxed(&caml_minor_cycles_started);

  #ifdef DEBUG
  CAMLassert(!caml_domain_is_in_stw());
  #endif

  CAML_EV_BEGIN(EV_EMPTY_MINOR);

  /* To handle the case where multiple domains try to execute a minor gc
     STW section */
  do {
    caml_try_empty_minor_heap_on_all_domains();
  } while (saved_minor_cycle ==
           atomic_load_relaxed(&caml_minor_cycles_started));

  CAML_EV_END(EV_EMPTY_MINOR);
}

/* Called by minor allocations when [Caml_state->young_ptr] reaches
   [Caml_state->young_limit]. We may have to either call memprof or
   the gc. */
void caml_alloc_small_dispatch (caml_domain_state * dom_st,
                                intnat wosize, int flags,
                                int nallocs, unsigned char* encoded_alloc_lens)
{
  intnat whsize = Whsize_wosize(wosize);

  /* First, we un-do the allocation performed in [Alloc_small] */
  dom_st->young_ptr += whsize;

  while(1) {
    /* We might be here because of an async callback / urgent GC
       request. Take the opportunity to do what has been requested. */
    if (flags & CAML_FROM_CAML)
      /* In the case of allocations performed from OCaml, execute
         asynchronous callbacks. */
      caml_get_value_or_raise(caml_do_pending_actions_res());
    else {
      /* In the case of allocations performed from C, only perform
         non-delayable actions. */
      caml_handle_gc_interrupt();
    }

    /* Now, there might be enough room in the minor heap to do our
       allocation. */
    if (dom_st->young_ptr - whsize >= dom_st->young_start)
      break;

    /* If not, then empty the minor heap, and check again for async
       callbacks. */
    CAML_EV_COUNTER(EV_C_FORCE_MINOR_ALLOC_SMALL, 1);
    caml_poll_gc_work();
  }

  /* Re-do the allocation: we now have enough space in the minor heap. */
  dom_st->young_ptr -= whsize;

  /* Check if the allocated block has been sampled by memprof. */
  if (dom_st->young_ptr < dom_st->memprof_young_trigger) {
    if(flags & CAML_DO_TRACK) {
      caml_memprof_sample_young(wosize, flags & CAML_FROM_CAML,
                                nallocs, encoded_alloc_lens);
      /* Until the allocation actually takes place, the heap is in an
         invalid state (see comments in [caml_memprof_sample_young]).
         Hence, very few heap operations are allowed between this point
         and the actual allocation.

         Specifically, [dom_st->young_ptr] must not now be modified
         before the allocation, because it has been used to predict
         addresses of sampled block(s).
      */
    } else { /* CAML DONT TRACK */
      caml_memprof_set_trigger(dom_st);
      caml_reset_young_limit(dom_st);
    }
  }
}

/* Request a minor collection and enter as if it were an interrupt.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_request_minor_gc();
  caml_handle_gc_interrupt();
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  if (Caml_check_gc_interrupt(Caml_state)) {
    CAMLparam1(extra_root);
    caml_handle_gc_interrupt();
    CAMLdrop;
  }
  return extra_root;
}

static void realloc_generic_table
(struct generic_table *tbl, asize_t element_size,
 ev_runtime_counter ev_counter_name,
 const char *msg_threshold, const char *msg_growing, const char *msg_error)
{
  CAMLassert (tbl->ptr == tbl->limit);
  CAMLassert (tbl->limit <= tbl->end);
  CAMLassert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    alloc_generic_table (tbl, Caml_state->minor_heap_wsz / 8, 256,
                         element_size);
  }else if (tbl->limit == tbl->threshold){
    CAML_EV_COUNTER (ev_counter_name, 1);
    CAML_GC_MESSAGE(STACKSIZE, msg_threshold, 0);
    tbl->limit = tbl->end;
    caml_request_minor_gc ();
  }else{
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * element_size;
    CAML_GC_MESSAGE(STACKSIZE, msg_growing, (intnat) sz/1024);
    tbl->base = caml_stat_resize_noexc (tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("%s", msg_error);
    }
    tbl->end = tbl->base + (tbl->size + tbl->reserve) * element_size;
    tbl->threshold = tbl->base + tbl->size * element_size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (value *),
     EV_C_REQUEST_MINOR_REALLOC_REF_TABLE,
     "ref_table threshold crossed\n",
     "Growing ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ref_table overflow");
}

void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_ephe_ref_elt),
     EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE,
     "ephe_ref_table threshold crossed\n",
     "Growing ephe_ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ephe_ref_table overflow");
}

void caml_realloc_custom_table (struct caml_custom_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_custom_elt),
     EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE,
     "custom_table threshold crossed\n",
     "Growing custom_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "custom_table overflow");
}
