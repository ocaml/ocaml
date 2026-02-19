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
#include "caml/sizeclasses.h"
#include "caml/startup_aux.h"
#include "caml/weak.h"

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
  CAMLassert (domain_state->young_ptr == domain_state->young_end);

  if(caml_reallocate_minor_heap_arena(wsize) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap");
  }

  reset_minor_tables(r);
}

/*****************************************************************************/

/* The `todo_list` of a minor collection is a linked list of
 * incompletely-scanned blocks. Each entry is a minor-heap block of
 * scannable size at least 2. Field 0 of that block points to its
 * major-heap copy. Field 1 _of the major-heap copy_ is the next entry
 * in the todo list. (field 0 of the major-heap copy is where the
 * (unscanned) field 0 of the minor-heap block has been
 * saved). `oldify_one` adds blocks to the todo list; `oldify_mopup`
 * traverses it.
 *
 * Blocks with scannable size 1 are scanned tail-recursively in
 * `oldify_one`.
*/

struct oldify_state {
  value todo_list;
  caml_domain_state* domain;
  bool domain_alone;
  status status;
  shared_heap_fast_data_p fast_data;
  uintnat live_bytes;
  uintnat pool_live_blocks;
  uintnat pool_live_words;
  uintnat pool_frag_words;
  uintnat allocated_words;
};

/* In-progress headers are zeros except for the lowest color bit set
   to 1. */
#define In_progress_hd (Make_header(0, 0, 0x100))
#define Is_update_in_progress(hd) ((hd) == In_progress_hd)

static header_t spin_on_header(value v) {
  SPIN_WAIT {
    header_t h = atomic_load(Hp_atomic_val(v));
    if (Is_promoted_hd(h))
      return h;
  }
}

CAMLno_tsan_for_perf
Caml_inline header_t get_header_val(value v) {
  header_t hd = atomic_load_acquire(Hp_atomic_val(v));
  if (!Is_update_in_progress(hd))
    return hd;

  return spin_on_header(v);
}

/* Allocate a block to copy `v` into, and attempt to write the
 * forwarding pointer into field 0 of `v`. If we lose the race against
 * some other domain to do that, return 0. If we win, return our
 * newly-allocated block. Win or lose, update `*p` with the promoted
 * block plus `infix_offset` (which is in bytes).
 *
 * `hd` is the header of `v`.
 *
 * `prefix` is the size in words of any unscannable prefix of `v`. If
 * `v` includes any infix tags, they must be within this prefix.
 *
 * `st` points to the oldify_state, which is where we cache all sorts
 * of handy values and accumulators during a single minor GC.
*/

Caml_inline value try_promote(value v, volatile value *p, header_t hd,
                              mlsize_t infix_offset, mlsize_t prefix,
                              struct oldify_state *st)
{
  caml_domain_state *domain = st->domain;
  void *mem = NULL;

  CAMLassert(!Is_update_in_progress(hd)); /* from get_header_val */
  CAMLassert(!Is_promoted_hd(hd)); /* Promoted blocks already filtered out */

  /* manual inline of parts of caml_shared_try_alloc */
  mlsize_t wosize = Wosize_hd(hd);
  mlsize_t whsize = Whsize_wosize(wosize);
  if (whsize <= SIZECLASS_MAX) {
    mem = caml_shared_fast_alloc(whsize, st->fast_data, domain);
    if (mem) {
      CAML_EV_ALLOC(wosize);
      ++ st->pool_live_blocks;
      st->pool_live_words += whsize;
      st->pool_frag_words += wfrag_whsize[whsize];
      Hd_hp((value *)mem) = Hd_with_color(hd, st->status);
    }
  }
  if (!mem) {
    mem = caml_shared_try_alloc(domain->shared_heap, Wosize_hd(hd),
                                Tag_hd(hd), Reserved_hd(hd));
  }
  if (mem == NULL) {
    caml_fatal_error("allocation failure during minor GC");
  }
  st->allocated_words += Whsize_wosize(wosize);
  value result = Val_hp(mem);

  /* Copy unscannable prefix, which will include any infix tags, so
   * that infix pointers to `v` can be oldified into pointers with
   * Infix_tag to a working header as soon as the header is
   * Promoted_hd (so that, e.g., major GC marking can work while the
   * block is still on our oldify todo list). Have to do this here,
   * before we update the object header. Start from field 2 as fields
   * 0 and 1 are used for forwarding pointers and the todo-list. */
  CAMLassert(infix_offset <= prefix * sizeof(value));
  for (mlsize_t j = 2; j < prefix; ++j) {
    Field(result, j) = Field(v, j);
  }

  if (st->domain_alone) {
    *Hp_val (v) = Promoted_hd;
    Field(v, 0) = result;
  } else {
    if (atomic_compare_exchange_strong(Hp_atomic_val(v), &hd, In_progress_hd)) {
      /* Success. Now we can write the forwarding pointer. */
      atomic_store_relaxed(Op_atomic_val(v), result);
      /* And update header ('release' ensures after update of fwd pointer) */
      atomic_store_release(Hp_atomic_val(v), Promoted_hd);
    } else {
      /* Failure case: header was updated by another domain. Spin for
         that update to complete, then throw away our allocated block
         and use the one from the other domain. */
      (void)spin_on_header(v);

      *Hp_val(result) = Make_header(wosize, Abstract_tag, st->status);
#ifdef DEBUG
      for (mlsize_t i = 0; i < wosize ; i++) {
        Field(result, i) = Debug_free_unused;
      }
#endif
      *p = Field(v, 0) + infix_offset;
      return (value)0;
    }
  }

  st->live_bytes += Bhsize_hd(hd);
  *p = result + infix_offset;
  return result;
}

/* oldify_one is a no-op outside the minor heap. */
static scanning_action_flags oldify_scanning_flags =
  SCANNING_ONLY_YOUNG_VALUES | SCANNING_ONLY_RECENT_FRAMES;

static void oldify_one (void* st_v, value v, volatile value *p)
{
tail_call:
  if (!(Is_block(v) && Is_young(v))) {
    /* not a minor block */
    *p = v;
    return;
  }

  struct oldify_state* st = st_v;
  header_t hd;
  tag_t tag;
  mlsize_t infix_offset = 0;

  do {
    hd = get_header_val(v);
    if (Is_promoted_hd(hd)) {
      /* already promoted */
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

  mlsize_t sz = Wosize_hd (hd);
  value field0 = Field(v, 0); /* will be overwritten by try_promote */
  if (tag == Forward_tag) {
    CAMLassert (infix_offset == 0);
    CAMLassert (sz == 1);
    value f = field0;
    tag_t ft = 0;
    if (Is_block (f)) {
      ft = Tag_val (Is_promoted_hd(get_header_val(f)) ? Field(f, 0) : f);
    }

    if (ft == Forward_tag || ft == Lazy_tag ||
        ft == Forcing_tag || ft == Double_tag) {
      /* Do not short-circuit the pointer.  Copy as a normal block. */
      value result = try_promote(v, p, hd, infix_offset, 0, st);
      if (result) {
        p = Op_val (result);
        v = f;
        goto tail_call;
      }
    } else {
      v = f;                        /* Follow the forwarding */
      goto tail_call;               /* then oldify. */
    }
  } else {
    mlsize_t unscannable_prefix =
      (tag == Closure_tag) ? Start_env_closinfo(Closinfo_val(v)) : 0;
    value result = try_promote(v, p, hd, infix_offset, unscannable_prefix, st);

    if (result) {
      if (tag == Cont_tag) {
        CAMLassert(infix_offset == 0);
        CAMLassert(sz == 1);
        struct stack_info* stk = Ptr_val(field0);
        Field(result, 0) = field0;
        if (stk != NULL) {
          caml_scan_stack(&oldify_one, oldify_scanning_flags, st, stk, 0);
        }
      } else if (!Scannable_tag(tag)) {
        CAMLassert (infix_offset == 0);
        CAMLassert (unscannable_prefix == 0); /* not Closure_tag */
        Field(result, 0) = field0;
        for (mlsize_t i = 1; i < sz; i++) {
          Field(result, i) = Field(v, i);
        }
      } else { /* Scannable, and neither Cont_tag nor Forward_tag */
        CAMLassert(tag < Infix_tag);
        if (sz == 1) {
          p = Op_val(result);
          v = field0;
          goto tail_call;
        } else { /* add to todo_list */
          CAMLassert (sz > 1);
          Field(result, 0) = field0;
          Field(result, 1) = st->todo_list;
          st->todo_list = v;
        }
      }
    }
  }
}

typedef struct {
  bool locked_ephemerons;
} mopup_result;

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
CAMLno_tsan_for_perf
static mopup_result oldify_mopup (struct oldify_state* st, int do_ephemerons)
{
  mopup_result result = { .locked_ephemerons = false, };
  bool redo;

  do {
    redo = false;
    while (st->todo_list != 0) {
      value v = st->todo_list;                        /* Get the head. */
      CAMLassert (Is_promoted_hd(get_header_val(v))); /* It must be promoted. */
      value new_v = Field(v, 0);                      /* Follow forwarding. */
      value next = Field (new_v, 1);
      st->todo_list = next;
      /* TODO: Measure whether this prefetch helps or hurts */
      caml_prefetchw((void*)next);

      mlsize_t wosize = Wosize_val(new_v);
      /* [v] was only added to the [todo_list] if its [wosize > 1].
         - It needs to be greater than 0 because we oldify the first field.
         - It needs to be greater than 1 so the below loop runs at least once,
         overwriting Field(new_v, 1) which [oldify_one] used as temporary
         storage of the next value of [todo_list].
      */
      CAMLassert (wosize > 1);

      value f = Field(new_v, 0);
      CAMLassert (!Is_debug_tag(f));
      if (Is_block (f) && Is_young(f)) {
        oldify_one (st, f, Op_val (new_v));
      }

      mlsize_t i = 1;
      if(Tag_val(new_v) == Closure_tag) {
        /* non-scannable prefix already copied in oldify_one */
        Field(new_v, 1) = Field(v, 1); /* was todo-list pointer */
        i = Start_env_closinfo(Closinfo_val(v));
      }

      for (; i < wosize; i++){
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

    /* Oldify ephemeron data fields pointing to the minor heap, and some keys.

       In theory the data need only be promoted if the ephemeron and all
       keys are live, but determining this may require a multi-round
       synchronisation (consider the case where the keys are live, but
       from different domains). So, we promote hard cases
       unconditionally, leaving them for the major GC.

       There are easy cases, though, in which an ephemeron key is on our
       own minor heap. In that case, we "lock" the key (stashing it in
       our ephe_ref table and replacing it with caml_ephe_locked), then
       after minor GC completes we check whether locked keys were
       promoted. If not, we can clean the ephemeron value (see
       ephe_clean_minor).

       The condition that it must be our *own* minor heap is important:
       checking whether a block was promoted after minor GC completes is
       safe only on our own heap, because other domains will immediately
       begin reusing theirs. */
    if (do_ephemerons) {
      struct caml_ephe_ref_table ephe_ref_table =
        st->domain->minor_tables->ephe_ref;
      /* Limits of *this* minor heap, not other domains' */
      value young_start = (value)st->domain->young_start;
      value young_end = (value)st->domain->young_end;
      for (struct caml_ephe_ref_elt *re = ephe_ref_table.base;
           re < ephe_ref_table.ptr; re++) {
        if (re->locked != Val_unit)
          continue; /* we locked it on a prior iteration */
        atomic_value* data = Op_atomic_val(re->ephe) + re->offset;
        value v = atomic_load_relaxed(data);
        header_t hd;
        if (v != caml_ephe_none &&                 /* occupied field       */
            v != caml_ephe_locked &&               /* not already locked   */
            re->offset != CAML_EPHE_DATA_OFFSET && /* ephe key (not data)  */
            Is_block(v) &&                         /* a block              */
            young_start <= v && v < young_end &&   /* on *this* minor heap */
            !Is_promoted_hd(hd = Hd_val(v)) &&     /* not already promoted */
            Tag_hd(hd) != Infix_tag &&             /* not Infix_tag        */
            atomic_compare_exchange_strong(data, &v, caml_ephe_locked)) {
          /* locked, clean it later */
          re->locked = v;
          result.locked_ephemerons = true;
        } else {
          value new_v;
          oldify_one(st, v, &new_v);
          if (new_v != v) {
            /* atomic CAS, because another domain might be trying to lock it.
               (We don't care who wins the race, so result not checked) */
            atomic_compare_exchange_strong(data, &v, new_v);
            redo = true; /* may have found new oldify_todo_list */
          }
        }
      }
    }
  } while (redo);
  return result;
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

typedef struct {
  bool locked_ephemerons;
} promote_result;

static promote_result
caml_empty_minor_heap_promote(caml_domain_state* domain,
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
  promote_result result = { .locked_ephemerons = false, };

  st.domain = domain;
  st.domain_alone = caml_domain_alone();
  st.status = caml_allocation_status();
  st.fast_data = caml_shared_fast_data(domain->shared_heap);

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

      caml_gc_log("idx: %d, foreign_domain: %d, ref_size: %" CAML_PRIdNAT ", "
        "refs_per_domain: %" CAML_PRIdNAT ", ref_base: %p, ref_ptr: %p, "
        "ref_start: %p, ref_end: %p",
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
  mopup_result mopup_result = oldify_mopup (&st, 1); /* promoting ephemerons */
  result.locked_ephemerons = mopup_result.locked_ephemerons;
  CAML_EV_END(EV_MINOR_REMEMBERED_SET_PROMOTE);
  CAML_EV_END(EV_MINOR_REMEMBERED_SET);
  caml_gc_log("promoted %d roots, %" CAML_PRIuNAT " bytes",
              remembered_roots, st.live_bytes);

#ifdef DEBUG
  caml_global_barrier(participating_count);
  caml_gc_log("ref_base: %p, ref_ptr: %p",
    self_minor_tables->major_ref.base, self_minor_tables->major_ref.ptr);
  for (r = self_minor_tables->major_ref.base;
       r < self_minor_tables->major_ref.ptr; r++) {
    value vnew = **r;
    CAMLassert (!Is_block(vnew)
            || (!Is_promoted_hd(get_header_val(vnew)) && !Is_young(vnew)));
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
  (void)oldify_mopup (&st, 0); /* ignore result as we're not doing ephemerons */
  CAML_EV_END(EV_MINOR_LOCAL_ROOTS_PROMOTE);
  CAML_EV_END(EV_MINOR_LOCAL_ROOTS);

  caml_shared_add_pool_stats(domain->shared_heap,
                             st.pool_live_blocks,
                             st.pool_live_words,
                             st.pool_frag_words);
  domain->allocated_words += st.allocated_words;

  if (minor_allocated_bytes) {
    CAML_GC_MESSAGE(MINOR,
                    "Promoted %"CAML_PRIuNAT" bytes "
                    "(%2.0f%% of %u KB)\n",
                    st.live_bytes,
                    (100.0 * st.live_bytes) / minor_allocated_bytes,
                    (unsigned)(minor_allocated_bytes + 512)/1024);
  } else {
    CAML_GC_MESSAGE(MINOR,
                    "Promoted %"CAML_PRIuNAT" bytes (of zero)\n",
                    st.live_bytes);
  }

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
  CAML_EV_COUNTER(EV_C_MINOR_PROMOTED_WORDS,
                  domain->allocated_words - prev_alloc_words);

  CAML_EV_COUNTER(EV_C_MINOR_ALLOCATED, minor_allocated_bytes);
  CAML_EV_COUNTER(EV_C_MINOR_ALLOCATED_WORDS,
                  Whsize_wosize(minor_allocated_bytes));

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
  return result;
}

static void ephe_clean_minor (caml_domain_state* domain)
{
  struct caml_ephe_ref_table table =
    domain->minor_tables->ephe_ref;
  for (struct caml_ephe_ref_elt* re = table.base; re < table.ptr; re++) {
    value v = re->locked;
    if (v == Val_unit)
      continue;
    /* This runs after the barrier: any promotion has completed,
       so we don't need to get_header_val / spin_on_header */
    header_t hd = Hd_val(v);
    mlsize_t infix_offset = 0;
    if (Tag_hd(hd) == Infix_tag) {
      infix_offset = Infix_offset_hd(hd);
      v -= infix_offset;
      hd = Hd_val(v);
    }
    CAMLassert(Tag_hd(hd) != Infix_tag);
    if (Is_promoted_hd(hd)) {
      /* promoted */
      v = Field(v, 0) + infix_offset;
    } else {
      /* collected */
      v = caml_ephe_none;
      atomic_store_relaxed(Ephe_data_addr(re->ephe), caml_ephe_none);
    }
    atomic_store_release(Op_atomic_val(re->ephe) + re->offset, v);
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
      if (Is_promoted_hd(Hd_val(*v))) { /* value copied to major heap */
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

   This function also samples [caml_gc_mark_phase_requested] to see whether
   [caml_mark_roots_stw] should be called. To guarantee that all domains
   agree on whether the roots should be marked, this variable is sampled
   only once, instead of having domains check it individually.
*/
void caml_empty_minor_heap_setup(caml_domain_state* domain_unused,
                                 void *mark_requested_p) {
  /* Check whether the mark phase has been requested */
  *(uintnat*)mark_requested_p =
    atomic_load_relaxed(&caml_gc_mark_phase_requested)
    ? atomic_exchange(&caml_gc_mark_phase_requested, 0)
    : 0;
  /* Increment the total number of minor collections done in the program */
  nonatomic_increment_counter (&caml_minor_collections_count);
  caml_plat_barrier_reset(&minor_gc_end_barrier);
}

/* must be called within a STW section */
static void
caml_stw_empty_minor_heap_no_major_slice(caml_domain_state* domain,
                                         void* mark_requested_p,
                                         int participating_count,
                                         caml_domain_state** participating)
{
#ifdef DEBUG
  uintnat* initial_young_ptr = (uintnat*)domain->young_ptr;
  CAMLassert(caml_domain_is_in_stw());
#endif

  /* mark_requested_p must be read before minor GC barrier */
  uintnat mark_requested = *(uintnat*)mark_requested_p;

  if( participating[0] == domain ) {
    nonatomic_increment_counter(&caml_minor_cycles_started);
  }

  caml_gc_log("running stw empty_minor_heap_promote");
  promote_result prom =
    caml_empty_minor_heap_promote(domain, participating_count, participating);

  if (prom.locked_ephemerons) {
    CAML_EV_BEGIN(EV_MINOR_EPHE_CLEAN);
    caml_gc_log("cleaning minor ephemerons");
    ephe_clean_minor(domain);
    CAML_EV_END(EV_MINOR_EPHE_CLEAN);
  }

  CAML_EV_BEGIN(EV_MINOR_MEMPROF_CLEAN);
  caml_gc_log("updating memprof");
  caml_memprof_after_minor_gc(domain);
  CAML_EV_END(EV_MINOR_MEMPROF_CLEAN);

  /* while the minor heap is empty, allow the major GC to mark roots */
  if (mark_requested)
    caml_mark_roots_stw(participating_count, participating);

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
  CAMLassert(domain->young_ptr == domain->young_end);
}

static void caml_stw_empty_minor_heap (caml_domain_state* domain,
                                       void* mark_requested_p,
                                       int participating_count,
                                       caml_domain_state** participating)
{
  caml_stw_empty_minor_heap_no_major_slice(domain, mark_requested_p,
                                           participating_count, participating);
}

/* must be called within a STW section  */
void caml_empty_minor_heap_no_major_slice_from_stw(
  caml_domain_state* domain,
  void* unused,
  int participating_count,
  caml_domain_state** participating)
{

  static uintnat mark_requested; /* Written by only one domain */
  Caml_global_barrier_if_final(participating_count) {
    caml_empty_minor_heap_setup(domain, &mark_requested);
  }

  /* if we are entering from within a major GC STW section then
     we do not schedule another major collection slice */
  caml_stw_empty_minor_heap_no_major_slice(domain, &mark_requested,
                                           participating_count, participating);
}

/* must be called outside a STW section */
int caml_try_empty_minor_heap_on_all_domains (void)
{
  #ifdef DEBUG
  CAMLassert(!caml_domain_is_in_stw());
  #endif

  caml_gc_log("requesting stw empty_minor_heap");
  uintnat mark_requested = 0;
  return caml_try_run_on_all_domains_with_spin_work(
    1, /* synchronous */
    &caml_stw_empty_minor_heap, /* stw handler */
    &mark_requested,
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
     "Growing ref_table to %" CAML_PRIdNAT "k bytes\n",
     "ref_table overflow");
}

void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_ephe_ref_elt),
     EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE,
     "ephe_ref_table threshold crossed\n",
     "Growing ephe_ref_table to %" CAML_PRIdNAT "k bytes\n",
     "ephe_ref_table overflow");
}

void caml_realloc_custom_table (struct caml_custom_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_custom_elt),
     EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE,
     "custom_table threshold crossed\n",
     "Growing custom_table to %" CAML_PRIdNAT "k bytes\n",
     "custom_table overflow");
}
