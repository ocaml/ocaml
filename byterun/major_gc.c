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

#include <stdlib.h>

#include "caml/config.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/roots.h"
#include "caml/globroots.h"
#include "caml/domain.h"
#include "caml/fiber.h"
#include "caml/addrmap.h"
#include "caml/platform.h"
#include "caml/startup_aux.h"
#include "caml/eventlog.h"
#include <string.h>

/*
  FIXME: This is far too small. A better policy would be to match
  OCaml's: allow the mark stack to grow to major_heap / 32. However,
  leaving it small for now means the overflow code gets more testing.
*/
#define MARK_STACK_SIZE (1 << 14)

uintnat caml_percent_free = Percent_free_def;

/* This variable is only written with the world stopped,
   so it need not be atomic */
static uintnat major_cycles_completed = 0;

static atomic_uintnat num_domains_to_sweep = {0};
static atomic_uintnat num_domains_to_mark = {0};

uintnat caml_get_num_domains_to_mark () {
  return atomic_load_acq(&num_domains_to_mark);
}

static uintnat default_slice_budget() {
  /*
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = caml_stat_heap_size * caml_percent_free
                      / (100 + caml_percent_free)

     Assuming steady state and enforcing a constant allocation rate, then
     FM is divided in 2/3 for garbage and 1/3 for free list.
                 G = 2 * FM / 3
     G is also the amount of memory that will be used during this cycle
     (still assuming steady state).

     Proportion of G consumed since the previous slice:
                 PH = Caml_state->allocated_words / G
                    = Caml_state->allocated_words * 3 * (100 + caml_percent_free)
                      / (2 * caml_stat_heap_size * caml_percent_free)
     Proportion of extra-heap resources consumed since the previous slice:
                 PE = caml_extra_heap_resources
     Proportion of total work to do in this slice:
                 P  = max (PH, PE)
     Amount of marking work for the GC cycle:
                 MW = caml_stat_heap_size * 100 / (100 + caml_percent_free)
     Amount of sweeping work for the GC cycle:
                 SW = caml_stat_heap_size

     Total amount of work for the GC cycle:
                 TW = MW + SW

     Amount of work to do for this slice:
                 W = P * TW
  */
  uintnat heap_size = caml_heap_size(Caml_state->shared_heap);
  double heap_words = (double)Wsize_bsize(heap_size);
  double p = (double) Caml_state->allocated_words * 3.0 * (100 + caml_percent_free)
      / heap_words / caml_percent_free / 2.0;

  double total_work =
    heap_words * 100 / (100 + caml_percent_free) /* marking */
    + heap_words; /* sweeping */

  return (intnat)(p * total_work);
  //return 1ll << 50;
}

enum steal_result { Shared, Not_shared, No_work };

struct steal_payload {
  caml_domain_state* thief;
  /* The major cycle at which the stealing was initiated. */
  uintnat major_cycle;
  enum steal_result result;
};

static void handle_steal_req (struct domain* targetd, void* plv,
                              interrupt* done) {
  struct steal_payload* pl = plv;
  uintnat steal_size, new_size;
  caml_domain_state* target = targetd->state;

  if (pl->major_cycle != major_cycles_completed) {
    pl->result = Not_shared;
  } else if (target->stealing || target->mark_stack_count == 0) {
    pl->result = No_work;
  } else {
    CAMLassert(pl->thief->mark_stack_count == 0);

    if (target->mark_stack_count < 16) {
      steal_size = target->mark_stack_count;
    } else {
      steal_size = target->mark_stack_count / 2
                 + target->mark_stack_count % 2;
    }
    new_size = target->mark_stack_count - steal_size;
    memcpy(pl->thief->mark_stack, &target->mark_stack[new_size],
           steal_size * sizeof(value));
    target->mark_stack_count = new_size;

    /* Mark stack size and domain local marking_done flag are updated in
     * steal_mark_work(). */
    CAMLassert (pl->thief->marking_done);
    atomic_fetch_add(&num_domains_to_mark, 1);

    if (new_size == 0) {
      CAMLassert(!target->marking_done);
      atomic_fetch_add(&num_domains_to_mark, -1);
      target->marking_done = 1;
    }

    pl->thief->mark_stack_count = steal_size;
    pl->thief->marking_done = 0;
    pl->result = Shared;
  }
  caml_acknowledge_interrupt(done);
}

/* Return domain_id of victim if success. Otherwise, return -1. */
static int steal_mark_work () {
  struct steal_payload pl;
  struct domain* domain_self = caml_domain_self ();
  int my_id = domain_self->state->id;
  struct domain* victim;
  int i;

  pl.thief = Caml_state;
  pl.major_cycle = major_cycles_completed;

  if (atomic_load_acq(&num_domains_to_mark) == 0)
    return -1;

  Caml_state->stealing = 1;
  for (i = (my_id + 1) % Max_domains; i != my_id; i = (i + 1) % Max_domains) {
    victim = caml_domain_of_id(i);

    if(victim->state && caml_domain_rpc(victim, &handle_steal_req, &pl)) {
      if (pl.result == Shared) {
        caml_gc_log("Stolen mark work (size=%llu) from domain %d",
                    domain_self->state->mark_stack_count, i);
        Caml_state->stealing = 0;
        return i;
      }
    }

    /* Abort stealing if marking was done or a major cycle was completed. */
    if (atomic_load_acq(&num_domains_to_mark) == 0 ||
        pl.major_cycle != major_cycles_completed)
      break;
  }

  caml_gc_log("Mark work stealing failure");
  Caml_state->stealing = 0;
  return -1;
}

static void mark_stack_prune();
static struct pool* find_pool_to_rescan();

static void mark_stack_push(value v) {
  CAMLassert(Is_block(v));
  caml_domain_state* domain_state = Caml_state;
  if (domain_state->mark_stack_count >= MARK_STACK_SIZE)
    mark_stack_prune();
  domain_state->mark_stack[domain_state->mark_stack_count++] = v;
}

/* to fit scanning_action */
static void mark_stack_push_act(void* state, value v, value* ignored) {
  mark_stack_push(v);
}

#ifdef DEBUG
#define Is_markable(v) (Is_block(v) && !Is_minor(v) && v != Debug_free_major)
#else
#define Is_markable(v) (Is_block(v) && !Is_minor(v))
#endif

/* mark until the budget runs out or the mark stack empties */
static intnat do_some_marking(intnat budget) {
  status UNMARKED = global.UNMARKED;
  status MARKED = global.MARKED;
  value* stack = Caml_state->mark_stack;
  uint64_t stack_count = Caml_state->mark_stack_count;
  uintnat blocks_marked = 0;

  while (budget > 0 && stack_count > 0) {
    value v = stack[--stack_count];
    header_t hd_v;

    CAMLassert(Is_markable(v));
    CAMLassert(Tag_val(v) != Infix_tag);

    blocks_marked++;
    /* mark the current object */
    hd_v = Hd_val(v);
    if (Tag_hd (hd_v) == Stack_tag) {
      Caml_state->mark_stack_count = stack_count;
      caml_darken_stack(v);
      stack = Caml_state->mark_stack;
      stack_count = Caml_state->mark_stack_count;
    } else if (Tag_hd (hd_v) < No_scan_tag) {
      int i;
      for (i = 0; i < Wosize_hd(hd_v); i++) {
        value child = Op_val(v)[i];
        /* FIXME: this is wrong, as Debug_tag(N) is a valid value.
           However, it's a useful debugging aid for now */
        CAMLassert(!Is_debug_tag(child) || child == Debug_uninit_major || child == Debug_uninit_minor);
        if (Is_markable(child)) {
          header_t hd = Hd_val(child);
          if (Tag_hd(hd) == Infix_tag) {
            child -= Infix_offset_hd(hd);
            hd = Hd_val(child);
          }
          /* FIXME: short-circuit Forward_tag here? */
          CAMLassert (!Has_status_hd(hd, global.GARBAGE));
          if (Has_status_hd(hd, UNMARKED)) {
            Hd_val(child) = With_status_hd(hd, MARKED);
            if (stack_count >= MARK_STACK_SIZE) {
              Caml_state->mark_stack_count = stack_count;
              mark_stack_prune();
              stack = Caml_state->mark_stack;
              stack_count = Caml_state->mark_stack_count;
            }
            stack[stack_count++] = child;
          }
        }
      }
    }
    budget -= Whsize_hd(hd_v);
  }
  Caml_state->stat_blocks_marked += blocks_marked;
  Caml_state->mark_stack_count = stack_count;
  return budget;
}

/* mark until the budget runs out or marking is done */
static intnat mark(intnat budget) {
  while (budget > 0 && !Caml_state->marking_done) {
    budget = do_some_marking(budget);
    if (budget > 0) {
      struct pool* p = find_pool_to_rescan();
      if (p) {
        caml_redarken_pool(p, &mark_stack_push_act, 0);
      } else {
        Caml_state->marking_done = 1;
        atomic_fetch_add(&num_domains_to_mark, -1);
      }
    }
  }
  return budget;
}

void caml_darken(void* state, value v, value* ignored) {
  header_t hd;
  /* CAMLassert (Is_markable(v)); */
  if (!Is_markable (v)) return; /* foreign stack, at least */

  hd = Hd_val(v);
  if (Tag_hd(hd) == Infix_tag) {
    v -= Infix_offset_hd(hd);
    hd = Hd_val(v);
  }
  if (Has_status_hd(hd, global.UNMARKED)) {
    if (Caml_state->marking_done) {
      atomic_fetch_add(&num_domains_to_mark, 1);
      Caml_state->marking_done = 0;
    }
    Hd_val(v) = With_status_hd(hd, global.MARKED);
    mark_stack_push(v);
  }
}


/* double-buffered sampled GC stats.
   At the end of GC cycle N, domains update sampled_gc_stats[N&1],
   but requests to Gc.stats() read from sampled_gc_stats[!(N&1)].
   That way, Gc.stats() returns the statistics atomically sampled
   at the end of the most recently completed GC cycle */
static struct gc_stats sampled_gc_stats[2][Max_domains];

void caml_accum_heap_stats(struct heap_stats* acc, const struct heap_stats* h)
{
  acc->pool_words += h->pool_words;
  if (acc->pool_max_words < h->pool_max_words)
    acc->pool_max_words = h->pool_max_words;
  acc->pool_live_words += h->pool_live_words;
  acc->pool_live_blocks += h->pool_live_blocks;
  acc->pool_frag_words += h->pool_frag_words;
  acc->large_words += h->large_words;
  if (acc->large_max_words < h->large_max_words)
    acc->large_max_words = h->large_max_words;
  acc->large_blocks += h->large_blocks;
}

void caml_sample_gc_stats(struct gc_stats* buf)
{
  memset(buf, 0, sizeof(*buf));
  /* we read from the buffers that are not currently being
     written to. that way, we pick up the numbers written
     at the end of the most recently completed GC cycle */
  int phase = ! (major_cycles_completed & 1);
  int i;
  intnat pool_max = 0, large_max = 0;
  for (i=0; i<Max_domains; i++) {
    struct gc_stats* s = &sampled_gc_stats[phase][i];
    struct heap_stats* h = &s->major_heap;
    buf->minor_words += s->minor_words;
    buf->promoted_words += s->promoted_words;
    buf->major_words += s->major_words;
    buf->minor_collections += s->minor_collections;
    /* The instantaneous maximum heap size cannot be computed
       from per-domain statistics, and would be very expensive
       to maintain directly. Here, we just sum the per-domain
       maxima, which is statistically dubious.

       FIXME: maybe maintain coarse global maxima? */
    pool_max += h->pool_max_words;
    large_max += h->large_max_words;
    caml_accum_heap_stats(&buf->major_heap, h);
  }
  buf->major_heap.pool_max_words = pool_max;
  buf->major_heap.large_max_words = large_max;
}

static void major_cycle_callback(struct domain* domain, void* unused)
{
  uintnat num_domains_in_stw;

  CAMLassert(domain == caml_domain_self());

  caml_gc_log("In STW callback");

  /* finish GC */
  caml_finish_sweeping();
  caml_empty_minor_heap();
  caml_finish_marking();

  {
    /* update GC stats */
    int stats_phase = major_cycles_completed & 1;
    struct gc_stats* stats = &sampled_gc_stats[stats_phase][domain->state->id];
    stats->minor_words = domain->state->stat_minor_words;
    stats->promoted_words = domain->state->stat_promoted_words;
    stats->major_words = domain->state->stat_major_words;
    stats->minor_collections = domain->state->stat_minor_collections;
    caml_sample_heap_stats(domain->state->shared_heap, &stats->major_heap);
  }

  {
    /* Cycle major heap */
    // FIXME: delete caml_cycle_heap_stw and have per-domain copies of the data?
    barrier_status b = caml_global_barrier_begin();
    if (caml_global_barrier_is_final(b)) {
      caml_cycle_heap_stw();
      /* FIXME: Maybe logging outside the barrier would be better */
      caml_gc_log("GC cycle %lu completed (heap cycled)", (long unsigned int)major_cycles_completed);
      caml_ev_msg("GC cycle completed");
      major_cycles_completed++;

      num_domains_in_stw = (uintnat)caml_global_barrier_num_domains();
      atomic_store_rel(&num_domains_to_sweep, num_domains_in_stw);
      atomic_store_rel(&num_domains_to_mark, num_domains_in_stw);
    }
    // should interrupts be processed here or not?
    // depends on whether marking above may need interrupts
    caml_global_barrier_end(b);
  }

  /* If the heap is to be verified, do it before the domains continue
     running OCaml code. */
  if (caml_params->verify_heap) {
    struct heap_verify_state* ver = caml_verify_begin();
    caml_do_local_roots(&caml_verify_root, ver, caml_domain_self());
    caml_scan_stack(&caml_verify_root, ver, Caml_state->current_stack);
    caml_scan_global_roots(&caml_verify_root, ver);
    caml_verify_heap(ver);
    caml_gc_log("Heap verified");
    caml_global_barrier();
  }

  domain->state->stat_major_collections++;
  caml_cycle_heap(domain->state->shared_heap);
  domain->state->sweeping_done = 0;

  /* Mark roots for new cycle */
  domain->state->marking_done = 0;

  caml_ev_start_gc();
  caml_ev_msg("Start marking roots");
  caml_do_local_roots(&caml_darken, 0, caml_domain_self());
  caml_scan_stack(&caml_darken, 0, Caml_state->current_stack);
  caml_scan_global_roots(&caml_darken, 0);
  caml_ev_msg("End marking roots");
  caml_ev_end_gc();

  if (domain->state->mark_stack_count == 0) {
    atomic_fetch_add(&num_domains_to_mark, -1);
    domain->state->marking_done = 1;
  }
}

void caml_finish_major_cycle() {
  uintnat cycle = major_cycles_completed;
  /* To handle the case where multiple domains try to finish the major
     cycle simultaneously, we loop until the current cycle has ended,
     ignoring whether caml_try_run_on_all_domains succeeds. */
  while (cycle == major_cycles_completed) {
    caml_try_run_on_all_domains(&major_cycle_callback, 0);
  }
}

#define Chunk_size 0x4000

intnat caml_major_collection_slice(intnat howmuch, intnat* budget_left /* out */)
{
  caml_domain_state* domain_state = Caml_state;
  intnat computed_work = howmuch ? howmuch : default_slice_budget();
  intnat budget = computed_work;
  intnat sweep_work = 0, mark_work = 0;
  intnat available, left;
  uintnat blocks_marked_before = domain_state->stat_blocks_marked;
  int steal_result;
  int was_marking = 0;

  caml_save_stack_gc();
  caml_ev_start_gc();

  if (!domain_state->sweeping_done) {
    caml_ev_msg("Start sweeping");

    sweep_work = budget;
    do {
      available = budget > Chunk_size ? Chunk_size : budget;
      left = caml_sweep(domain_state->shared_heap, available);
      budget -= available - left;
      caml_handle_incoming_interrupts();
    } while (budget > 0 && available != left);

    if (budget > 0) {
      domain_state->sweeping_done = 1;
      atomic_fetch_add(&num_domains_to_sweep, -1);
    }

    caml_ev_msg("End sweeping");

    caml_handle_incoming_interrupts();
  }

  mark_work = budget;
  while (budget > 0) {
    if (!domain_state->marking_done) {
      if (!was_marking) {
        caml_ev_msg("Start marking");
        was_marking = 1;
      }
      available = budget > Chunk_size ? Chunk_size : budget;
      left = mark(available);
      budget -= available - left;
      caml_handle_incoming_interrupts();
    } else if (0) {
      if (was_marking) {
        caml_ev_msg("End marking");
        was_marking = 0;
      }
      caml_ev_end_gc();
      caml_ev_msg("Start stealing");
      steal_result = steal_mark_work();
      caml_ev_msg("Finish stealing from domain %d size=%lu",
                  steal_result, domain_state->mark_stack_count);
      caml_ev_start_gc();
      if (steal_result == -1) break;
    } else {
      break;
    }
  }
  if (was_marking)
    caml_ev_msg("End marking");
  mark_work -= budget;
  caml_ev_end_gc();

  if (budget_left)
    *budget_left = budget;

  caml_gc_log("Major slice: %lu alloc, %ld work, %ld sweep, %ld mark (%lu blocks)",
              (unsigned long)domain_state->allocated_words,
              (long)computed_work, (long)sweep_work, (long)mark_work,
              (unsigned long)(domain_state->stat_blocks_marked - blocks_marked_before));
  domain_state->stat_major_words += domain_state->allocated_words;
  domain_state->allocated_words = 0;

  caml_restore_stack_gc();

  if (atomic_load_acq(&num_domains_to_sweep) == 0 &&
      atomic_load_acq(&num_domains_to_mark) == 0) {
    caml_finish_major_cycle();
  }

  return computed_work;
}

void caml_empty_mark_stack () {
  while (!Caml_state->marking_done)
    mark(10000000);

  if (Caml_state->stat_blocks_marked)
    caml_gc_log("Finished marking major heap. Marked %u blocks",
                (unsigned)Caml_state->stat_blocks_marked);
  Caml_state->stat_blocks_marked = 0;
}

void caml_finish_marking () {
  if (!Caml_state->marking_done) {
    caml_ev_start_gc();
    caml_save_stack_gc();
    caml_empty_mark_stack();
    Caml_state->stat_major_words += Caml_state->allocated_words;
    Caml_state->allocated_words = 0;
    caml_restore_stack_gc();
    caml_ev_msg("Mark stack empty");
    caml_ev_end_gc();
  }
}

void caml_finish_sweeping () {
  if (!Caml_state->sweeping_done) {
    caml_ev_start_gc();
    while (caml_sweep(Caml_state->shared_heap, 10) <= 0);
    Caml_state->sweeping_done = 1;
    atomic_fetch_add(&num_domains_to_sweep, -1);
    caml_ev_end_gc();
  }
}

static struct pool** pools_to_rescan;
static int pools_to_rescan_count;
static int pools_to_rescan_len;
static caml_plat_mutex pools_to_rescan_lock = CAML_PLAT_MUTEX_INITIALIZER;

static struct pool* find_pool_to_rescan()
{
  struct pool* p;
  caml_plat_lock(&pools_to_rescan_lock);
  if (pools_to_rescan_count > 0) {
    p = pools_to_rescan[--pools_to_rescan_count];
    caml_gc_log("Redarkening pool %p (%d others left)", p, pools_to_rescan_count);
  } else {
    p = 0;
  }
  caml_plat_unlock(&pools_to_rescan_lock);
  return p;
}

struct pool_count {
  struct pool* pool;
  int occurs;
};

static int pool_count_cmp(const void* a, const void* b)
{
  const struct pool_count* p = a;
  const struct pool_count* q = b;
  return p->occurs - q->occurs;
}

static void mark_stack_prune ()
{
  struct addrmap t = ADDRMAP_INIT;
  int count = 0, entry;
  caml_domain_state* domain_state = Caml_state;
  addrmap_iterator i;
  uintnat mark_stack_count = domain_state->mark_stack_count;
  value* mark_stack = domain_state->mark_stack;

  /* space used by the computations below */
  uintnat table_max = mark_stack_count / 100;
  if (table_max < 1000) table_max = 1000;

  /* amount of space we want to free up */
  int entries_to_free = (uintnat)(mark_stack_count * 0.20);

  /* We compress the mark stack by removing all of the objects from a
     subset of pools, which are rescanned later. For efficiency, we
     want to select those pools which occur most frequently, so that
     we need to rescan as few pools as possible. However, we do not
     have space to build a complete histogram.

     Using ~1% of the mark stack's space, we can find all of the
     elements that occur at least 100 times using the Misra-Gries
     heavy hitter algorithm (see J. Misra and D. Gries, "Finding
     repeated elements", 1982). */

  for (entry = 0; entry < mark_stack_count; entry++) {
    struct pool* pool = caml_pool_of_shared_block(mark_stack[entry]);
    if (!pool) continue;
    value p = (value)pool;
    if (caml_addrmap_contains(&t, p)) {
      /* if it's already present, increase the count */
      (*caml_addrmap_insert_pos(&t, p)) ++;
    } else if (count < table_max) {
      /* if there's space, insert it with count 1 */
      *caml_addrmap_insert_pos(&t, p) = 1;
      count++;
    } else {
      /* otherwise, decrease all entries by 1 */
      struct addrmap s = ADDRMAP_INIT;
      int scount = 0;
      for (i = caml_addrmap_iterator(&t);
           caml_addrmap_iter_ok(&t, i);
           i = caml_addrmap_next(&t, i)) {
        value k = caml_addrmap_iter_key(&t, i);
        value v = caml_addrmap_iter_value(&t, i);
        if (v > 1) {
          *caml_addrmap_insert_pos(&s, k) = v - 1;
          scount++;
        }
      }
      caml_addrmap_clear(&t);
      t = s;
      count = scount;
    }
  }

  /* t now contains all pools that occur at least 100 times.
     If no pools occur at least 100 times, t is some arbitrary subset of pools.
     Next, we get an accurate count of the occurrences of the pools in t */

  for (i = caml_addrmap_iterator(&t);
       caml_addrmap_iter_ok(&t, i);
       i = caml_addrmap_next(&t, i)) {
    *caml_addrmap_iter_val_pos(&t, i) = 0;
  }
  for (entry = 0; entry < mark_stack_count; entry++) {
    value p = (value)caml_pool_of_shared_block(mark_stack[entry]);
    if (p && caml_addrmap_contains(&t, p))
      (*caml_addrmap_insert_pos(&t, p))++;
  }

  /* Next, find a subset of those pools that covers enough entries */

  struct pool_count* pools = caml_stat_alloc(count * sizeof(struct pool_count));
  int pos = 0;
  for (i = caml_addrmap_iterator(&t);
       caml_addrmap_iter_ok(&t, i);
       i = caml_addrmap_next(&t, i)) {
    struct pool_count* p = &pools[pos++];
    p->pool = (struct pool*)caml_addrmap_iter_key(&t, i);
    p->occurs = (int)caml_addrmap_iter_value(&t, i);
  }
  CAMLassert(pos == count);
  caml_addrmap_clear(&t);

  qsort(pools, count, sizeof(struct pool_count), &pool_count_cmp);

  int start = count, total = 0;
  while (start > 0 && total < entries_to_free) {
    start--;
    total += pools[start].occurs;
  }



  for (i = start; i < count; i++) {
    *caml_addrmap_insert_pos(&t, (value)pools[i].pool) = 1;
  }
  int out = 0;
  for (entry = 0; entry < mark_stack_count; entry++) {
    value v = mark_stack[entry];
    value p = (value)caml_pool_of_shared_block(v);
    if (!(p && caml_addrmap_contains(&t, p))) {
      mark_stack[out++] = v;
    }
  }
  domain_state->mark_stack_count = out;

  caml_gc_log("Mark stack overflow. Postponing %d pools (%.1f%%, leaving %d).",
              count-start, 100. * (double)total / (double)mark_stack_count,
              (int)domain_state->mark_stack_count);


  /* Add the pools to rescan to the global list.

     This must be done after the mark stack is filtered, since other
     threads race to remove pools from the global list. As soon as
     pools_to_rescan_lock is released, we cannot rely on pools being
     in the global list. */

  caml_plat_lock(&pools_to_rescan_lock);
  for (i = start; i < count; i++) {
    if (pools_to_rescan_count == pools_to_rescan_len) {
      pools_to_rescan_len = pools_to_rescan_len * 2 + 128;
      pools_to_rescan =
        caml_stat_resize(pools_to_rescan, pools_to_rescan_len * sizeof(struct pool*));
    }
    pools_to_rescan[pools_to_rescan_count++] = pools[i].pool;
  }
  caml_plat_unlock(&pools_to_rescan_lock);
}

void caml_init_major_gc() {
  Caml_state->mark_stack = caml_stat_alloc(MARK_STACK_SIZE * sizeof(value));
  Caml_state->mark_stack_count = 0;
  /* Fresh domains do not need to performing marking or sweeping. */
  Caml_state->sweeping_done = 1;
  Caml_state->marking_done = 1;
  Caml_state->stealing = 0;
}

void caml_teardown_major_gc() {
  CAMLassert(Caml_state->mark_stack_count == 0);
  caml_stat_free(Caml_state->mark_stack);
  Caml_state->mark_stack = NULL;
}

void caml_finalise_heap (void)
{
#if 0
  /* Finishing major cycle (all values become white) */
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  CAMLassert (caml_gc_phase == Phase_idle);

  /* Finalising all values (by means of forced sweeping) */
  caml_fl_init_merge ();
  caml_gc_phase = Phase_sweep;
  chunk = caml_heap_start;
  caml_gc_sweep_hp = chunk;
  limit = chunk + Chunk_size (chunk);
  while (caml_gc_phase == Phase_sweep)
    sweep_slice (LONG_MAX);
#endif
}
