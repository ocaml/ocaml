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
#include <string.h>
#include <math.h>

#include "caml/addrmap.h"
#include "caml/config.h"
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/gc_stats.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/shared_heap.h"
#include "caml/startup_aux.h"
#include "caml/weak.h"

/* NB the MARK_STACK_INIT_SIZE must be larger than the number of objects
   that can be in a pool, see POOL_WSIZE */
#define MARK_STACK_INIT_SIZE (1 << 12)

/* The mark stack contains spans of fields that need to be marked.
   The mark stack is bounded relative to the heap size, if the mark stack
   overflows the bound, then entries are compressed into entries
   (k, bitfield) where the bitfield represents word offsets from k.
   When the mark stack is empty, compressed entries (if any) are taken
   from the compressed stack and processed. */

typedef struct {
  value* start;
  value* end;
} mark_entry; /* represents fields in the span [start, end) */

struct mark_stack {
  mark_entry* stack;
  uintnat count;
  uintnat size;
  struct addrmap compressed_stack;
  addrmap_iterator compressed_stack_iter;
};

uintnat caml_percent_free = Percent_free_def;

/* This variable is only written with the world stopped,
   so it need not be atomic */
uintnat caml_major_cycles_completed = 0;

static atomic_uintnat num_domains_to_sweep;
static atomic_uintnat num_domains_to_mark;
static atomic_uintnat num_domains_to_ephe_sweep;
static atomic_uintnat num_domains_to_final_update_first;
static atomic_uintnat num_domains_to_final_update_last;

static atomic_uintnat terminated_domains_allocated_words;

enum global_roots_status{
  WORK_UNSTARTED,
  WORK_STARTED
};
static atomic_uintnat domain_global_roots_started;

gc_phase_t caml_gc_phase;

extern value caml_ephe_none; /* See weak.c */

static struct ephe_cycle_info_t {
  atomic_uintnat num_domains_todo;
  /* Number of domains that need to scan their ephemerons in the current major
   * GC cycle. This field is decremented when ephe_info->todo list at a domain
   * becomes empty.  */
  atomic_uintnat ephe_cycle;
  /* Ephemeron cycle count */
  atomic_uintnat num_domains_done;
  /* Number of domains that have marked their ephemerons in the current
   * ephemeron cycle. */
} ephe_cycle_info;
  /* In the first major cycle, there is no ephemeron marking to be done. */

/* ephe_cycle_info is always updated with the critical section protected by
 * ephe_lock or in the global barrier. However, the fields may be read without
 * the lock. */
static caml_plat_mutex ephe_lock = CAML_PLAT_MUTEX_INITIALIZER;

static void ephe_next_cycle ()
{
  caml_plat_lock(&ephe_lock);
  atomic_fetch_add(&ephe_cycle_info.ephe_cycle, +1);
  CAMLassert(atomic_load_acq(&ephe_cycle_info.num_domains_done) <=
             atomic_load_acq(&ephe_cycle_info.num_domains_todo));
  atomic_store(&ephe_cycle_info.num_domains_done, 0);
  caml_plat_unlock(&ephe_lock);
}

void caml_ephe_todo_list_emptied (void)
{
  caml_plat_lock(&ephe_lock);

  /* Force next ephemeron marking cycle in order to avoid reasoning about
   * whether the domain has already incremented
   * [ephe_cycle_info.num_domains_done] counter. */
  atomic_store(&ephe_cycle_info.num_domains_done, 0);
  atomic_fetch_add(&ephe_cycle_info.ephe_cycle, +1);
  atomic_fetch_add(&ephe_cycle_info.num_domains_todo, -1);
  atomic_fetch_add_verify_ge0(&num_domains_to_ephe_sweep, -1);
  CAMLassert(atomic_load_acq(&ephe_cycle_info.num_domains_done) <=
             atomic_load_acq(&ephe_cycle_info.num_domains_todo));

  caml_plat_unlock(&ephe_lock);
}

/* Record that ephemeron marking was done for the given ephemeron cycle. */
static void record_ephe_marking_done (uintnat ephe_cycle)
{
  CAMLassert (ephe_cycle <= atomic_load_acq(&ephe_cycle_info.ephe_cycle));
  CAMLassert (Caml_state->marking_done);

  if (ephe_cycle < atomic_load_acq(&ephe_cycle_info.ephe_cycle))
    return;

  caml_plat_lock(&ephe_lock);
  if (ephe_cycle == atomic_load(&ephe_cycle_info.ephe_cycle)) {
    Caml_state->ephe_info->cycle = ephe_cycle;
    atomic_fetch_add(&ephe_cycle_info.num_domains_done, +1);
    CAMLassert(atomic_load_acq(&ephe_cycle_info.num_domains_done) <=
               atomic_load_acq(&ephe_cycle_info.num_domains_todo));
  }
  caml_plat_unlock(&ephe_lock);
}

/* These are biased data structures left over from terminating domains. */
static struct {
  value ephe_list_live;
  struct caml_final_info *final_info;
} orph_structs = {0, 0};

static caml_plat_mutex orphaned_lock = CAML_PLAT_MUTEX_INITIALIZER;

void caml_add_orphaned_finalisers (struct caml_final_info* f)
{
  CAMLassert (caml_gc_phase == Phase_sweep_and_mark_main);
  CAMLassert (!f->updated_first);
  CAMLassert (!f->updated_last);

  caml_plat_lock(&orphaned_lock);
  f->next = orph_structs.final_info;
  orph_structs.final_info = f;
  caml_plat_unlock(&orphaned_lock);

}

/* Called by terminating domain from handover_finalisers */
void caml_final_domain_terminate (caml_domain_state *domain_state)
{
  struct caml_final_info *f = domain_state->final_info;
  if(!f->updated_first) {
    atomic_fetch_add_verify_ge0(&num_domains_to_final_update_first, -1);
    f->updated_first = 1;
  }
  if(!f->updated_last) {
    atomic_fetch_add_verify_ge0(&num_domains_to_final_update_last, -1);
    f->updated_last = 1;
  }
}

static int no_orphaned_work (void)
{
  return
    orph_structs.ephe_list_live == 0 &&
    orph_structs.final_info == NULL;
}

void caml_orphan_allocated_words (void)
{
  atomic_fetch_add(&terminated_domains_allocated_words,
                   Caml_state->allocated_words);
}

Caml_inline value ephe_list_tail(value e)
{
  value last = 0;
  while (e != 0) {
    CAMLassert (Tag_val(e) == Abstract_tag);
    last = e;
    e = Ephe_link(e);
  }
  return last;
}

#ifdef DEBUG
static void orph_ephe_list_verify_status (int status)
{
  value v;

  caml_plat_lock(&orphaned_lock);

  v = orph_structs.ephe_list_live;
  while (v) {
    CAMLassert (Tag_val(v) == Abstract_tag);
    CAMLassert (Has_status_hd (Hd_val(v), status));
    v = Ephe_link(v);
  }

  caml_plat_unlock(&orphaned_lock);
}
#endif

#define EPHE_MARK_DEFAULT 0
#define EPHE_MARK_FORCE_ALIVE 1

static intnat ephe_mark (intnat budget, uintnat for_cycle, int force_alive);

void caml_add_to_orphaned_ephe_list(struct caml_ephe_info* ephe_info)
{
  caml_plat_lock(&orphaned_lock);

  /* Force all ephemerons and their data on todo list to be alive */
  if (ephe_info->todo) {
    while (ephe_info->todo) {
      ephe_mark (100000, 0, EPHE_MARK_FORCE_ALIVE);
    }
    caml_ephe_todo_list_emptied ();
  }
  CAMLassert (ephe_info->todo == 0);

  if (ephe_info->live) {
    value live_tail = ephe_list_tail(ephe_info->live);
    CAMLassert(Ephe_link(live_tail) == 0);
    Ephe_link(live_tail) = orph_structs.ephe_list_live;
    orph_structs.ephe_list_live = ephe_info->live;
    ephe_info->live = 0;
  }

  caml_plat_unlock(&orphaned_lock);
}

void caml_adopt_orphaned_work (void)
{
  caml_domain_state* domain_state = Caml_state;
  value last;
  struct caml_final_info *f, *myf, *temp;

  if (no_orphaned_work() || caml_domain_is_terminating())
    return;

  caml_plat_lock(&orphaned_lock);

  if (orph_structs.ephe_list_live) {
    last = ephe_list_tail(orph_structs.ephe_list_live);
    CAMLassert(Ephe_link(last) == 0);
    Ephe_link(last) = domain_state->ephe_info->live;
    domain_state->ephe_info->live = orph_structs.ephe_list_live;
    orph_structs.ephe_list_live = 0;
  }

  f = orph_structs.final_info;
  myf = domain_state->final_info;
  while (f != NULL) {
    CAMLassert (!f->updated_first);
    CAMLassert (!f->updated_last);
    CAMLassert (!myf->updated_first);
    CAMLassert (!myf->updated_last);
    CAMLassert (caml_gc_phase == Phase_sweep_and_mark_main);
    if (f->todo_head) {
      if (myf->todo_tail == NULL) {
        CAMLassert(myf->todo_head == NULL);
        myf->todo_head = f->todo_head;
        myf->todo_tail = f->todo_tail;
      } else {
        myf->todo_tail->next = f->todo_head;
        myf->todo_tail = f->todo_tail;
      }
    }
    if (f->first.young > 0) {
      caml_final_merge_finalisable (&f->first, &myf->first);
    }
    if (f->last.young > 0) {
      caml_final_merge_finalisable (&f->last, &myf->last);
    }
    temp = f;
    f = f->next;
    caml_stat_free (temp);
  }
  orph_structs.final_info = NULL;
  caml_plat_unlock(&orphaned_lock);
}

#define BUFFER_SIZE 64

struct buf_list_t {
  double buffer[BUFFER_SIZE];
  struct buf_list_t *next;
};

static struct {
  intnat heap_words_last_cycle;
  intnat not_garbage_words_last_cycle;
  int index;
  struct buf_list_t *l;
 } caml_stat_space_overhead = {0, 0, 0, NULL};

double caml_mean_space_overhead (void)
{
  int index = caml_stat_space_overhead.index;
  struct buf_list_t *t, *l = caml_stat_space_overhead.l;
  /* Use Welford's online algorithm for calculating running variance to remove
   * outliers from mean calculation. */
  double mean = 0.0, m2 = 0.0, stddev = 0.0, v;
  double delta, delta2;
  intnat count = 0;

  while (l) {
    while (index > 0) {
      v = l->buffer[--index];
      if (count > 5 && (v < mean - 3 * stddev || v > mean + 3 * stddev)) {
        continue;
      }
      count++;
      delta = v - mean;
      mean = mean + delta / count;
      delta2 = v - mean;
      m2 = m2 + delta * delta2;
      stddev = sqrt (m2 / count);
    }
    t = l;
    l = l->next;
    caml_stat_free(t);
    index = BUFFER_SIZE;
  }
  return mean;
}

static void update_major_slice_work() {
  double p, dp, heap_words;
  intnat computed_work;
  caml_domain_state *dom_st = Caml_state;
  uintnat heap_size, heap_sweep_words, saved_terminated_words;
  /*
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = heap_words * caml_percent_free
                      / (100 + caml_percent_free)

     Assuming steady state and enforcing a constant allocation rate, then
     FM is divided in 2/3 for garbage and 1/3 for free list.
              G = 2 * FM / 3
     G is also the amount of memory that will be used during this cycle
     (still assuming steady state).

     Proportion of G consumed since the previous slice:
              PH = dom_st->allocated_words / G
                = dom_st->allocated_words * 3 * (100 + caml_percent_free)
                  / (2 * heap_words * caml_percent_free)
     Proportion of extra-heap resources consumed since the previous slice:
              PE = dom_st->extra_heap_resources
     Proportion of total work to do in this slice:
              P  = max (PH, PE)
     Amount of marking work for the GC cycle:
              MW = heap_words * 100 / (100 + caml_percent_free)
     Amount of sweeping work for the GC cycle:
              SW = heap_sweep_words
     Amount of total work for the GC cycle:
              TW = MW + SW
              = heap_words * 100 / (100 + caml_percent_free) + heap_sweep_words

     Amount of time to spend on this slice:
                 T = P * TT

     Since we must do TW amount of work in TT time, the amount of work done
     for this slice is:
                 S = P * TW
  */
  heap_size = caml_heap_size(dom_st->shared_heap);
  heap_words = (double)Wsize_bsize(heap_size);
  heap_sweep_words = heap_words;

  saved_terminated_words = terminated_domains_allocated_words;
  if( saved_terminated_words > 0 ) {
    while(!atomic_compare_exchange_strong
            (&terminated_domains_allocated_words, &saved_terminated_words, 0));
  }

  if (heap_words > 0) {
    p = (double) (saved_terminated_words
                + dom_st->allocated_words) * 3.0 * (100 + caml_percent_free)
                / heap_words / caml_percent_free / 2.0;
  } else {
    p = 0.0;
  }

  if (dom_st->dependent_size > 0) {
    dp = (double) dom_st->dependent_allocated * (100 + caml_percent_free)
         / dom_st->dependent_size / caml_percent_free;
  }else{
    dp = 0.0;
  }
  if (p < dp) p = dp;

  if (p < dom_st->extra_heap_resources) p = dom_st->extra_heap_resources;

  computed_work = (intnat) (p * (heap_sweep_words
                            + (heap_words * 100 / (100 + caml_percent_free))));

  /* accumulate work */
  dom_st->major_work_computed += computed_work;
  dom_st->major_work_todo += computed_work;

  caml_gc_message (0x40, "heap_words = %"
                         ARCH_INTNAT_PRINTF_FORMAT "u\n",
                   (uintnat)heap_words);
  caml_gc_message (0x40, "allocated_words = %"
                         ARCH_INTNAT_PRINTF_FORMAT "u\n",
                   dom_st->allocated_words);
  caml_gc_message (0x40, "raw work-to-do = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (p * 1000000));
  caml_gc_message (0x40, "extra_heap_resources = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (dom_st->extra_heap_resources * 1000000));
  caml_gc_message (0x40, "computed work = %"
                         ARCH_INTNAT_PRINTF_FORMAT "d words\n",
                   computed_work);

  caml_gc_log("Updated major work: [%c] "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "u heap_words, "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "u allocated, "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "d computed_work, "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "d work_computed, "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "d work_todo, "
                         " %"ARCH_INTNAT_PRINTF_FORMAT "u gc_clock",
                         caml_gc_phase_char(caml_gc_phase),
                         (uintnat)heap_words, dom_st->allocated_words,
                         computed_work,
                         dom_st->major_work_computed,
                         dom_st->major_work_todo,
                         (intnat)(dom_st->major_gc_clock*1000000));

  dom_st->stat_major_words += dom_st->allocated_words;
  dom_st->allocated_words = 0;
  dom_st->dependent_allocated = 0;
  dom_st->extra_heap_resources = 0.0;
}

static intnat get_major_slice_work(intnat howmuch) {
  caml_domain_state *dom_st = Caml_state;
  intnat computed_work;

  /* calculate how much work to do now */
  if (howmuch == AUTO_TRIGGERED_MAJOR_SLICE ||
      howmuch == GC_CALCULATE_MAJOR_SLICE) {
    computed_work = (dom_st->major_work_todo > 0)
      ? dom_st->major_work_todo
      : 0;

    /* cap computed_work to 0.3 */
    {
      uintnat heap_size = caml_heap_size(dom_st->shared_heap);
      uintnat heap_words = (double)Wsize_bsize(heap_size);
      uintnat heap_sweep_words = heap_words;
      intnat limit = (intnat)(0.3 * (heap_sweep_words
                            + (heap_words * 100 / (100 + caml_percent_free))));

      if (computed_work > limit)
      {
        computed_work = limit;
      }
    }
  } else {
    /* forced or opportunistic GC slice with explicit quantity */
    computed_work = howmuch;
  }

  /* TODO: do we want to do anything more complex or simplify the above? */

  return computed_work;
}

static void commit_major_slice_work(intnat words_done) {
  caml_domain_state *dom_st = Caml_state;
  intnat limit;

  dom_st->major_work_todo -= words_done;

  /* cap how far work todo can be in credit */
  limit = -2*Wsize_bsize(caml_heap_size(dom_st->shared_heap));
  if (dom_st->major_work_todo < limit)
  {
    dom_st->major_work_todo = limit;
  }

  /* check clock to close a cycle if need be */
  if (dom_st->major_work_todo <= 0
      && dom_st->major_gc_clock >= 1.0)
  {
    caml_gc_log("Major GC slice complete: "
        " %"ARCH_INTNAT_PRINTF_FORMAT "d words_done, "
        " %"ARCH_INTNAT_PRINTF_FORMAT "d todo, "
        " %"ARCH_INTNAT_PRINTF_FORMAT "d computed, "
        " %"ARCH_INTNAT_PRINTF_FORMAT "u clock",
        words_done,
        dom_st->major_work_todo,
        dom_st->major_work_computed,
        (uintnat)(dom_st->major_gc_clock * 1000000)
      );

    /* we have caught up */
    while( dom_st->major_gc_clock >= 1.0 ) {
      dom_st->major_gc_clock -= 1.;
    }

    /* limit amount of work credit that can go into next cycle */
    limit = -2*dom_st->major_work_computed;
    dom_st->major_work_todo = dom_st->major_work_todo < limit
      ? limit
      : dom_st->major_work_todo;
    dom_st->major_work_computed = 0;
  }
}

static void mark_stack_prune(struct mark_stack* stk);

#ifdef DEBUG
#define Is_markable(v) \
    (CAMLassert (v != Debug_free_major), \
     Is_block(v) && !Is_young(v))
#else
#define Is_markable(v) (Is_block(v) && !Is_young(v))
#endif

static void realloc_mark_stack (struct mark_stack* stk)
{
  mark_entry* new;
  uintnat mark_stack_bsize = stk->size * sizeof(mark_entry);

  if (mark_stack_bsize < caml_heap_size(Caml_state->shared_heap) / 32) {
    caml_gc_log ("Growing mark stack to %"ARCH_INTNAT_PRINTF_FORMAT"uk bytes\n",
                 (intnat) mark_stack_bsize * 2 / 1024);

    new = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                                2 * mark_stack_bsize);
    if (new != NULL) {
      stk->stack = new;
      stk->size *= 2;
      return;
    }
    caml_gc_log ("No room for growing mark stack. Pruning..\n");
  }

  caml_gc_log ("Mark stack size is %"ARCH_INTNAT_PRINTF_FORMAT"u "
               "bytes (> major heap size of this domain %"
               ARCH_INTNAT_PRINTF_FORMAT"u bytes / 32). Pruning..\n",
               mark_stack_bsize,
               caml_heap_size(Caml_state->shared_heap));
  mark_stack_prune(stk);
}

Caml_inline void mark_stack_push_range(struct mark_stack* stk,
                                       value* start, value* end)
{
  mark_entry* me;

  if (stk->count == stk->size)
    realloc_mark_stack(stk);

  me = &stk->stack[stk->count++];
  me->start = start;
  me->end = end;
}

/* returns the work done by skipping unmarkable objects */
static intnat mark_stack_push_block(struct mark_stack* stk, value block)
{
  int i, block_wsz = Wosize_val(block), end;
  uintnat offset = 0;

  if (Tag_val(block) == Closure_tag) {
    /* Skip the code pointers and integers at beginning of closure;
       start scanning at the first word of the environment part. */
    offset = Start_env_closinfo(Closinfo_val(block));

    CAMLassert(offset <= Wosize_val(block)
      && offset >= Start_env_closinfo(Closinfo_val(block)));
  }

  CAMLassert(Has_status_hd(Hd_val(block), caml_global_heap_state.MARKED));
  CAMLassert(Is_block(block) && !Is_young(block));
  CAMLassert(Tag_val(block) != Infix_tag);
  CAMLassert(Tag_val(block) < No_scan_tag);
  CAMLassert(Tag_val(block) != Cont_tag);

  /* Optimisation to avoid pushing small, unmarkable objects such as
     [Some 42] into the mark stack. */
  end = (block_wsz < 8 ? block_wsz : 8);

  for (i = offset; i < end; i++) {
    value v = Field(block, i);

    if (Is_markable(v))
      break;
  }

  if (i == block_wsz){
    /* nothing left to mark and credit header */
    return Whsize_wosize(block_wsz - offset);
  }

  mark_stack_push_range(stk,
                        Op_val(block) + i,
                        Op_val(block) + block_wsz);

  /* take credit for the work we skipped due to the optimisation.
     we will take credit for the header later as part of marking. */
  return i - offset;
}

/* This function shrinks the mark stack back to the MARK_STACK_INIT_SIZE size
   and is called at domain termination via caml_finish_marking. */
void caml_shrink_mark_stack (void)
{
  struct mark_stack* stk = Caml_state->mark_stack;
  intnat init_stack_bsize = MARK_STACK_INIT_SIZE * sizeof(mark_entry);
  mark_entry* shrunk_stack;

  caml_gc_log ("Shrinking mark stack to %"
                  ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                  init_stack_bsize / 1024);

  shrunk_stack = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                              init_stack_bsize);
  if (shrunk_stack != NULL) {
    stk->stack = shrunk_stack;
    stk->size = MARK_STACK_INIT_SIZE;
  }else{
    caml_gc_log ("Mark stack shrinking failed");
  }
}

void caml_darken_cont(value cont);

static void mark_slice_darken(struct mark_stack* stk, value child,
                              intnat* work)
{
  header_t chd;

  if (Is_markable(child)){
    chd = Hd_val(child);
    if (Tag_hd(chd) == Infix_tag) {
      child -= Infix_offset_hd(chd);
      chd = Hd_val(child);
    }
    CAMLassert(!Has_status_hd(chd, caml_global_heap_state.GARBAGE));
    if (Has_status_hd(chd, caml_global_heap_state.UNMARKED)){
      Caml_state->stat_blocks_marked++;
      if (Tag_hd(chd) == Cont_tag){
        caml_darken_cont(child);
        *work -= Wosize_hd(chd);
      } else {
    again:
        if (Tag_hd(chd) == Lazy_tag || Tag_hd(chd) == Forcing_tag){
          if(!atomic_compare_exchange_strong(Hp_atomic_val(child), &chd,
                With_status_hd(chd, caml_global_heap_state.MARKED))){
                  chd = Hd_val(child);
                  goto again;
          }
        } else {
          atomic_store_relaxed(
            Hp_atomic_val(child),
            With_status_hd(chd, caml_global_heap_state.MARKED));
        }
        if(Tag_hd(chd) < No_scan_tag){
          *work -= mark_stack_push_block(stk, child);
        } else {
          *work -= Wosize_hd(chd);
        }
      }
    }
  }
}

static intnat do_some_marking(struct mark_stack* stk, intnat budget) {
  while (stk->count > 0) {
    mark_entry me = stk->stack[--stk->count];
    while (me.start < me.end) {
      if (budget <= 0) {
        mark_stack_push_range(stk, me.start, me.end);
        return budget;
      }
      budget--;
      mark_slice_darken(stk, *me.start, &budget);
      me.start++;
    }
    budget--; /* credit for header */
  }
  return budget;
}

/* compressed mark stack */
#define PAGE_MASK (~(uintnat)(BITS_PER_WORD-1))
#define PTR_TO_PAGE(v) (((uintnat)(v)>>3) & PAGE_MASK)
#define PTR_TO_PAGE_OFFSET(v) ((((uintnat)(v)>>3) & ~PAGE_MASK))

/* mark until the budget runs out or marking is done */
static intnat mark(intnat budget) {
  caml_domain_state *domain_state = Caml_state;
  while (budget > 0 && !domain_state->marking_done) {
    budget = do_some_marking(domain_state->mark_stack, budget);
    if (budget > 0) {
      int i;
      struct mark_stack* mstk = domain_state->mark_stack;
      addrmap_iterator it = mstk->compressed_stack_iter;
      if (caml_addrmap_iter_ok(&mstk->compressed_stack, it)) {
        uintnat k = caml_addrmap_iter_key(&mstk->compressed_stack, it);
        value v = caml_addrmap_iter_value(&mstk->compressed_stack, it);

        /* NB: must update the iterator here, as possible that
           mark_slice_darken could lead to the mark stack being pruned
           and invalidation of the iterator */
        mstk->compressed_stack_iter =
                      caml_addrmap_next(&mstk->compressed_stack, it);

        for(i=0; i<BITS_PER_WORD; i++) {
          if(v & ((uintnat)1 << i)) {
            value* p = (value*)((k + i)<<3);
            mark_slice_darken(domain_state->mark_stack, *p, &budget);
          }
        }
      } else {
        ephe_next_cycle ();
        domain_state->marking_done = 1;
        atomic_fetch_add_verify_ge0(&num_domains_to_mark, -1);
      }
    }
  }
  return budget;
}

static scanning_action_flags darken_scanning_flags = 0;

void caml_darken_cont(value cont)
{
  CAMLassert(Is_block(cont) && !Is_young(cont) && Tag_val(cont) == Cont_tag);
  {
    SPIN_WAIT {
      header_t hd = atomic_load_relaxed(Hp_atomic_val(cont));
      CAMLassert(!Has_status_hd(hd, caml_global_heap_state.GARBAGE));
      if (Has_status_hd(hd, caml_global_heap_state.MARKED))
        break;
      if (Has_status_hd(hd, caml_global_heap_state.UNMARKED) &&
          atomic_compare_exchange_strong(
              Hp_atomic_val(cont), &hd,
              With_status_hd(hd, NOT_MARKABLE))) {
        value stk = Field(cont, 0);
        if (Ptr_val(stk) != NULL)
          caml_scan_stack(&caml_darken, darken_scanning_flags, Caml_state,
                          Ptr_val(stk), 0);
        atomic_store_explicit(
          Hp_atomic_val(cont),
          With_status_hd(hd, caml_global_heap_state.MARKED),
          memory_order_release);
      }
    }
  }
}

void caml_darken(void* state, value v, volatile value* ignored) {
  header_t hd;
  if (!Is_markable (v)) return; /* foreign stack, at least */

  hd = Hd_val(v);
  if (Tag_hd(hd) == Infix_tag) {
    v -= Infix_offset_hd(hd);
    hd = Hd_val(v);
  }
  if (Has_status_hd(hd, caml_global_heap_state.UNMARKED)) {
    caml_domain_state* domain_state = (caml_domain_state*)state;
    if (domain_state->marking_done) {
      atomic_fetch_add(&num_domains_to_mark, 1);
      domain_state->marking_done = 0;
    }
    if (Tag_hd(hd) == Cont_tag) {
      caml_darken_cont(v);
    } else {
      atomic_store_relaxed(
         Hp_atomic_val(v),
         With_status_hd(hd, caml_global_heap_state.MARKED));
      if (Tag_hd(hd) < No_scan_tag) {
        mark_stack_push_block(domain_state->mark_stack, v);
      }
    }
  }
}

static intnat ephe_mark (intnat budget, uintnat for_cycle,
                         /* Forces ephemerons and their data to be alive */
                         int force_alive)
{
  value v, data, key, f, todo;
  value* prev_linkp;
  header_t hd;
  mlsize_t size, i;
  caml_domain_state* domain_state = Caml_state;
  int alive_data;
  intnat marked = 0, made_live = 0;

  if (domain_state->ephe_info->cursor.cycle == for_cycle &&
      !force_alive) {
    prev_linkp = domain_state->ephe_info->cursor.todop;
    todo = *prev_linkp;
  } else {
    todo = domain_state->ephe_info->todo;
    prev_linkp = &domain_state->ephe_info->todo;
  }
  while (todo != 0 && budget > 0) {
    v = todo;
    todo = Ephe_link(v);
    CAMLassert (Tag_val(v) == Abstract_tag);
    hd = Hd_val(v);
    data = Ephe_data(v);
    alive_data = 1;

    if (force_alive)
      caml_darken (domain_state, v, 0);

    /* If ephemeron is unmarked, data is dead */
    if (is_unmarked(v)) alive_data = 0;

    size = Wosize_hd(hd);
    for (i = CAML_EPHE_FIRST_KEY; alive_data && i < size; i++) {
      key = Field(v, i);
    ephemeron_again:
      if (key != caml_ephe_none && Is_block(key)) {
        if (Tag_val(key) == Forward_tag) {
          f = Forward_val(key);
          if (Is_block(f)) {
            if (Tag_val(f) == Forward_tag || Tag_val(f) == Lazy_tag ||
                Tag_val(f) == Forcing_tag || Tag_val(f) == Double_tag) {
              /* Do not short-circuit the pointer */
            } else {
              Field(v, i) = key = f;
              goto ephemeron_again;
            }
          }
        }
        else {
          if (Tag_val (key) == Infix_tag) key -= Infix_offset_val (key);
          if (is_unmarked (key))
            alive_data = 0;
        }
      }
    }
    budget -= Whsize_wosize(i);

    if (force_alive || alive_data) {
      if (data != caml_ephe_none && Is_block(data)) {
        caml_darken (domain_state, data, 0);
      }
      Ephe_link(v) = domain_state->ephe_info->live;
      domain_state->ephe_info->live = v;
      *prev_linkp = todo;
      made_live++;
    } else {
      /* Leave this ephemeron on the todo list */
      prev_linkp = &Ephe_link(v);
    }
    marked++;
  }

  caml_gc_log
  ("Mark Ephemeron: %s. for ephemeron cycle=%"ARCH_INTNAT_PRINTF_FORMAT"d "
  "marked=%"ARCH_INTNAT_PRINTF_FORMAT"d made_live=%"ARCH_INTNAT_PRINTF_FORMAT"d"
  , domain_state->ephe_info->cursor.cycle == for_cycle
                              ? "continued from cursor" : "discarded cursor",
                              for_cycle, marked, made_live);

  domain_state->ephe_info->cursor.cycle = for_cycle;
  domain_state->ephe_info->cursor.todop = prev_linkp;

  return budget;
}

static intnat ephe_sweep (caml_domain_state* domain_state, intnat budget)
{
  value v;
  CAMLassert (caml_gc_phase == Phase_sweep_ephe);

  while (domain_state->ephe_info->todo != 0 && budget > 0) {
    v = domain_state->ephe_info->todo;
    domain_state->ephe_info->todo = Ephe_link(v);
    CAMLassert (Tag_val(v) == Abstract_tag);

    if (is_unmarked(v)) {
      /* The whole array is dead, drop this ephemeron */
      budget -= 1;
    } else {
      caml_ephe_clean(v);
      Ephe_link(v) = domain_state->ephe_info->live;
      domain_state->ephe_info->live = v;
      budget -= Whsize_val(v);
    }
  }
  return budget;
}


static void cycle_all_domains_callback(caml_domain_state* domain, void* unused,
                                       int participating_count,
                                       caml_domain_state** participating)
{
  uintnat num_domains_in_stw;

  CAML_EV_BEGIN(EV_MAJOR_GC_CYCLE_DOMAINS);

  CAMLassert(domain == Caml_state);
  CAMLassert(atomic_load_acq(&ephe_cycle_info.num_domains_todo) ==
             atomic_load_acq(&ephe_cycle_info.num_domains_done));
  CAMLassert(atomic_load(&num_domains_to_mark) == 0);
  CAMLassert(atomic_load(&num_domains_to_sweep) == 0);
  CAMLassert(atomic_load(&num_domains_to_ephe_sweep) == 0);

  caml_empty_minor_heap_no_major_slice_from_stw
                        (domain, (void*)0, participating_count, participating);

  CAML_EV_BEGIN(EV_MAJOR_GC_STW);

  {
    /* Cycle major heap */
    // FIXME: delete caml_cycle_heap_stw and have per-domain copies of the data?
    barrier_status b = caml_global_barrier_begin();
    if (caml_global_barrier_is_final(b)) {
      caml_cycle_heap_stw();
      caml_gc_log("GC cycle %lu completed (heap cycled)",
                  (long unsigned int)caml_major_cycles_completed);

      caml_major_cycles_completed++;
      caml_gc_message(0x40, "Starting major GC cycle\n");

      if (caml_params->verb_gc & 0x400) {
        struct gc_stats s;
        intnat heap_words, not_garbage_words, swept_words;

        caml_compute_gc_stats(&s);
        heap_words = s.heap_stats.pool_words + s.heap_stats.large_words;
        not_garbage_words = s.heap_stats.pool_live_words
                            + s.heap_stats.large_words;
        swept_words = domain->swept_words;
        caml_gc_log ("heap_words: %"ARCH_INTNAT_PRINTF_FORMAT"d "
                      "not_garbage_words %"ARCH_INTNAT_PRINTF_FORMAT"d "
                      "swept_words %"ARCH_INTNAT_PRINTF_FORMAT"d",
                      heap_words, not_garbage_words, swept_words);

        if (caml_stat_space_overhead.heap_words_last_cycle != 0) {
          /* At the end of a major cycle, no object has colour MARKED.

             [not_garbage_words] counts all objects which are UNMARKED.
             Importantly, this includes both live objects and objects which are
             unreachable in the current cycle (i.e, garbage). But we don't get
             to know which objects are garbage until the end of the next cycle.

             live_words@N = not_garbage_words@N - swept_words@N+1

             space_overhead@N =
                      100.0 * (heap_words@N - live_words@N) / live_words@N
            */
          double live_words_last_cycle =
            caml_stat_space_overhead.not_garbage_words_last_cycle - swept_words;
          double space_overhead =
            100.0 * (double)(caml_stat_space_overhead.heap_words_last_cycle
                            - live_words_last_cycle) / live_words_last_cycle;

          if (caml_stat_space_overhead.l == NULL ||
              caml_stat_space_overhead.index == BUFFER_SIZE) {
            struct buf_list_t *l =
              (struct buf_list_t*)
                  caml_stat_alloc_noexc(sizeof(struct buf_list_t));
            l->next = caml_stat_space_overhead.l;
            caml_stat_space_overhead.l = l;
            caml_stat_space_overhead.index = 0;
          }
          caml_stat_space_overhead.l->buffer[caml_stat_space_overhead.index++] =
            space_overhead;
          caml_gc_log("Previous cycle's space_overhead: %lf", space_overhead);
        }
        caml_stat_space_overhead.heap_words_last_cycle = heap_words;

        caml_stat_space_overhead.not_garbage_words_last_cycle
        = not_garbage_words;
      }
      domain->swept_words = 0;

      num_domains_in_stw = (uintnat)caml_global_barrier_num_domains();
      atomic_store_rel(&num_domains_to_sweep, num_domains_in_stw);
      atomic_store_rel(&num_domains_to_mark, num_domains_in_stw);

      caml_gc_phase = Phase_sweep_and_mark_main;
      atomic_store(&ephe_cycle_info.num_domains_todo, num_domains_in_stw);
      atomic_store(&ephe_cycle_info.ephe_cycle, 1);
      atomic_store(&ephe_cycle_info.num_domains_done, 0);
      atomic_store_rel(&num_domains_to_ephe_sweep, num_domains_in_stw);
      atomic_store_rel(&num_domains_to_final_update_first, num_domains_in_stw);
      atomic_store_rel(&num_domains_to_final_update_last, num_domains_in_stw);

      atomic_store(&domain_global_roots_started, WORK_UNSTARTED);

      /* Cleanups for various data structures that must be done in a STW by
        only a single domain */
      caml_code_fragment_cleanup();
    }
    // should interrupts be processed here or not?
    // depends on whether marking above may need interrupts
    caml_global_barrier_end(b);
  }

  /* If the heap is to be verified, do it before the domains continue
     running OCaml code. */
  if (caml_params->verify_heap) {
    caml_verify_heap(domain);
    caml_gc_log("Heap verified");
    caml_global_barrier();
  }

  caml_cycle_heap(domain->shared_heap);
  domain->sweeping_done = 0;

  /* Mark roots for new cycle */
  domain->marking_done = 0;
  domain->major_work_computed = 0;
  domain->major_work_todo = 0;
  domain->major_gc_clock = 0.0;

  CAML_EV_BEGIN(EV_MAJOR_MARK_ROOTS);
  caml_do_roots (&caml_darken, darken_scanning_flags, domain, domain, 0);
  {
    uintnat work_unstarted = WORK_UNSTARTED;
    if(atomic_compare_exchange_strong(&domain_global_roots_started,
                                      &work_unstarted,
                                      WORK_STARTED)){
        caml_scan_global_roots(&caml_darken, domain);
    }
  }
  CAML_EV_END(EV_MAJOR_MARK_ROOTS);

  if (domain->mark_stack->count == 0 &&
      !caml_addrmap_iter_ok(&domain->mark_stack->compressed_stack,
                            domain->mark_stack->compressed_stack_iter)
      ) {
    atomic_fetch_add_verify_ge0(&num_domains_to_mark, -1);
    domain->marking_done = 1;
  }

  /* Ephemerons */
  // Adopt orphaned work from domains that were spawned and terminated in
  // the previous cycle.
#ifdef DEBUG
  orph_ephe_list_verify_status (caml_global_heap_state.UNMARKED);
#endif
  caml_adopt_orphaned_work ();
  CAMLassert(domain->ephe_info->todo == (value) NULL);
  domain->ephe_info->todo = domain->ephe_info->live;
  domain->ephe_info->live = (value) NULL;
  domain->ephe_info->cycle = 0;
  domain->ephe_info->cursor.todop = NULL;
  domain->ephe_info->cursor.cycle = 0;
  if (domain->ephe_info->todo == (value) NULL)
    caml_ephe_todo_list_emptied();

  /* Finalisers */
  domain->final_info->updated_first = 0;
  domain->final_info->updated_last = 0;

  /* To ensure a mutator doesn't resume while global roots are being marked.
     Mutators can alter the set of global roots, to preserve its correctness,
     they should not run while global roots are being marked.*/
  caml_global_barrier();

  /* Someone should flush the allocation stats we gathered during the cycle */
  if( participating[0] == Caml_state ) {
    CAML_EV_ALLOC_FLUSH();
  }

  CAML_EV_END(EV_MAJOR_GC_STW);
  CAML_EV_END(EV_MAJOR_GC_CYCLE_DOMAINS);
}

static int is_complete_phase_sweep_and_mark_main ()
{
  return
    caml_gc_phase == Phase_sweep_and_mark_main &&
    atomic_load_acq (&num_domains_to_sweep) == 0 &&
    atomic_load_acq (&num_domains_to_mark) == 0 &&
    /* Marking is done */
    atomic_load_acq(&ephe_cycle_info.num_domains_todo) ==
    atomic_load_acq(&ephe_cycle_info.num_domains_done) &&
    /* Ephemeron marking is done */
    no_orphaned_work();
    /* All orphaned ephemerons have been adopted */
}

static int is_complete_phase_mark_final ()
{
  return
    caml_gc_phase == Phase_mark_final &&
    atomic_load_acq (&num_domains_to_final_update_first) == 0 &&
    /* updated finalise first values */
    atomic_load_acq (&num_domains_to_mark) == 0 &&
    /* Marking is done */
    atomic_load_acq(&ephe_cycle_info.num_domains_todo) ==
    atomic_load_acq(&ephe_cycle_info.num_domains_done) &&
    /* Ephemeron marking is done */
    no_orphaned_work();
    /* All orphaned ephemerons have been adopted */
}

static int is_complete_phase_sweep_ephe ()
{
  return
    caml_gc_phase == Phase_sweep_ephe &&
    atomic_load_acq (&num_domains_to_ephe_sweep) == 0 &&
    /* All domains have swept their ephemerons */
    atomic_load_acq (&num_domains_to_final_update_last) == 0 &&
    /* All domains have updated finalise last values */
    no_orphaned_work();
    /* All orphaned structures have been adopted */
}

static void try_complete_gc_phase (caml_domain_state* domain, void* unused,
                                   int participating_count,
                                   caml_domain_state** participating)
{
  barrier_status b;
  CAML_EV_BEGIN(EV_MAJOR_GC_PHASE_CHANGE);

  b = caml_global_barrier_begin ();
  if (caml_global_barrier_is_final(b)) {
    if (is_complete_phase_sweep_and_mark_main()) {
      caml_gc_phase = Phase_mark_final;
    } else if (is_complete_phase_mark_final()) {
      caml_gc_phase = Phase_sweep_ephe;
    }
  }
  caml_global_barrier_end(b);
  CAML_EV_END(EV_MAJOR_GC_PHASE_CHANGE);
}

intnat caml_opportunistic_major_work_available (void)
{
  caml_domain_state* domain_state = Caml_state;
  return !domain_state->sweeping_done || !domain_state->marking_done;
}

typedef enum {
  Slice_uninterruptible,
  Slice_interruptible,
  Slice_opportunistic
} collection_slice_mode;

static char collection_slice_mode_char(collection_slice_mode mode)
{
  switch(mode) {
    case Slice_uninterruptible:
      return 'u';
    case Slice_interruptible:
      return 'i';
    case Slice_opportunistic:
      return 'o';
    default:
      return ' ';
  }
}

#define Chunk_size 0x400

static intnat major_collection_slice(intnat howmuch,
                                     int participant_count,
                                     caml_domain_state** barrier_participants,
                                     collection_slice_mode mode,
                                     int major_cycle_spinning)
{
  caml_domain_state* domain_state = Caml_state;
  intnat sweep_work = 0, mark_work = 0;
  intnat available, left;
  uintnat blocks_marked_before = domain_state->stat_blocks_marked;
  int was_marking = 0;
  uintnat saved_ephe_cycle;
  uintnat saved_major_cycle = caml_major_cycles_completed;
  intnat computed_work, budget, interrupted_budget = 0;

  int log_events = mode != Slice_opportunistic ||
                   (caml_params->verb_gc & 0x40) ||
                   major_cycle_spinning;

  update_major_slice_work();
  computed_work = get_major_slice_work(howmuch);
  budget = computed_work;

  /* shortcut out if there is no opportunistic work to be done
   * NB: needed particularly to avoid caml_ev spam when polling */
  if (mode == Slice_opportunistic &&
      !caml_opportunistic_major_work_available()) {
    return budget;
  }

  if (log_events) CAML_EV_BEGIN(EV_MAJOR_SLICE);
  call_timing_hook(&caml_major_slice_begin_hook);

  if (!domain_state->sweeping_done) {
    if (log_events) CAML_EV_BEGIN(EV_MAJOR_SWEEP);

    do {
      available = budget > Chunk_size ? Chunk_size : budget;
      left = caml_sweep(domain_state->shared_heap, available);
      budget -= available - left;
      sweep_work += available - left;

      if (budget > 0 && available == left) {
        domain_state->sweeping_done = 1;
        atomic_fetch_add_verify_ge0(&num_domains_to_sweep, -1);
      }

      if (mode == Slice_interruptible && caml_incoming_interrupts_queued())
      {
        interrupted_budget = budget;
        budget = 0;
      }
    } while (budget > 0 && available != left);

    if (log_events) CAML_EV_END(EV_MAJOR_SWEEP);
  }

mark_again:
  while (budget > 0) {
    if (!domain_state->marking_done) {
      if (!was_marking) {
        if (log_events) CAML_EV_BEGIN(EV_MAJOR_MARK);
        was_marking = 1;
      }
      available = budget > Chunk_size ? Chunk_size : budget;
      left = mark(available);
      budget -= available - left;
      mark_work += available - left;
    } else {
      break;
    }

    if (mode == Slice_interruptible && caml_incoming_interrupts_queued()) {
      interrupted_budget = budget;
      budget = 0;
    }
  }
  if (was_marking) {
    if (log_events) CAML_EV_END(EV_MAJOR_MARK);
    was_marking = 0;
  }

  if (mode != Slice_opportunistic) {
    /* Finalisers */
    if (caml_gc_phase == Phase_mark_final &&
        caml_final_update_first(domain_state)) {
      /* This domain has updated finalise first values */
      atomic_fetch_add_verify_ge0(&num_domains_to_final_update_first, -1);
      if (budget > 0 && !domain_state->marking_done)
        goto mark_again;
    }

    if (caml_gc_phase == Phase_sweep_ephe &&
        caml_final_update_last(domain_state)) {
      /* This domain has updated finalise last values */
      atomic_fetch_add_verify_ge0(&num_domains_to_final_update_last, -1);
      /* Nothing has been marked while updating last */
    }

#ifdef DEBUG
    orph_ephe_list_verify_status (caml_global_heap_state.MARKED);
#endif
    caml_adopt_orphaned_work();

    /* Ephemerons */
    saved_ephe_cycle = atomic_load_acq(&ephe_cycle_info.ephe_cycle);
    if (domain_state->ephe_info->todo != (value) NULL &&
        saved_ephe_cycle > domain_state->ephe_info->cycle) {
      CAML_EV_BEGIN(EV_MAJOR_EPHE_MARK);
      budget = ephe_mark(budget, saved_ephe_cycle, EPHE_MARK_DEFAULT);
      CAML_EV_END(EV_MAJOR_EPHE_MARK);
      if (domain_state->ephe_info->todo == (value) NULL)
        caml_ephe_todo_list_emptied ();
      else if (budget > 0 && domain_state->marking_done)
        record_ephe_marking_done(saved_ephe_cycle);
      else if (budget > 0) goto mark_again;
    }

    if (caml_gc_phase == Phase_sweep_ephe &&
        domain_state->ephe_info->todo != 0) {
      CAML_EV_BEGIN(EV_MAJOR_EPHE_SWEEP);
      budget = ephe_sweep (domain_state, budget);
      CAML_EV_END(EV_MAJOR_EPHE_SWEEP);
      if (domain_state->ephe_info->todo == 0) {
        atomic_fetch_add_verify_ge0(&num_domains_to_ephe_sweep, -1);
      }
    }

    /* Complete GC phase */
    if (is_complete_phase_sweep_and_mark_main() ||
        is_complete_phase_mark_final ()) {
      if (barrier_participants) {
        try_complete_gc_phase (domain_state,
                              (void*)0,
                              participant_count,
                              barrier_participants);
      } else {
        caml_try_run_on_all_domains (&try_complete_gc_phase, 0, 0);
      }
      if (budget > 0) goto mark_again;
    }
  }

  call_timing_hook(&caml_major_slice_end_hook);
  if (log_events) CAML_EV_END(EV_MAJOR_SLICE);

  caml_gc_log
    ("Major slice [%c%c%c]: %ld work, %ld sweep, %ld mark (%lu blocks)",
              collection_slice_mode_char(mode),
              interrupted_budget == 0 ? '.' : '*',
              caml_gc_phase_char(caml_gc_phase),
              (long)computed_work, (long)sweep_work, (long)mark_work,
              (unsigned long)(domain_state->stat_blocks_marked
                                                      - blocks_marked_before));

  /* we did: work we were asked - interrupted_budget + any overwork */
  commit_major_slice_work
              (computed_work - interrupted_budget + (budget < 0 ? -budget : 0));

  if (mode != Slice_opportunistic && is_complete_phase_sweep_ephe()) {
    saved_major_cycle = caml_major_cycles_completed;
    /* To handle the case where multiple domains try to finish the major
      cycle simultaneously, we loop until the current cycle has ended,
      ignoring whether caml_try_run_on_all_domains succeeds. */


    while (saved_major_cycle == caml_major_cycles_completed) {
      if (barrier_participants) {
        cycle_all_domains_callback
              (domain_state, (void*)0, participant_count, barrier_participants);
      } else {
        caml_try_run_on_all_domains(&cycle_all_domains_callback, 0, 0);
      }
    }
  }

  return interrupted_budget;
}

void caml_opportunistic_major_collection_slice(intnat howmuch)
{
  major_collection_slice(howmuch, 0, 0, Slice_opportunistic, 0);
}

void caml_major_collection_slice(intnat howmuch)
{

  /* if this is an auto-triggered GC slice, make it interruptible */
  if (howmuch == AUTO_TRIGGERED_MAJOR_SLICE) {
    intnat interrupted_work = major_collection_slice(
        AUTO_TRIGGERED_MAJOR_SLICE,
        0,
        0,
        Slice_interruptible,
        0
        );
    if (interrupted_work > 0) {
      caml_gc_log("Major slice interrupted, rescheduling major slice");
      caml_request_major_slice();
    }
  } else {
    /* TODO: could make forced API slices interruptible, but would need to do
       accounting or pass up interrupt */
    major_collection_slice(howmuch, 0, 0, Slice_uninterruptible, 0);
  }
}

static void finish_major_cycle_callback (caml_domain_state* domain, void* arg,
                                         int participating_count,
                                         caml_domain_state** participating)
{
  uintnat saved_major_cycles = (uintnat)arg;
  CAMLassert (domain == Caml_state);

  caml_empty_minor_heap_no_major_slice_from_stw
    (domain, (void*)0, participating_count, participating);

  CAML_EV_BEGIN(EV_MAJOR_FINISH_CYCLE);
  while (saved_major_cycles == caml_major_cycles_completed) {
    major_collection_slice(10000000, participating_count, participating,
                           Slice_uninterruptible, 1);
  }
  CAML_EV_END(EV_MAJOR_FINISH_CYCLE);
}

void caml_finish_major_cycle (void)
{
  uintnat saved_major_cycles = caml_major_cycles_completed;

  while( saved_major_cycles == caml_major_cycles_completed ) {
    caml_try_run_on_all_domains
    (&finish_major_cycle_callback, (void*)caml_major_cycles_completed, 0);
  }
}

void caml_empty_mark_stack (void)
{
  while (!Caml_state->marking_done){
    mark(1000);
    caml_handle_incoming_interrupts();
  }

  if (Caml_state->stat_blocks_marked)
    caml_gc_log("Finished marking major heap. Marked %u blocks",
                (unsigned)Caml_state->stat_blocks_marked);
  Caml_state->stat_blocks_marked = 0;
}

void caml_finish_marking (void)
{
  if (!Caml_state->marking_done) {
    CAML_EV_BEGIN(EV_MAJOR_FINISH_MARKING);
    caml_empty_mark_stack();
    caml_shrink_mark_stack();
    Caml_state->stat_major_words += Caml_state->allocated_words;
    Caml_state->allocated_words = 0;
    CAML_EV_END(EV_MAJOR_FINISH_MARKING);
  }
}

void caml_finish_sweeping (void)
{
  if (Caml_state->sweeping_done) return;
  CAML_EV_BEGIN(EV_MAJOR_FINISH_SWEEPING);
  while (!Caml_state->sweeping_done) {
    if (caml_sweep(Caml_state->shared_heap, 10) > 0) {
      /* just finished sweeping */
      CAMLassert(Caml_state->sweeping_done == 0);
      Caml_state->sweeping_done = 1;
      atomic_fetch_add_verify_ge0(&num_domains_to_sweep, -1);
      break;
    }
    caml_handle_incoming_interrupts();
  }
  CAML_EV_END(EV_MAJOR_FINISH_SWEEPING);
}

Caml_inline int add_addr(struct addrmap* amap, value v) {
  uintnat k = PTR_TO_PAGE(v);
  uintnat flag = (uintnat)1 << PTR_TO_PAGE_OFFSET(v);
  int new_entry = 0;

  value* amap_pos = caml_addrmap_insert_pos(amap, k);

  if (*amap_pos == ADDRMAP_NOT_PRESENT) {
    new_entry = 1;
    *amap_pos = 0;
  }

  CAMLassert(v == (value)((k + PTR_TO_PAGE_OFFSET(v))<<3));

  if (!(*amap_pos & flag)) {
    *amap_pos |= flag;
  }

  return new_entry;
}

static void mark_stack_prune(struct mark_stack* stk)
{
  /* We need to handle the case when the mark stack overflows and we already
     have entries in the compressed stack. addrmap is (currently) using open
     address hashing, to keep things simple we just copy out the remaining
     compressed entries to a new map. */
  uintnat old_compressed_entries = 0;
  struct addrmap new_compressed_stack = ADDRMAP_INIT;
  addrmap_iterator it;
  for (it = stk->compressed_stack_iter;
       caml_addrmap_iter_ok(&stk->compressed_stack, it);
       it = caml_addrmap_next(&stk->compressed_stack, it)) {
    value k = caml_addrmap_iter_key(&stk->compressed_stack, it);
    value v = caml_addrmap_iter_value(&stk->compressed_stack, it);
    caml_addrmap_insert(&new_compressed_stack, k, v);
    ++old_compressed_entries;
  }
  if (old_compressed_entries > 0) {
    caml_gc_log("Preserved %"ARCH_INTNAT_PRINTF_FORMAT "d compressed entries",
                old_compressed_entries);
  }
  caml_addrmap_clear(&stk->compressed_stack);
  stk->compressed_stack = new_compressed_stack;

  /* scan mark stack and compress entries */
  int i;
  uintnat new_stk_count = 0, compressed_entries = 0, total_words = 0;
  for (i=0; i < stk->count; i++) {
    mark_entry me = stk->stack[i];
    total_words += me.end - me.start;
    if (me.end - me.start > BITS_PER_WORD) {
      /* keep entry in the stack as more efficent and move to front */
      stk->stack[new_stk_count++] = me;
    } else {
      while(me.start < me.end) {
        compressed_entries += add_addr(&stk->compressed_stack,
                                       (uintnat)me.start);
        me.start++;
      }
    }
  }

  caml_gc_log("Compressed %"ARCH_INTNAT_PRINTF_FORMAT "d mark stack words into "
              "%"ARCH_INTNAT_PRINTF_FORMAT "d mark stack entries and "
              "%"ARCH_INTNAT_PRINTF_FORMAT "d compressed entries",
              total_words, new_stk_count,
              compressed_entries+old_compressed_entries);

  stk->count = new_stk_count;
  CAMLassert(stk->count < stk->size);

  /* setup the compressed stack iterator */
  stk->compressed_stack_iter = caml_addrmap_iterator(&stk->compressed_stack);
}

int caml_init_major_gc(caml_domain_state* d) {
  d->mark_stack = caml_stat_alloc_noexc(sizeof(struct mark_stack));
  if(d->mark_stack == NULL) {
    return -1;
  }
  d->mark_stack->stack =
    caml_stat_alloc_noexc(MARK_STACK_INIT_SIZE * sizeof(mark_entry));
  if(d->mark_stack->stack == NULL) {
    caml_stat_free(d->mark_stack);
    d->mark_stack = NULL;
    return -1;
  }
  d->mark_stack->count = 0;
  d->mark_stack->size = MARK_STACK_INIT_SIZE;
  caml_addrmap_init(&d->mark_stack->compressed_stack);
  d->mark_stack->compressed_stack_iter =
                  caml_addrmap_iterator(&d->mark_stack->compressed_stack);

  /* Fresh domains do not need to performing marking or sweeping. */
  d->sweeping_done = 1;
  d->marking_done = 1;
  d->major_work_computed = 0;
  d->major_work_todo = 0;
  d->major_gc_clock = 0.0;
  /* Finalisers. Fresh domains participate in updating finalisers. */
  d->final_info = caml_alloc_final_info ();
  if(d->final_info == NULL) {
    caml_stat_free(d->mark_stack->stack);
    caml_stat_free(d->mark_stack);
    return -1;
  }
  d->ephe_info = caml_alloc_ephe_info();
  if(d->ephe_info == NULL) {
    caml_stat_free(d->final_info);
    caml_stat_free(d->mark_stack->stack);
    caml_stat_free(d->mark_stack);
    d->final_info = NULL;
    d->mark_stack = NULL;
    return -1;
  }
  atomic_fetch_add(&num_domains_to_final_update_first, 1);
  atomic_fetch_add(&num_domains_to_final_update_last, 1);

  return 0;
}

void caml_teardown_major_gc(void) {
  caml_domain_state* d = Caml_state;
  CAMLassert(!caml_addrmap_iter_ok(&d->mark_stack->compressed_stack,
                                   d->mark_stack->compressed_stack_iter));
  caml_addrmap_clear(&d->mark_stack->compressed_stack);
  CAMLassert(d->mark_stack->count == 0);
  caml_stat_free(d->mark_stack->stack);
  caml_stat_free(d->mark_stack);
  d->mark_stack = NULL;
}

void caml_finalise_heap (void)
{
  return;
}
