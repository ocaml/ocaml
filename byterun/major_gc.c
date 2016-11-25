#include <stdlib.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/roots.h"
#include "caml/globroots.h"
#include "caml/domain.h"
#include "caml/fiber.h"

#define MARK_STACK_SIZE (1 << 20)

typedef enum { Phase_idle, Phase_marking } gc_phase_t;

/* Phase of the current domain's GC. Phases are not necessarily
   synchronised between domains. */
static __thread gc_phase_t gc_phase = Phase_idle;

static __thread uintnat stat_blocks_marked = 0;

uintnat caml_percent_free = Percent_free_def;

void caml_init_major_gc() {
  caml_domain_state->mark_stack = caml_stat_alloc(MARK_STACK_SIZE * sizeof(value));
  caml_domain_state->mark_stack_count = 0;
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
                 PH = caml_domain_state->allocated_words / G
                    = caml_domain_state->allocated_words * 3 * (100 + caml_percent_free)
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
  uintnat heap_size = caml_heap_size(caml_domain_self()->shared_heap);
  double heap_words = (double)Wsize_bsize(heap_size);
  double p = (double) caml_domain_state->allocated_words * 3.0 * (100 + caml_percent_free)
      / heap_words / caml_percent_free / 2.0;

  double total_work =
    heap_words * 100 / (100 + caml_percent_free) /* marking */
    + heap_words; /* sweeping */

  return (intnat)(p * total_work);
  //return 1ll << 50;
}

static void mark_stack_push(value v) {
  Assert(Is_block(v));
  if (caml_domain_state->mark_stack_count >= MARK_STACK_SIZE)
    caml_fatal_error("mark stack overflow");
  caml_domain_state->mark_stack[caml_domain_state->mark_stack_count++] = v;
}

static int mark_stack_pop(value* ret) {
  if (caml_domain_state->mark_stack_count == 0)
    return 0;
  *ret = caml_domain_state->mark_stack[--caml_domain_state->mark_stack_count];
  return 1;
}

#ifdef DEBUG
#define Is_markable(v) (Is_block(v) && !Is_minor(v) && v != Debug_free_major)
#else
#define Is_markable(v) (Is_block(v) && !Is_minor(v))
#endif


static value mark_normalise(value v) {
  Assert(Is_markable(v));
  if (Tag_val(v) == Forward_tag) {
    /* FIXME: short-circuiting lazy values is a useful optimisation */
  } else if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
  }
  return v;
}

static intnat mark(value initial, intnat budget) {
  value next = initial;
  int found_next = 1;
  while (budget > 0 && found_next) {
    value v = next;
    header_t hd_v;
    found_next = 0;

    Assert(Is_markable(v));
    Assert(v == mark_normalise(v));

    stat_blocks_marked++;
    /* mark the current object */
    hd_v = Hd_val(v);
    // caml_gc_log ("mark: v=0x%lx hd=0x%lx tag=%d sz=%lu",
    //             v, hd_v, Tag_val(v), Wosize_val(v));
    if (Tag_hd (hd_v) == Stack_tag) {
      // caml_gc_log ("mark: stack=%p", (value*)v);
      caml_scan_stack(&caml_darken, v);
    } else if (Tag_hd (hd_v) < No_scan_tag) {
      int i;
      for (i = 0; i < Wosize_hd(hd_v); i++) {
        value child = Op_val(v)[i];
        // caml_gc_log ("mark: v=%p i=%u child=%p",(value*)v,i,(value*)child);
        /* FIXME: this is wrong, as Debug_tag(N) is a valid value.
           However, it's a useful debugging aid for now */
        Assert(!Is_debug_tag(child));
        if (Is_markable(child)) {
          child = mark_normalise(child);
          if (caml_mark_object(child)) {
            if (!found_next) {
              next = child;
              found_next = 1;
            } else {
              mark_stack_push(child);
            }
          }
        }
      }
    }
    budget -= Whsize_hd(hd_v);

    /* if we haven't found any markable children, pop an object to mark */
    if (!found_next) {
      found_next = mark_stack_pop(&next);
    }
  }
  if (found_next) {
    mark_stack_push(next);
  }
  return budget;
}

void caml_darken(value v, value* ignored) {

  /* Assert (Is_markable(v)); */
  if (!Is_markable (v)) return; /* foreign stack, at least */

  v = mark_normalise(v);
  if (caml_mark_object(v)) mark_stack_push(v);
}

intnat caml_major_collection_slice(intnat howmuch)
{
  intnat computed_work = howmuch ? howmuch : default_slice_budget();
  intnat budget = computed_work;
  intnat sweep_work, mark_work;
  uintnat blocks_marked_before = stat_blocks_marked;
  value v;

  caml_save_stack_gc();

  sweep_work = budget;
  budget = caml_sweep(caml_domain_self()->shared_heap, budget);
  sweep_work -= budget;

  if (gc_phase == Phase_idle) {
    caml_do_local_roots(&caml_darken, caml_domain_self());
    caml_scan_global_roots(&caml_darken);
    gc_phase = Phase_marking;
  }

  mark_work = budget;
  if (mark_stack_pop(&v))
    budget = mark(v, budget);
  mark_work -= budget;

  caml_gc_log("Major slice: %lu alloc, %ld work, %ld sweep, %ld mark (%lu blocks)",
              (unsigned long)caml_domain_state->allocated_words,
              (long)computed_work, (long)sweep_work, (long)mark_work,
              (unsigned long)(stat_blocks_marked - blocks_marked_before));
  caml_domain_state->allocated_words = 0;
  caml_restore_stack_gc();

  if (budget > 0) {
    caml_trigger_stw_gc();
    caml_handle_gc_interrupt();
  }


  return computed_work;
}

void caml_empty_mark_stack () {
  value v;

  while (mark_stack_pop(&v)) mark(v, 10000000);

  if (stat_blocks_marked)
    caml_gc_log("Finished marking major heap. Marked %u blocks", (unsigned)stat_blocks_marked);
  stat_blocks_marked = 0;
}

void caml_finish_marking () {
  //caml_gc_log ("caml_finish_marking(0)");
  caml_save_stack_gc();
  caml_do_local_roots(&caml_darken, caml_domain_self());
  caml_scan_global_roots(&caml_darken);
  caml_empty_mark_stack();
  caml_domain_state->allocated_words = 0;
  caml_restore_stack_gc();
  //caml_gc_log ("caml_finish_marking(1)");
}

void caml_empty_mark_stack_domain (struct domain* domain)
{
  value* mark_stack = *domain->mark_stack;
  uintnat* mark_stack_count = domain->mark_stack_count;

  while (*mark_stack_count) {
    *mark_stack_count = *mark_stack_count - 1;
    mark (mark_stack[*mark_stack_count], 10000000);
  }
}

void caml_finish_marking_domain (struct domain* domain) {
  //caml_gc_log("caml_finish_marking_domain(0): domain=%d", domain->id);
  caml_save_stack_gc();
  caml_do_local_roots(&caml_darken, domain);
  caml_empty_mark_stack_domain(domain);
  /* Previous step might have pushed values into our mark stack. Hence,
   * empty our mark stack */
  caml_empty_mark_stack();
  caml_domain_state->allocated_words = 0;
  caml_restore_stack_gc();
  //caml_gc_log("caml_finish_marking_domain(1): domain=%d", domain->id);
}
