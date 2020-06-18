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

#include <string.h>
#include <stdio.h>

#include "caml/addrmap.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/domain.h"
#include "caml/eventlog.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
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

static atomic_intnat domains_finished_minor_gc;

static atomic_uintnat caml_minor_cycles_started = 0;

double caml_extra_heap_resources_minor = 0;

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

struct caml_minor_tables* caml_alloc_minor_tables()
{
  struct caml_minor_tables *r =
      caml_stat_alloc_noexc(sizeof(struct caml_minor_tables));
  if(r != NULL)
    memset(r, 0, sizeof(*r));
  return r;
}

void reset_minor_tables(struct caml_minor_tables* r)
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
  return Is_minor(val);
}

extern int caml_debug_is_major(value val) {
  return Is_block(val) && !Is_minor(val);
}
#endif

void caml_set_minor_heap_size (asize_t wsize)
{
  caml_domain_state* domain_state = Caml_state;
  struct caml_minor_tables *r = domain_state->minor_tables;
  if (domain_state->young_ptr != domain_state->young_end) caml_minor_collection ();

  if(caml_reallocate_minor_heap(wsize) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap");
  }

  reset_minor_tables(r);
}

//*****************************************************************************

struct oldify_state {
  value todo_list;
  uintnat live_bytes;
  struct domain* promote_domain;
};

static value alloc_shared(mlsize_t wosize, tag_t tag)
{
  void* mem = caml_shared_try_alloc(Caml_state->shared_heap, wosize, tag,
                                    0 /* not pinned */);
  Caml_state->allocated_words += Whsize_wosize(wosize);
  if (mem == NULL) {
    caml_fatal_error("allocation failure during minor GC");
  }
  return Val_hp(mem);
}

#if 0
static inline void log_gc_value(const char* prefix, value v)
{
  if (Is_block(v)) {
    header_t hd = Hd_val(v);
    int sz = Wosize_hd (hd);
    caml_gc_log("%s 0x%lx hd=0x%lx tag=%d sz=%d", prefix, v, hd, Tag_val(v), sz);
  } else {
    caml_gc_log("%s not block 0x%lx", prefix, v);
  }
}
#endif

/* in progress updates are zeros except for the lowest color bit set to 1 */
#define In_progress_update_val ((header_t)0x100)
#define Is_update_in_progress(hd) ((hd) == In_progress_update_val)

/* TODO: Probably a better spinlock needed here though doesn't happen often */
static void spin_on_header(value v) {
  while (atomic_load(Hp_atomic_val(v)) != 0) {
    cpu_relax();
  }
}

static inline header_t get_header_val(value v) {
  header_t hd = atomic_load_explicit(Hp_atomic_val(v), memory_order_relaxed);
  if (!Is_update_in_progress(hd))
    return hd;

  spin_on_header(v);
  return 0;
}

header_t caml_get_header_val(value v) {
  return get_header_val(v);
}

static int try_update_object_header(value v, value *p, value result, mlsize_t infix_offset) {
  int success = 0;

  if( caml_domain_alone() ) {
    *Hp_val (v) = 0;
    Op_val(v)[0] = result;
    success = 1;
  } else {
    header_t hd = atomic_load(Hp_atomic_val(v));
    if( hd == 0 ) {
      // in this case this has been updated by another domain, throw away result
      // and return the one in the object
      result = Op_val(v)[0];
    } else if( Is_update_in_progress(hd) ) {
      // here we've caught a domain in the process of moving a minor heap object
      // we need to wait for it to finish
      spin_on_header(v);
      // Also throw away result and use the one from the other domain
      result = Op_val(v)[0];
    } else {
      // Here the header is neither zero nor an in-progress update
      header_t desired_hd = In_progress_update_val;
      if( atomic_compare_exchange_strong(Hp_atomic_val(v), &hd, desired_hd) ) {
        // Success
        // Now we can write the forwarding pointer
        atomic_store_explicit(Op_atomic_val(v), result, memory_order_relaxed);
        // And can update the header too
        atomic_store_explicit(Hp_atomic_val(v), 0, memory_order_relaxed);
        // Let the caller know we were responsible for the update
        success = 1;
      } else {
        // We were sniped by another domain, spin for that to complete then
        // throw away result and use the one from the other domain
        spin_on_header(v);
        result = Op_val(v)[0];
      }
    }
  }

  *p = result + infix_offset;
  return success;
}

/* If [*v] is an [Infix_tag] object, [v] is updated to point to the first
 * object in the block. */
static inline void resolve_infix_val (value* v)
{
  int offset = 0;
  if (get_header_val(*v) == Infix_tag) {
    offset = Infix_offset_val(*v);
    CAMLassert (offset > 0);
    *v -= offset;
  }
}

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */
static void oldify_one (void* st_v, value v, value *p)
{
  struct oldify_state* st = st_v;
  value result;
  header_t hd;
  mlsize_t sz, i;
  mlsize_t infix_offset;
  tag_t tag;

  tail_call:
  if (!(Is_block(v) && Is_minor(v))) {
    /* not a minor block */
    *p = v;
    return;
  }

  infix_offset = 0;
  do {
    hd = get_header_val(v);
    if (hd == 0) {
      /* already forwarded, another domain is likely working on this. */
      *p = Op_val(v)[0] + infix_offset;
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
    value stack_value = Op_val(v)[0];
    CAMLassert(Wosize_hd(hd) == 1 && infix_offset == 0);
    result = alloc_shared(1, Cont_tag);
    if( try_update_object_header(v, p, result, 0) ) {
      struct stack_info* stk = Ptr_val(stack_value);
      Op_val(result)[0] = Val_ptr(stk);
      if (stk != NULL) {
        caml_scan_stack(&oldify_one, st, stk);
      }
    }
    else
    {
      // Conflict - fix up what we allocated on the major heap
      *Hp_val(result) = Make_header(1, No_scan_tag, global.MARKED);
      #ifdef DEBUG
      Op_val(result)[0] = Val_long(1);
      #endif
    }
  } else if (tag < Infix_tag) {
    value field0;
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared (sz, tag);
    field0 = Op_val(v)[0];
    if( try_update_object_header(v, p, result, infix_offset) ) {
      if (sz > 1){
        Op_val (result)[0] = field0;
        Op_val (result)[1] = st->todo_list;
        st->todo_list = v;
      } else {
        CAMLassert (sz == 1);
        p = Op_val(result);
        v = field0;
        goto tail_call;
      }
    } else {
      // Conflict - fix up what we allocated on the major heap
      *Hp_val(result) = Make_header(sz, No_scan_tag, global.MARKED);
      #ifdef DEBUG
      {
        int c;
        for( c = 0; c < sz ; c++ ) {
          Op_val(result)[c] = Val_long(1);
        }
      }
      #endif
    }

  } else if (tag >= No_scan_tag) {
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared(sz, tag);
    for (i = 0; i < sz; i++) {
      value curr = Op_val(v)[i];
      Op_val (result)[i] = curr;
    }
    CAMLassert (infix_offset == 0);
    if( !try_update_object_header(v, p, result, 0) ) {
      // Conflict
      *Hp_val(result) = Make_header(sz, No_scan_tag, global.MARKED);
      #ifdef DEBUG
      for( i = 0; i < sz ; i++ ) {
        Op_val(result)[i] = Val_long(1);
      }
      #endif
    }
  } else {
    CAMLassert (tag == Forward_tag);
    CAMLassert (infix_offset == 0);

    value f = Forward_val (v);
    tag_t ft = 0;

    if (Is_block (f)) {
      ft = Tag_val (get_header_val(f) == 0 ? Op_val (f)[0] : f);
    }

    if (ft == Forward_tag || ft == Lazy_tag || ft == Double_tag) {
      /* Do not short-circuit the pointer.  Copy as a normal block. */
      CAMLassert (Wosize_hd (hd) == 1);
      st->live_bytes += Bhsize_hd(hd);
      result = alloc_shared (1, Forward_tag);
      if( try_update_object_header(v, p, result, 0) ) {
        p = Op_val (result);
        v = f;
        goto tail_call;
      } else {
        *Hp_val(result) = Make_header(1, No_scan_tag, global.MARKED);
        #ifdef DEBUG
        Op_val(result)[0] = Val_long(1);
        #endif
      }
    } else {
      v = f;                        /* Follow the forwarding */
      goto tail_call;               /*  then oldify. */
    }
  }
}

#if 0
/* Test if the ephemeron is alive */
/* Care needed with this test. It will only make sense if
   all minor heaps have been promoted
   as the liveness of the keys can only be known at that point */
static inline int ephe_check_alive_data (struct caml_ephe_ref_elt *re)
{
  mlsize_t i;
  value child;

  for (i = CAML_EPHE_FIRST_KEY; i < Wosize_val(re->ephe); i++) {
    child = Op_val(re->ephe)[i];
    if (child != caml_ephe_none
        && Is_block (child) && Is_minor(child)) {
      resolve_infix_val(&child);
      if (get_header_val(child) != 0) {
        /* value not copied to major heap */
        return 0;
      }
    }
  }

  return 1;
}
#endif

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
static void oldify_mopup (struct oldify_state* st, int do_ephemerons)
{
  value v, new_v, f;
  mlsize_t i;
  caml_domain_state* domain_state =
    st->promote_domain ? st->promote_domain->state : Caml_state;
  struct caml_ephe_ref_table ephe_ref_table = domain_state->minor_tables->ephe_ref;
  struct caml_ephe_ref_elt *re;
  int redo = 0;

  while (st->todo_list != 0) {
    v = st->todo_list;                 /* Get the head. */
    /* I'm not convinced we can ever have something in todo_list that was updated
    by another domain, so this assert using get_header_val is probably not
    neccessary */
    CAMLassert (get_header_val(v) == 0);       /* It must be forwarded. */
    new_v = Op_val (v)[0];                /* Follow forward pointer. */
    st->todo_list = Op_val (new_v)[1]; /* Remove from list. */

    f = Op_val (new_v)[0];
    CAMLassert (!Is_debug_tag(f));
    if (Is_block (f) && Is_minor(f)) {
      oldify_one (st, f, Op_val (new_v));
    }
    for (i = 1; i < Wosize_val (new_v); i++){
      f = Op_val (v)[i];
      CAMLassert (!Is_debug_tag(f));
      if (Is_block (f) && Is_minor(f)) {
        oldify_one (st, f, Op_val (new_v) + i);
      } else {
        Op_val (new_v)[i] = f;
      }
    }
    CAMLassert (Wosize_val(new_v));
  }

  /* Oldify the key and data in the minor heap of all ephemerons touched in this
     cycle
    We are doing this to allow concurrent minor collections to resume
    executing mutating code while others are still collecting.
   */
  if( do_ephemerons ) {
    for (re = ephe_ref_table.base;
         re < ephe_ref_table.ptr; re++) {
      value *data = re->offset == CAML_EPHE_DATA_OFFSET
              ? &Ephe_data(re->ephe)
              :  &Op_val(re->ephe)[re->offset];
      if (*data != caml_ephe_none && Is_block(*data) && Is_minor(*data) ) {
        resolve_infix_val(data);
        if (get_header_val(*data) == 0) { /* Value copied to major heap */
          *data = Op_val(*data)[0];
        } else {
          oldify_one(st, *data, data);
          redo = 1; /* oldify_todo_list can still be 0 */
        }
      }
    }
  }

  if (redo) oldify_mopup (st, 1);
}

//*****************************************************************************

CAMLexport value caml_promote(struct domain* domain, value root)
{
  return root;
}

//*****************************************************************************



void caml_minor_heap_domain_finalizers_admin (struct domain* domain, void* unused)
{
  /* need to do the finalizer data structure book-keeping */
  caml_final_update_last_minor(domain);

  /*caml_domain_state* domain_state = domain->state;
  struct caml_minor_tables *minor_tables = domain_state->minor_tables;
  struct caml_custom_elt *elt; */

  /* There will be no dead minor values as we can not tell the state of
     a minor heap aliveness until all domains complete */
  /*for (elt = minor_tables->custom.base; elt < minor_tables->custom.ptr; elt++) {
    value v = elt->block;
    if (get_header_val(v) == 0) {*/
      /* !!caml_adjust_gc_speed(elt->mem, elt->max); */
  /*} else {*/
      /* Block will be freed: call finalisation function, if any */
  /*    void (*final_fun)(value) = Custom_ops_val(v)->finalize;
      if (final_fun != NULL) final_fun(v);
    }
  }*/
}

void caml_empty_minor_heap_domain_clear (struct domain* domain, void* unused)
{
  caml_domain_state* domain_state = domain->state;
  struct caml_minor_tables *minor_tables = domain_state->minor_tables;

  caml_final_empty_young(domain);

  clear_table ((struct generic_table *)&minor_tables->major_ref);
  clear_table ((struct generic_table *)&minor_tables->ephe_ref);
  clear_table ((struct generic_table *)&minor_tables->custom);
}

void caml_empty_minor_heap_promote (struct domain* domain, int participating_count, struct domain** participating, int not_alone)
{
  caml_domain_state* domain_state = domain->state;
  struct caml_minor_tables *self_minor_tables = domain_state->minor_tables;
  struct caml_custom_elt *elt;
  unsigned rewrite_successes = 0;
  unsigned rewrite_failures = 0;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  uintnat minor_allocated_bytes = young_end - young_ptr;
  struct oldify_state st = {0};
  value **r;
  intnat c, curr_idx;

  st.promote_domain = domain;

  /* TODO: are there any optimizations we can make where we don't need to scan
     when minor heaps can reference each other? */
  uintnat prev_alloc_words = domain_state->allocated_words;

  caml_gc_log ("Minor collection of domain %d starting", domain->state->id);
  caml_ev_begin("minor_gc");
  caml_ev_begin("minor_gc/remembered_set");

  int remembered_roots = 0;

  if( not_alone ) {
    int participating_idx = -1;
    struct domain* domain_self = caml_domain_self();

    for( int i = 0; i < participating_count ; i++ ) {
      if( participating[i] == domain_self ) {
        participating_idx = i;
        break;
      }
    }

    CAMLassert(participating_idx != -1);

    struct domain* foreign_domain;
    // We use this rather odd scheme because it better smoothes the remainder
    for( curr_idx = 0, c = participating_idx; curr_idx < participating_count; curr_idx++) {
      foreign_domain = participating[c];
      struct caml_minor_tables* foreign_minor_tables = foreign_domain->state->minor_tables;
      struct caml_ref_table* foreign_major_ref = &foreign_minor_tables->major_ref;
      // calculate the size of the remembered set
      intnat major_ref_size = foreign_major_ref->ptr - foreign_major_ref->base;
      // number of remembered set entries each domain takes here
      intnat refs_per_domain = (major_ref_size / participating_count);
      // where to start in the remembered set
      value** ref_start = foreign_major_ref->base + (curr_idx * refs_per_domain);
      // where to end in the remembered set
      value** ref_end = foreign_major_ref->base + ((curr_idx+1) * refs_per_domain);
      // if we're the last domain this time, cover all the remaining refs
      if( curr_idx == participating_count-1 ) {
        caml_gc_log("taking remainder");
        ref_end = foreign_major_ref->ptr;
      }

      caml_gc_log("idx: %d, foreign_domain: %d, ref_size: %"ARCH_INTNAT_PRINTF_FORMAT"d, refs_per_domain: %"ARCH_INTNAT_PRINTF_FORMAT"d, ref_base: %p, ref_ptr: %p, ref_start: %p, ref_end: %p",
        participating_idx, foreign_domain->state->id, major_ref_size, refs_per_domain, foreign_major_ref->base, foreign_major_ref->ptr, ref_start, ref_end);

      for( r = ref_start ; r < foreign_major_ref->ptr && r < ref_end ; r++ )
      {
        oldify_one (&st, **r, *r);
        remembered_roots++;
      }

      c = (c+1) % participating_count;
    }
  }
  else
  {
    // If we're alone, we just do our own remembered set
    for( r = self_minor_tables->major_ref.base ; r < self_minor_tables->major_ref.ptr ; r++ )
    {
      oldify_one (&st, **r, *r);
      remembered_roots++;
    }
  }

  #ifdef DEBUG
    caml_global_barrier();
    // At this point all domains should have gone through all remembered set entries
    // We need to verify that all our remembered set entries are now in the major heap or promoted
    for( r = self_minor_tables->major_ref.base ; r < self_minor_tables->major_ref.ptr ; r++ ) {
      // Everything should be promoted
      CAMLassert(!Is_minor(*r));
    }
  #endif

  /* promote the finalizers unconditionally as we want to allow early release */
  caml_ev_begin("minor_gc/finalizers/oldify");
  for (elt = self_minor_tables->custom.base; elt < self_minor_tables->custom.ptr; elt++) {
    value *v = &elt->block;
    if (Is_block(*v) && Is_minor(*v)) {
      if (get_header_val(*v) == 0) { /* value copied to major heap */
        *v = Op_val(*v)[0];
      } else {
        oldify_one(&st, *v, v);
      }
    }
  }
  caml_final_do_young_roots (&oldify_one, &st, domain, 0);
  caml_ev_end("minor_gc/finalizers/oldify");

  caml_ev_begin("minor_gc/remembered_set/promote");
  oldify_mopup (&st, 1); /* ephemerons promoted here */
  caml_ev_end("minor_gc/remembered_set/promote");
  caml_ev_end("minor_gc/remembered_set");
  caml_gc_log("promoted %d roots, %lu bytes", remembered_roots, st.live_bytes);

  caml_ev_begin("minor_gc/finalizers_admin");
  caml_gc_log("running stw minor_heap_domain_finalizers_admin");
  caml_minor_heap_domain_finalizers_admin(domain, 0);
  caml_ev_end("minor_gc/finalizers_admin");

#ifdef DEBUG
  caml_global_barrier();
  caml_gc_log("ref_base: %p, ref_ptr: %p",
    self_minor_tables->major_ref.base, self_minor_tables->major_ref.ptr);
  for (r = self_minor_tables->major_ref.base;
       r < self_minor_tables->major_ref.ptr; r++) {
    value vnew = **r;
    CAMLassert (!Is_block(vnew) || (get_header_val(vnew) != 0 && !Is_minor(vnew)));
  }

  for (elt = self_minor_tables->custom.base; elt < self_minor_tables->custom.ptr; elt++) {
    value vnew = elt->block;
    CAMLassert (!Is_block(vnew) || (get_header_val(vnew) != 0 && !Is_minor(vnew)));
  }
#endif

  caml_ev_begin("minor_gc/local_roots");
  caml_do_local_roots(&oldify_one, &st, domain, 0);
  caml_scan_stack(&oldify_one, &st, domain_state->current_stack);
  caml_ev_begin("minor_gc/local_roots/promote");
  oldify_mopup (&st, 0);
  caml_ev_end("minor_gc/local_roots/promote");
  caml_ev_end("minor_gc/local_roots");

  /* we reset these pointers before allowing any mutators to be
     released to avoid races where another domain signals an interrupt
     and we clobber it */
  atomic_store_rel((atomic_uintnat*)&domain_state->young_limit, (uintnat)domain_state->young_start);
  atomic_store_rel((atomic_uintnat*)&domain_state->young_ptr, (uintnat)domain_state->young_end);

  if( not_alone ) {
    atomic_fetch_add_explicit(&domains_finished_minor_gc, 1, memory_order_release);
  }

  domain_state->stat_minor_words += Wsize_bsize (minor_allocated_bytes);
  domain_state->stat_minor_collections++;
  domain_state->stat_promoted_words += domain_state->allocated_words - prev_alloc_words;

  caml_ev_end("minor_gc");
  caml_gc_log ("Minor collection of domain %d completed: %2.0f%% of %u KB live, rewrite: successes=%u failures=%u",
               domain->state->id,
               100.0 * (double)st.live_bytes / (double)minor_allocated_bytes,
               (unsigned)(minor_allocated_bytes + 512)/1024, rewrite_successes, rewrite_failures);
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/

void caml_empty_minor_heap_setup(struct domain* domain) {
  atomic_store_explicit(&domains_finished_minor_gc, 0, memory_order_release);
}

/* must be called within a STW section */
static void caml_stw_empty_minor_heap (struct domain* domain, void* unused, int participating_count, struct domain** participating)
{
  #ifdef DEBUG
  CAMLassert(caml_domain_is_in_stw());
  #endif

  int not_alone = !caml_domain_alone();

  if( not_alone ) {
    if( participating[0] == caml_domain_self() ) {
      atomic_fetch_add(&caml_minor_cycles_started, 1);
    }
  }
  else
  {
    atomic_fetch_add(&caml_minor_cycles_started, 1);
  }

  caml_gc_log("running stw empty_minor_heap_promote");
  caml_empty_minor_heap_promote(domain, participating_count, participating, not_alone);

  if( not_alone ) {
    caml_ev_begin("minor_gc/leave_barrier");
    SPIN_WAIT {
      if( atomic_load_explicit(&domains_finished_minor_gc, memory_order_acquire) == participating_count ) {
        break;
      }
    }
    caml_ev_end("minor_gc/leave_barrier");
  }

  caml_ev_begin("minor_gc/clear");
  caml_gc_log("running stw empty_minor_heap_domain_clear");
  caml_empty_minor_heap_domain_clear(domain, 0);
  caml_ev_end("minor_gc/clear");
  caml_gc_log("finished stw empty_minor_heap");
}

/* must be called within a STW section  */
void caml_empty_minor_heap_from_stw (struct domain* domain, void* unused, int participating_count, struct domain** participating)
{
  barrier_status b = caml_global_barrier_begin();
  if( caml_global_barrier_is_final(b) ) {
    caml_empty_minor_heap_setup(domain);
  }
  caml_global_barrier_end(b);

  caml_stw_empty_minor_heap(domain, (void*)0, participating_count, participating);
}

void caml_do_opportunistic_major_slice(struct domain* domain, void* unused)
{
  /* NB: need to put guard around the ev logs to prevent
    spam when we poll */
  if (caml_opportunistic_major_work_available()) {
    int log_events = caml_params->verb_gc & 0x40;
    if (log_events) caml_ev_begin("minor_gc/opportunistic_major_slice");
    caml_opportunistic_major_collection_slice(0x200, 0);
    if (log_events) caml_ev_end("minor_gc/opportunistic_major_slice");
  }
}

/* must be called outside a STW section */
int caml_try_stw_empty_minor_heap_on_all_domains ()
{
  #ifdef DEBUG
  CAMLassert(!caml_domain_is_in_stw());
  #endif

  caml_gc_log("requesting stw empty_minor_heap");
  return caml_try_run_on_all_domains_with_spin_work(
    &caml_stw_empty_minor_heap, 0, /* stw handler */
    &caml_empty_minor_heap_setup, /* leader setup */
    &caml_do_opportunistic_major_slice, 0, /* enter spin work */
    0, 0, /* leave spin work */
    1); /* leave when done */
}

/* must be called outside a STW section, will retry until we have emptied our minor heap */
void caml_empty_minor_heaps_once ()
{
  uintnat saved_minor_cycle = atomic_load(&caml_minor_cycles_started);

  #ifdef DEBUG
  CAMLassert(!caml_domain_is_in_stw());
  #endif

  /* To handle the case where multiple domains try to execute a minor gc STW section */
  do {
    caml_try_stw_empty_minor_heap_on_all_domains();
  } while (saved_minor_cycle == atomic_load(&caml_minor_cycles_started));
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_ev_pause(EV_PAUSE_GC);

  caml_handle_incoming_interrupts ();
  caml_empty_minor_heaps_once();
  caml_handle_incoming_interrupts ();
  caml_major_collection_slice (0, 0);
  caml_final_do_calls();

  caml_ev_resume();

  /* If the major slice triggered a STW, do that now */
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
 char * msg_intr_int, char *msg_threshold, char *msg_growing, char *msg_error)
{
  CAMLassert (tbl->ptr == tbl->limit);
  CAMLassert (tbl->limit <= tbl->end);
  CAMLassert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    alloc_generic_table (tbl, Caml_state->minor_heap_wsz / 8, 256,
                         element_size);
  }else if (tbl->limit == tbl->threshold){
    CAML_INSTR_INT (msg_intr_int, 1);
    caml_gc_message (0x08, msg_threshold, 0);
    tbl->limit = tbl->end;
    caml_urge_major_slice ();
  }else{
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * element_size;
    caml_gc_message (0x08, msg_growing, (intnat) sz/1024);
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
     "request_minor/realloc_ref_table@",
     "ref_table threshold crossed\n",
     "Growing ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ref_table overflow");
}

void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_ephe_ref_elt),
     "request_minor/realloc_ephe_ref_table@",
     "ephe_ref_table threshold crossed\n",
     "Growing ephe_ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "ephe_ref_table overflow");
}

void caml_realloc_custom_table (struct caml_custom_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_custom_elt),
     "request_minor/realloc_custom_table@",
     "custom_table threshold crossed\n",
     "Growing custom_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "custom_table overflow");
}
