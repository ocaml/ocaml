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
#include "caml/roots.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/weak.h"

extern value caml_ephe_none; /* See weak.c */
struct generic_table CAML_TABLE_STRUCT(char);

/* [sz] and [rsv] are numbers of entries */
static void alloc_generic_table (struct generic_table *tbl, asize_t sz,
                                 asize_t rsv, asize_t element_size)
{
  void *new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (void *) caml_stat_alloc_noexc((tbl->size + tbl->reserve) *
                                             element_size);
  if (new_table == NULL) caml_fatal_error ("Fatal error: not enough memory\n");
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

void caml_free_minor_tables(struct caml_minor_tables* r)
{
  CAMLassert(r->major_ref.ptr == r->major_ref.base);
  CAMLassert(r->minor_ref.ptr == r->minor_ref.base);
  reset_table((struct generic_table *)&r->major_ref);
  reset_table((struct generic_table *)&r->minor_ref);
  reset_table((struct generic_table *)&r->ephe_ref);
  reset_table((struct generic_table *)&r->custom);
  caml_stat_free(r);
}

void caml_set_minor_heap_size (asize_t wsize)
{
  caml_domain_state* domain_state = Caml_state;
  struct caml_minor_tables *r = domain_state->minor_tables;
  if (domain_state->young_ptr != domain_state->young_end) caml_minor_collection ();

  if(caml_reallocate_minor_heap(wsize) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap");
  }

  reset_table ((struct generic_table *)&r->major_ref);
  reset_table ((struct generic_table *)&r->minor_ref);
  reset_table ((struct generic_table *)&r->ephe_ref);
  reset_table((struct generic_table *)&r->custom);
}

//*****************************************************************************

struct oldify_state {
  value todo_list;
  uintnat live_bytes;
  struct domain* promote_domain;
  value oldest_promoted;
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

static inline int is_in_interval (value v, char* low_closed, char* high_open)
{
  return low_closed <= (char*)v && (char*)v < high_open;
}

/* If [*v] is an [Infix_tag] object, [v] is updated to point to the first
 * object in the block. */
static inline void resolve_infix_val (value* v)
{
  int offset = 0;
  if (Hd_val(*v) == Infix_tag) {
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
  caml_domain_state* domain_state =
    st->promote_domain ? st->promote_domain->state : Caml_state;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  CAMLassert (domain_state->young_start <= domain_state->young_ptr &&
          domain_state->young_ptr <= domain_state->young_end);

 tail_call:
  if (!(Is_block(v) && is_in_interval((value)Hp_val(v), young_ptr, young_end))) {
    /* not a minor block */
    *p = v;
    return;
  }

  infix_offset = 0;
  do {
    hd = Hd_val (v);
    if (hd == 0) {
      /* already forwarded, forward pointer is first field. */
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

  if (((value)Hp_val(v)) > st->oldest_promoted) {
    st->oldest_promoted = (value)Hp_val(v);
  }

  if (tag == Cont_tag) {
    struct stack_info* stk = Ptr_val(Op_val(v)[0]);
    CAMLassert(Wosize_hd(hd) == 1 && infix_offset == 0);
    result = alloc_shared(1, Cont_tag);
    *p = result;
    Op_val(result)[0] = Val_ptr(stk);
    *Hp_val (v) = 0;
    Op_val(v)[0] = result;
    if (stk != NULL)
      caml_scan_stack(&oldify_one, st, stk);
  } else if (tag < Infix_tag) {
    value field0;
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared (sz, tag);
    *p = result + infix_offset;
    field0 = Op_val(v)[0];
    CAMLassert (!Is_debug_tag(field0));
    *Hp_val (v) = 0;           /* Set forward flag */
    Op_val(v)[0] = result;     /*  and forward pointer. */
    if (sz > 1){
      Op_val (result)[0] = field0;
      Op_val (result)[1] = st->todo_list;    /* Add this block */
      st->todo_list = v;                     /*  to the "to do" list. */
    }else{
      CAMLassert (sz == 1);
      p = Op_val(result);
      v = field0;
      goto tail_call;
    }
  } else if (tag >= No_scan_tag) {
    sz = Wosize_hd (hd);
    st->live_bytes += Bhsize_hd(hd);
    result = alloc_shared(sz, tag);
    for (i = 0; i < sz; i++) {
      value curr = Op_val(v)[i];
      Op_val (result)[i] = curr;
    }
    *Hp_val (v) = 0;           /* Set forward flag */
    Op_val (v)[0] = result;    /*  and forward pointer. */
    CAMLassert (infix_offset == 0);
    *p = result;
  } else {
    CAMLassert (tag == Forward_tag);
    CAMLassert (infix_offset == 0);

    value f = Forward_val (v);
    tag_t ft = 0;

    if (Is_block (f)) {
      ft = Tag_val (Hd_val (f) == 0 ? Op_val (f)[0] : f);
    }

    if (ft == Forward_tag || ft == Lazy_tag || ft == Double_tag) {
      /* Do not short-circuit the pointer.  Copy as a normal block. */
      CAMLassert (Wosize_hd (hd) == 1);
      st->live_bytes += Bhsize_hd(hd);
      result = alloc_shared (1, Forward_tag);
      *p = result;
      *Hp_val (v) = 0;             /* Set (GC) forward flag */
      Op_val (v)[0] = result;      /*  and forward pointer. */
      p = Op_val (result);
      v = f;
      goto tail_call;
    } else {
      v = f;                        /* Follow the forwarding */
      goto tail_call;               /*  then oldify. */
    }
  }
}

/* Test if the ephemeron is alive */
static inline int ephe_check_alive_data (struct caml_ephe_ref_elt *re,
                                         char* young_ptr, char* young_end)
{
  mlsize_t i;
  value child;

  for (i = CAML_EPHE_FIRST_KEY; i < Wosize_val(re->ephe); i++) {
    child = Op_val(re->ephe)[i];
    if (child != caml_ephe_none
        && Is_block (child) && is_in_interval(child, young_ptr, young_end)) {
      resolve_infix_val(&child);
      if (Hd_val(child) != 0) {
        /* value not copied to major heap */
        return 0;
      }
    }
  }
  return 1;
}

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
static void oldify_mopup (struct oldify_state* st)
{
  value v, new_v, f;
  mlsize_t i;
  caml_domain_state* domain_state =
    st->promote_domain ? st->promote_domain->state : Caml_state;
  struct caml_ephe_ref_table ephe_ref_table = domain_state->minor_tables->ephe_ref;
  struct caml_ephe_ref_elt *re;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  int redo = 0;

  while (st->todo_list != 0) {
    v = st->todo_list;                 /* Get the head. */
    CAMLassert (Hd_val (v) == 0);             /* It must be forwarded. */
    new_v = Op_val (v)[0];                /* Follow forward pointer. */
    st->todo_list = Op_val (new_v)[1]; /* Remove from list. */

    f = Op_val (new_v)[0];
    CAMLassert (!Is_debug_tag(f));
    if (Is_block (f) &&
        is_in_interval((value)Hp_val(v), young_ptr, young_end)) {
      oldify_one (st, f, Op_val (new_v));
    }
    for (i = 1; i < Wosize_val (new_v); i++){
      f = Op_val (v)[i];
      CAMLassert (!Is_debug_tag(f));
      if (Is_block (f) &&
          is_in_interval((value)Hp_val(v), young_ptr, young_end)) {
        oldify_one (st, f, Op_val (new_v) + i);
      } else {
        Op_val (new_v)[i] = f;
      }
    }
    CAMLassert (Wosize_val(new_v));
  }

  /* Oldify the data in the minor heap of alive ephemeron
     During minor collection keys outside the minor heap are considered alive */
  for (re = ephe_ref_table.base;
       re < ephe_ref_table.ptr; re++) {
    /* look only at ephemeron with data in the minor heap */
    if (re->offset == CAML_EPHE_DATA_OFFSET) {
      value *data = &Ephe_data(re->ephe);
      if (*data != caml_ephe_none && Is_block(*data) &&
          is_in_interval(*data, young_ptr, young_end)) {
        resolve_infix_val(data);
        if (Hd_val(*data) == 0) { /* Value copied to major heap */
          *data = Op_val(*data)[0];
        } else {
          if (ephe_check_alive_data(re, young_ptr, young_end)) {
            oldify_one(st, *data, data);
            redo = 1; /* oldify_todo_list can still be 0 */
          }
        }
      }
    }
  }

  if (redo) oldify_mopup (st);
}

//*****************************************************************************

void forward_pointer (void* state, value v, value *p) {
  header_t hd;
  mlsize_t offset;
  value fwd;
  struct domain* promote_domain = state;
  caml_domain_state* domain_state =
    promote_domain ? promote_domain->state : Caml_state;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;

  if (Is_block (v) && is_in_interval((value)Hp_val(v), young_ptr, young_end)) {
    hd = Hd_val(v);
    if (hd == 0) {
      *p = Op_val(v)[0];
      CAMLassert (Is_block(*p) && !Is_minor(*p));
    } else if (Tag_hd(hd) == Infix_tag) {
      offset = Infix_offset_hd(hd);
      fwd = 0;
      forward_pointer (state, v - offset, &fwd);
      if (fwd) *p = fwd + offset;
    }
  }
}

static value next_minor_block(caml_domain_state* domain_state, value curr_hp)
{
  mlsize_t wsz;
  header_t hd;
  value curr_val;
  CAMLassert ((value)domain_state->young_ptr <= curr_hp);
  CAMLassert (curr_hp < (value)domain_state->young_end);
  hd = Hd_hp(curr_hp);
  curr_val = Val_hp(curr_hp);
  if (hd == 0) {
    /* Forwarded object, find the promoted version */
    curr_val = Op_val(curr_val)[0];
  }
  CAMLassert (Is_block(curr_val) && Hd_val(curr_val) != 0 && Tag_val(curr_val) != Infix_tag);
  wsz = Wosize_val(curr_val);
  CAMLassert (wsz <= Max_young_wosize);
  return curr_hp + Bsize_wsize(Whsize_wosize(wsz));
}

void caml_empty_minor_heap_domain (struct domain* domain);

CAMLexport value caml_promote(struct domain* domain, value root)
{
  value **r;
  value iter, f;
  mlsize_t i;
  caml_domain_state* domain_state = domain->state;
  struct caml_minor_tables *minor_tables = domain_state->minor_tables;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  float percent_to_scan;
  uintnat prev_alloc_words = domain_state->allocated_words;
  struct oldify_state st = {0};
  struct caml_ephe_ref_elt *re;

  /* Integers are already shared */
  if (Is_long(root))
    return root;

  /* Objects which are in the major heap are already shared. */
  if (!Is_minor(root))
    return root;

  st.oldest_promoted = (value)domain_state->young_start;
  st.promote_domain = domain;

  CAMLassert(caml_owner_of_young_block(root) == domain);
  oldify_one (&st, root, &root);
  oldify_mopup (&st);

  CAMLassert (!Is_minor(root));
  /* FIXME: surely a newly-allocated root is already darkened? */
  caml_darken(0, root, 0);

  percent_to_scan = st.oldest_promoted <= (value)young_ptr ? 0.0 :
    (((float)(st.oldest_promoted - (value)young_ptr)) * 100.0 /
     ((value)young_end - (value)domain_state->young_start));

  if (percent_to_scan > Percent_to_promote_with_GC) {
    caml_gc_log("caml_promote: forcing minor GC. %%_minor_to_scan=%f", percent_to_scan);
    // ???
    caml_empty_minor_heap_domain (domain);
  } else {
    caml_do_local_roots (&forward_pointer, st.promote_domain, domain, 1);
    caml_scan_stack (&forward_pointer, st.promote_domain, domain_state->current_stack);

    /* Scan major to young pointers. */
    for (r = minor_tables->major_ref.base;
         r < minor_tables->major_ref.ptr; r++) {
      value old_p = **r;
      if (Is_block(old_p) && is_in_interval(old_p,young_ptr,young_end)) {
        value new_p = old_p;
        forward_pointer (st.promote_domain, new_p, &new_p);
        if (old_p != new_p) {
          if (caml_domain_alone())
            **r = new_p;
          else
            atomic_compare_exchange_strong((atomic_value*)*r, &old_p, new_p);
        }
      }
    }

    /* Scan ephemeron ref table */
    for (re = minor_tables->ephe_ref.base;
         re < minor_tables->ephe_ref.ptr; re++) {
      value* key = &Op_val(re->ephe)[re->offset];
      if (Is_block(*key) && is_in_interval(*key,young_ptr,young_end)) {
        forward_pointer (st.promote_domain, *key, key);
      }
    }

    /* Scan young to young pointers */
    for (r = minor_tables->minor_ref.base; r < minor_tables->minor_ref.ptr; r++) {
      forward_pointer (st.promote_domain, **r, *r);
    }

    /* Scan newer objects */
    for (iter = (value)young_ptr;
         iter <= st.oldest_promoted;
         iter = next_minor_block(domain_state, iter)) {
      value hd = Hd_hp(iter);
      value curr = Val_hp(iter);
      if (hd != 0) {
        tag_t tag = Tag_hd (hd);
        if (tag == Cont_tag) {
          struct stack_info* stk = Ptr_val(Op_val(curr)[0]);
          if (stk != NULL)
            caml_scan_stack(&forward_pointer, st.promote_domain, stk);
        } else if (tag < No_scan_tag) {
          for (i = 0; i < Wosize_hd (hd); i++) {
            f = Op_val(curr)[i];
            if (Is_block(f)) {
              forward_pointer (st.promote_domain, f,((value*)curr) + i);
            }
          }
        }
      }
    }
  }
  domain_state->stat_promoted_words += domain_state->allocated_words - prev_alloc_words;
  return root;
}

//*****************************************************************************

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/

void caml_empty_minor_heap_domain (struct domain* domain)
{
  CAMLnoalloc;
  caml_domain_state* domain_state = domain->state;
  struct caml_minor_tables *minor_tables = domain_state->minor_tables;
  unsigned rewrite_successes = 0;
  unsigned rewrite_failures = 0;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  uintnat minor_allocated_bytes = young_end - young_ptr;
  struct oldify_state st = {0};
  value **r;
  value **r_new;
  struct caml_ephe_ref_elt *re;
  struct caml_custom_elt *elt;
  struct caml_ref_table major_ref_rewrites = {0};

  st.promote_domain = domain;

  if (minor_allocated_bytes != 0) {
    uintnat prev_alloc_words = domain_state->allocated_words;

#ifdef DEBUG
    /* In DEBUG mode, verify that the minor_ref table contains all young-young pointers
       from older to younger objects */
    {
    struct addrmap young_young_ptrs = ADDRMAP_INIT;
    mlsize_t i;
    value iter;
    for (r = minor_tables->minor_ref.base; r < minor_tables->minor_ref.ptr; r++) {
      *caml_addrmap_insert_pos(&young_young_ptrs, (value)*r) = 1;
    }
    for (iter = (value)young_ptr;
         iter < (value)young_end;
         iter = next_minor_block(domain_state, iter)) {
      value hd = Hd_hp(iter);
      if (hd != 0) {
        value curr = Val_hp(iter);
        tag_t tag = Tag_hd (hd);

        if (tag < No_scan_tag && tag != Cont_tag) {
          // FIXME: should scan Cont_tag
          for (i = 0; i < Wosize_hd(hd); i++) {
            value* f = Op_val(curr) + i;
            if (Is_block(*f) && is_in_interval(*f, young_ptr, young_end) &&
                *f < curr) {
              CAMLassert(caml_addrmap_contains(&young_young_ptrs, (value)f));
            }
          }
        }
      }
    }
    caml_addrmap_clear(&young_young_ptrs);
    }
#endif

    caml_gc_log ("Minor collection of domain %d starting", domain->state->id);
    caml_ev_begin("minor_gc");
    caml_ev_begin("minor_gc/roots");
    caml_do_local_roots(&oldify_one, &st, domain, 0);

    caml_scan_stack(&oldify_one, &st, domain_state->current_stack);

    for (r = minor_tables->major_ref.base; r < minor_tables->major_ref.ptr; r++) {
      value x = **r;
      oldify_one (&st, x, &x);
      /* save pointer to the final location and do atomic update later */
      Ref_table_add(&major_ref_rewrites, Op_val(x));
    }
    caml_ev_end("minor_gc/roots");

    caml_ev_begin("minor_gc/promote");
    oldify_mopup (&st);
    caml_ev_end("minor_gc/promote");

    caml_ev_begin("minor_gc/ephemerons");
    for (re = minor_tables->ephe_ref.base;
         re < minor_tables->ephe_ref.ptr; re++) {
      CAMLassert (Ephe_domain(re->ephe) == domain);
      if (re->offset == CAML_EPHE_DATA_OFFSET) {
        /* Data field has already been handled in oldify_mopup. Handle only
         * keys here. */
        continue;
      }
      value* key = &Op_val(re->ephe)[re->offset];
      if (*key != caml_ephe_none && Is_block(*key) &&
          is_in_interval(*key, young_ptr, young_end)) {
        resolve_infix_val(key);
        if (Hd_val(*key) == 0) { /* value copied to major heap */
          *key = Op_val(*key)[0];
        } else {
          CAMLassert(!ephe_check_alive_data(re,young_ptr,young_end));
          *key = caml_ephe_none;
          Ephe_data(re->ephe) = caml_ephe_none;
        }
      }
    }
    caml_ev_end("minor_gc/ephemerons");

    caml_ev_begin("minor_gc/update_minor_tables");
    for (r = minor_tables->major_ref.base, r_new = major_ref_rewrites.base;
         r < minor_tables->major_ref.ptr;
         r++, r_new++) {
      CAMLassert (r_new < major_ref_rewrites.ptr);
      value v = **r;
      if (Is_block (v) && is_in_interval ((value)Hp_val(v), young_ptr, young_end)) {
        value vnew = (value)*r_new;
        CAMLassert (!Is_block(vnew) || (!Is_minor(vnew) && Hd_val(vnew) != 0));
        if (caml_domain_alone()) {
          **r = vnew;
          ++rewrite_successes;
        } else {
          if (atomic_compare_exchange_strong((atomic_value*)*r, &v, vnew))
            ++rewrite_successes;
          else
            ++rewrite_failures;
        }
      }
    }
    CAMLassert (!caml_domain_alone() || rewrite_failures == 0);
    reset_table((struct generic_table *)&major_ref_rewrites);
    caml_ev_end("minor_gc/update_minor_tables");

    caml_ev_begin("minor_gc/finalisers");
    caml_final_update_last_minor(domain);
    /* Run custom block finalisation of dead minor values */
    for (elt = minor_tables->custom.base; elt < minor_tables->custom.ptr; elt++) {
      value v = elt->block;
      if (Hd_val(v) == 0) {
        /* !!caml_adjust_gc_speed(elt->mem, elt->max); */
      } else {
        /* Block will be freed: call finalisation function, if any */
        void (*final_fun)(value) = Custom_ops_val(v)->finalize;
        if (final_fun != NULL) final_fun(v);
      }
    }
    caml_final_empty_young(domain);
    caml_ev_end("minor_gc/finalisers");


    clear_table ((struct generic_table *)&minor_tables->major_ref);
    clear_table ((struct generic_table *)&minor_tables->minor_ref);
    clear_table ((struct generic_table *)&minor_tables->ephe_ref);
    clear_table ((struct generic_table *)&minor_tables->custom);

    domain_state->young_ptr = domain_state->young_end;
    domain_state->stat_minor_words += Wsize_bsize (minor_allocated_bytes);
    domain_state->stat_minor_collections++;
    domain_state->stat_promoted_words += domain_state->allocated_words - prev_alloc_words;

    caml_ev_end("minor_gc");
    caml_gc_log ("Minor collection of domain %d completed: %2.0f%% of %u KB live, rewrite: successes=%u failures=%u",
                 domain->state->id,
                 100.0 * (double)st.live_bytes / (double)minor_allocated_bytes,
                 (unsigned)(minor_allocated_bytes + 512)/1024, rewrite_successes, rewrite_failures);
  }
  else {
    caml_final_empty_young(domain);
    caml_gc_log ("Minor collection of domain %d: skipping", domain->state->id);
  }

#ifdef DEBUG
  {
    value *p;
    for (p = (value *) domain_state->young_start;
         p < (value *) domain_state->young_end; ++p){
      *p = Debug_free_minor;
    }
  }
#endif
}

void caml_empty_minor_heap ()
{
  caml_empty_minor_heap_domain (caml_domain_self());
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_ev_pause(EV_PAUSE_GC);

  caml_handle_incoming_interrupts ();
  caml_empty_minor_heap ();
  caml_handle_incoming_interrupts ();
  caml_major_collection_slice (0, 0);
  caml_final_do_calls();

  caml_ev_resume();

  /* If the major slice triggered a STW, do that now */
  caml_handle_gc_interrupt();
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  caml_handle_gc_interrupt();
  CAMLreturn (extra_root);
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
      caml_fatal_error (msg_error);
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
     "Fatal error: ref_table overflow\n");
}

void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *tbl)
{
  realloc_generic_table
    ((struct generic_table *) tbl, sizeof (struct caml_ephe_ref_elt),
     "request_minor/realloc_ephe_ref_table@",
     "ephe_ref_table threshold crossed\n",
     "Growing ephe_ref_table to %" ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
     "Fatal error: ephe_ref_table overflow\n");
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
