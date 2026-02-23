/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>

#include "caml/callback.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/roots.h"
#include "caml/shared_heap.h"

/* [size] is a number of elements for the [to_do.item] array */
static void alloc_todo (caml_domain_state* d, int size)
{
  struct final_todo *result =
    caml_stat_alloc_noexc (sizeof (struct final_todo) +
                           size * sizeof (struct final));
  struct caml_final_info *f = d->final_info;
  if (result == NULL) caml_fatal_error ("out of memory");
  result->next = NULL;
  result->size = size;
  if (f->todo_tail == NULL) {
    f->todo_head = result;
    f->todo_tail = result;
  } else {
    CAMLassert (f->todo_tail->next == NULL);
    f->todo_tail->next = result;
    f->todo_tail = result;
  }
}

/* Find white finalisable values, move them to the finalising set, and
   darken them (if darken_value is true). */
static void generic_final_update
  (caml_domain_state* d, struct finalisable *final, int darken_value)
{
  uintnat todo_count = 0;
  struct caml_final_info *f = d->final_info;

  CAMLassert (final->old <= final->young);
  for (uintnat i = 0; i < final->old; i++) {
    CAMLassert (Is_block (final->table[i].val));
    if (is_unmarked (final->table[i].val)) {
      ++ todo_count;
    }
  }

  /** invariant:
      - 0 <= j <= i /\ 0 <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are black
      (alive or in the minor heap) or the finalizer have been copied
      in to_do_tl.
      - j : index in final_table, before j all the values are black
      (alive or in the minor heap), next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0) {
    uintnat i, j, k;
    caml_set_action_pending(d);
    alloc_todo (d, todo_count);
    j = k = 0;
    for (i = 0; i < final->old; i++){
      CAMLassert (Is_block (final->table[i].val));
      CAMLassert (Tag_val (final->table[i].val) != Forward_tag);
      if (is_unmarked (final->table[i].val)) {
        /** dead */
        f->todo_tail->item[k] = final->table[i];
        if (!darken_value) {
          /* The value is not darken so the finalisation function
             is called with unit not with the value */
          f->todo_tail->item[k].val = Val_unit;
          f->todo_tail->item[k].offset = 0;
        };
        k++;
      } else {
        /** alive */
        final->table[j++] = final->table[i];
      }
    }
    CAMLassert (i == final->old);
    CAMLassert (k == todo_count);
    final->old = j;
    for ( ; i < final->young; i++) {
      final->table[j++] = final->table[i];
    }
    final->young = j;
    f->todo_tail->size = k;
    if (darken_value) {
      for (i = 0; i < k; i++) {
        /* Note that item may already be dark due to multiple entries in
           the final table. */
        caml_darken (d, f->todo_tail->item[i].val, NULL);
      }
    }
  }
}

int caml_final_update_first (caml_domain_state* d)
{
  struct caml_final_info *f = d->final_info;
  if (!f->updated_first) {
    CAML_EV_BEGIN(EV_FINALISE_UPDATE_FIRST);
    generic_final_update (d, &f->first, /* darken_value */ 1);
    CAML_EV_END(EV_FINALISE_UPDATE_FIRST);
    f->updated_first = 1;
    return 1;
  }
  return 0;
}

int caml_final_update_last (caml_domain_state* d)
{
  struct caml_final_info *f = d->final_info;
  if (!f->updated_last) {
    CAML_EV_BEGIN(EV_FINALISE_UPDATE_LAST);
    generic_final_update (d, &f->last, /* darken_value */ 0);
    CAML_EV_END(EV_FINALISE_UPDATE_LAST);
    f->updated_last = 1;
    return 1;
  }
  return 0;
}

/* Call the finalisation functions for the finalising set.
   Note that this function must be reentrant.
*/
caml_result caml_final_do_calls_res(void)
{
  struct final f;
  caml_result res;
  struct caml_final_info *fi = Caml_state->final_info;

  if (fi->running_finalisation_function) return Result_unit;
  if (fi->todo_head != NULL) {
    call_timing_hook(&caml_finalise_begin_hook);
    CAML_GC_MESSAGE(FINALIZE,
                    "Calling finalisation functions.\n");
    while (1) {
      while (fi->todo_head != NULL && fi->todo_head->size == 0) {
        struct final_todo *next_head = fi->todo_head->next;
        caml_stat_free (fi->todo_head);
        fi->todo_head = next_head;
        if (fi->todo_head == NULL) fi->todo_tail = NULL;
      }
      if (fi->todo_head == NULL) break;
      CAMLassert (fi->todo_head->size > 0);
      --fi->todo_head->size;
      f = fi->todo_head->item[fi->todo_head->size];
      fi->running_finalisation_function = 1;
      res = caml_callback_res (f.fun, f.val + f.offset);
      fi->running_finalisation_function = 0;
      if (caml_result_is_exception(res)) return res;
    }
    CAML_GC_MESSAGE(FINALIZE,
                    "Done calling finalisation functions.\n");
    call_timing_hook(&caml_finalise_end_hook);
  }
  return Result_unit;
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,d,x) (*(f)) ((d), (x), &(x))

/* Called my major_gc for marking roots */
void caml_final_do_roots
  (scanning_action act, scanning_action_flags fflags, void* fdata,
   caml_domain_state* d, int do_val)
{
  struct caml_final_info *f = d->final_info;

  CAMLassert (f->first.old <= f->first.young);
  for (uintnat i = 0; i < f->first.young; i++) {
    Call_action (act, fdata, f->first.table[i].fun);
    if (do_val)
      Call_action (act, fdata, f->first.table[i].val);
  }

  CAMLassert (f->last.old <= f->last.young);
  for (uintnat i = 0; i < f->last.young; i++) {
    Call_action (act, fdata, f->last.table[i].fun);
    if (do_val)
      Call_action (act, fdata, f->last.table[i].val);
  }

  for (struct final_todo *todo = f->todo_head;
       todo != NULL;
       todo = todo->next) {
    for (uintnat i = 0; i < todo->size; i++) {
      Call_action (act, fdata, todo->item[i].fun);
      Call_action (act, fdata, todo->item[i].val);
    }
  }
}

static void final_cont_do_young_roots
  (scanning_action act, void* fdata, caml_domain_state* d);

/* Called by minor gc for marking roots */
void caml_final_do_young_roots
  (scanning_action act, scanning_action_flags fflags, void* fdata,
   caml_domain_state* d, int do_last_val)
{
  struct caml_final_info *f = d->final_info;

  CAMLassert (f->first.old <= f->first.young);
  for (uintnat i = f->first.old; i < f->first.young; i++) {
    Call_action (act, fdata, f->first.table[i].fun);
    Call_action (act, fdata, f->first.table[i].val);
  }

  CAMLassert (f->last.old <= f->last.young);
  for (uintnat i = f->last.old; i < f->last.young; i++) {
    Call_action (act, fdata, f->last.table[i].fun);
    if (do_last_val)
      Call_action (act, fdata, f->last.table[i].val);
  }

  final_cont_do_young_roots(act, fdata, d);
}

static void generic_final_minor_update
  (caml_domain_state* d, struct finalisable * final)
{
  uintnat todo_count = 0;
  struct caml_final_info *fi = d->final_info;

  CAMLassert (final->old <= final->young);
  for (uintnat i = final->old; i < final->young; i++){
    CAMLassert (Is_block (final->table[i].val));
    if (Is_young(final->table[i].val) &&
        !Is_promoted_hd(Hd_val(final->table[i].val))) {
      ++ todo_count;
    }
  }

  /** invariant:
      - final->old <= j <= i /\ final->old <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are alive
            or the finalizer have been copied in to_do_tl.
      - j : index in final_table, before j all the values are alive,
            next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0) {
    uintnat i, j, k;
    caml_set_action_pending(d);
    alloc_todo (d, todo_count);
    k = 0;
    j = final->old;
    for (i = final->old; i < final->young; i++) {
      CAMLassert (Is_block (final->table[i].val));
      CAMLassert (Tag_val (final->table[i].val) != Forward_tag);
      if (Is_young(final->table[i].val) &&
          !Is_promoted_hd(Hd_val(final->table[i].val))) {
        /** dead */
        fi->todo_tail->item[k] = final->table[i];
        /* The finalisation function is called with unit not with the value */
        fi->todo_tail->item[k].val = Val_unit;
        fi->todo_tail->item[k].offset = 0;
        k++;
      } else {
        /** alive */
        final->table[j++] = final->table[i];
      }
    }
    CAMLassert (i == final->young);
    CAMLassert (k == todo_count);
    final->young = j;
    fi->todo_tail->size = todo_count;
  }

  /** update the minor value to the copied major value */
  for (uintnat i = final->old; i < final->young; i++) {
    CAMLassert (Is_block (final->table[i].val));
    if (Is_young(final->table[i].val)) {
      CAMLassert (Is_promoted_hd(Hd_val(final->table[i].val)));
      final->table[i].val = Field(final->table[i].val, 0);
    }
  }
}

void caml_final_update_last_minor (caml_domain_state* d)
{
  generic_final_minor_update(d, &d->final_info->last);
}

void caml_final_empty_young (caml_domain_state* d)
{
  struct caml_final_info *f = d->final_info;
  f->first.old = f->first.young;
  f->last.old = f->last.young;
}

void caml_final_merge_finalisable
  (struct finalisable *source, struct finalisable *target)
{
  uintnat new_size;

  CAMLassert (target->old <= target->young);
  /* to merge the source structure, all its values are in the major heap */
  CAMLassert (source->old == source->young);
  if (target->young + source->young >= target->size) {
    new_size = 2 * (target->young + source->young);
    if (target->table == NULL) {
      target->table = caml_stat_alloc (new_size * sizeof (struct final));
      CAMLassert (target->old == 0);
      CAMLassert (target->young == 0);
      target->size = new_size;
    } else {
      target->table = caml_stat_resize (target->table,
                                       new_size * sizeof (struct final));
      target->size = new_size;
    }
  }
  /* all values from the source are old, we will prepend them
     into the old area of the target */
  memmove(target->table + source->young, target->table,
          target->young * sizeof (struct final));
  memcpy(target->table, source->table,
         source->young * sizeof (struct final));
  /* adjust indices for the prepended values from the source */
  target->old += source->young;
  target->young += source->young;

#ifdef DEBUG
  {
    /** check target is well formed on the values */
    int i;
    for (i = 0; i < target->old; i++) {
      CAMLassert (target->table[i].val); /* no null ptrs */
      CAMLassert (Is_block(target->table[i].val));
      CAMLassert (!Is_young(target->table[i].val));
    };
    for (; i < target->young; i++) {
      CAMLassert (target->table[i].val); /* no null ptrs */
      CAMLassert (Is_block(target->table[i].val));
    }
  }
#endif
}

static void generic_final_register (struct finalisable *final, value f, value v)
{
  uintnat new_size;

  if (!Is_block(v) || Tag_val(v) == Lazy_tag
#ifdef FLAT_FLOAT_ARRAY
      || Tag_val(v) == Double_tag
#endif
      || Tag_val(v) == Forcing_tag
      || Tag_val(v) == Forward_tag) {
    caml_invalid_argument ("Gc.finalise");
  }
  CAMLassert (final->old <= final->young);

  if (final->young >= final->size) {
    if (final->table == NULL) {
      new_size = 30;
      final->table = caml_stat_alloc (new_size * sizeof (struct final));
      CAMLassert (final->old == 0);
      CAMLassert (final->young == 0);
      final->size = new_size;
    } else {
      new_size = final->size * 2;
      final->table = caml_stat_resize (final->table,
                                       new_size * sizeof (struct final));
      final->size = new_size;
    }
  }
  CAMLassert (final->young < final->size);
  final->table[final->young].fun = f;
  if (Tag_val(v) == Infix_tag) {
    final->table[final->young].offset = Infix_offset_val (v);
    final->table[final->young].val = v - Infix_offset_val (v);
  } else {
    final->table[final->young].offset = 0;
    final->table[final->young].val = v;
  }
  ++ final->young;
}


CAMLprim value caml_final_register (value f, value v)
{
  generic_final_register (&Caml_state->final_info->first, f, v);
  return Val_unit;
}

CAMLprim value caml_final_register_called_without_value (value f, value v)
{
  generic_final_register (&Caml_state->final_info->last, f, v);
  return Val_unit;
}

CAMLprim value caml_final_release (value unit)
{
  Caml_state->final_info->running_finalisation_function = 0;
  return Val_unit;
}

struct caml_final_info* caml_alloc_final_info (void)
{
  struct caml_final_info* f =
    caml_stat_alloc_noexc (sizeof(struct caml_final_info));
  if(f != NULL)
    memset (f, 0, sizeof(struct caml_final_info));
  return f;
}

/******************************************************************************/
/* Continuations */
/******************************************************************************/

static void cont_minor_clean_prefix (void)
{
  struct cont *cont = &Caml_state->final_info->cont;
  while (cont->minor != (value) NULL && caml_is_continuation_used(cont->minor)) {
    cont->minor = Cont_link(cont->minor);
  }
}

value caml_final_cont_register (value cont)
{
  CAMLparam1(cont);
  CAMLassert (Is_block(cont) && Tag_val(cont) == Cont_tag);
  cont_minor_clean_prefix();
  Cont_link(cont) = Caml_state->final_info->cont.minor;
  Caml_state->final_info->cont.minor = cont;
  CAMLreturn(Val_unit);
}

void caml_final_cont_register_major (value cont)
{
  CAMLassert (Is_block(cont) && Tag_val(cont) == Cont_tag);
  Cont_link(cont) = Caml_state->final_info->cont.major;
  Caml_state->final_info->cont.major = cont;
}

static void final_cont_do_young_roots
  (scanning_action act, void* fdata, caml_domain_state* d)
{
  struct cont *cont = &d->final_info->cont;
  while (cont->minor != (value) NULL) {
    value c = cont->minor;
    header_t hd = Hd_val(c);
    if (hd != 0 && !caml_is_continuation_used(c)) {
      Call_action (act, fdata, c);
      /* Note: Don't use [cont->minor] for [c] above as [Call_action] will
         update its value. */
    }
    cont->minor = Cont_link(cont->minor);
  }
}

void caml_final_cont_update_major (caml_domain_state* d)
{
  struct cont *cont = &d->final_info->cont;

  value *prevp = &cont->major;
  value curr = cont->major;

  /* Iterate through the major list */
  while (curr != (value) NULL) {
    value next = Cont_link(curr);

    if (caml_is_continuation_used(curr)) {
      /* Taken: drop it from the list */
      *prevp = next;
    } else if (is_unmarked(curr)) {
      /* Unmarked and not taken: mark it and move it to [to_finalise] list */
      caml_darken(d, curr, NULL);
      *prevp = next;
      Cont_link(curr) = cont->to_finalise;
      cont->to_finalise = curr;
    } else {
      /* Marked and not taken: leave it in the major list */
      prevp = (value*)&Cont_link(curr);
    }

    curr = next;
  }
}

void caml_final_cont_do_calls(void)
{
  const value* deep_finalise = caml_named_value ("Effect.Deep.finalise");
  const value* shallow_finalise = caml_named_value ("Effect.Shallow.finalise");
  value c;
  struct stack_info* stk;
  struct caml_final_info* fi = Caml_state->final_info;
  struct cont *cont = &fi->cont;

  if (fi->running_finalisation_function) return;

  while (cont->to_finalise != (value) NULL) {
    c = cont->to_finalise;
    CAMLassert (Is_block(c) && !Is_young(c) && Tag_val(c) == Cont_tag);
    CAMLassert (!caml_is_continuation_used(c));
    cont->to_finalise = Cont_link(c);
    stk = Ptr_val(Field(c, 0));

    fi->running_finalisation_function = 1;
    if (Stack_handle_effect(stk) != Val_unit) {
      CAMLassert (deep_finalise != NULL);
      caml_callback(*deep_finalise, c);
    } else {
      /* Effect handler was set to [Val_unit] by
        [caml_continuation_clear_handler_noexc] */
      CAMLassert (shallow_finalise != NULL);
      caml_callback(*shallow_finalise, c);
    }
    fi->running_finalisation_function = 0;
  }
}