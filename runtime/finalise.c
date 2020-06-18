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
#include "caml/eventlog.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/roots.h"
#include "caml/shared_heap.h"

/* [size] is a number of elements for the [to_do.item] array */
static void alloc_todo (struct domain* d, int size)
{
  struct final_todo *result =
    caml_stat_alloc_noexc (sizeof (struct final_todo) +
                           size * sizeof (struct final));
  struct caml_final_info *f = d->state->final_info;
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
static void generic_final_update (struct domain* d, struct finalisable *final, int darken_value)
{
  uintnat i, j, k;
  uintnat todo_count = 0;
  struct caml_final_info *f = d->state->final_info;

  CAMLassert (final->old <= final->young);
  for (i = 0; i < final->old; i++) {
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
        caml_darken (NULL, f->todo_tail->item[i].val, NULL);
      }
    }
  }
}

int caml_final_update_first (struct domain* d)
{
  struct caml_final_info *f = d->state->final_info;
  if (!f->updated_first) {
    caml_ev_begin("major_gc/final_update_first");
    generic_final_update (d, &f->first, /* darken_value */ 1);
    caml_ev_end("major_gc/final_update_first");
    f->updated_first = 1;
    return 1;
  }
  return 0;
}

int caml_final_update_last (struct domain* d)
{
  struct caml_final_info *f = d->state->final_info;
  if (!f->updated_last) {
    caml_ev_begin("major_gc/final_update_last");
    generic_final_update (d, &f->last, /* darken_value */ 0);
    caml_ev_end("major_gc/final_update_last");
    f->updated_last = 1;
    return 1;
  }
  return 0;
}

void caml_final_do_calls ()
{
  struct final f;
  value res;
  struct domain *d = caml_domain_self();
  struct caml_final_info *fi = d->state->final_info;

  if (fi->running_finalisation_function) return;
  if (fi->todo_head != NULL) {
    caml_gc_message (0x80, "Calling finalisation functions.\n");
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
      res = caml_callback_exn (f.fun, f.val + f.offset);
      fi->running_finalisation_function = 0;
      if (Is_exception_result(res)) caml_raise (Extract_exception (res));
    }
    caml_gc_message (0x80, "Done calling finalisation functions.\n");
  }
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,d,x) (*(f)) ((d), (x), &(x))

/* Called my major_gc for marking roots */
void caml_final_do_roots (scanning_action act, void* fdata, struct domain* domain, int do_val)
{
  uintnat i;
  struct final_todo *todo;
  struct caml_final_info *f = domain->state->final_info;

  CAMLassert (f->first.old <= f->first.young);
  for (i = 0; i < f->first.young; i++) {
    Call_action (act, fdata, f->first.table[i].fun);
    if (do_val)
      Call_action (act, fdata, f->first.table[i].val);
  }

  CAMLassert (f->last.old <= f->last.young);
  for (i = 0; i < f->last.young; i++) {
    Call_action (act, fdata, f->last.table[i].fun);
    if (do_val)
      Call_action (act, fdata, f->last.table[i].val);
  }

  for (todo = f->todo_head; todo != NULL; todo = todo->next) {
    for (i = 0; i < todo->size; i++) {
      Call_action (act, fdata, todo->item[i].fun);
      Call_action (act, fdata, todo->item[i].val);
    }
  }
}

/* Called by minor gc for marking roots */
void caml_final_do_young_roots (scanning_action act, void* fdata, struct domain* d, int do_last_val)
{
  uintnat i;
  struct caml_final_info *f = d->state->final_info;

  CAMLassert (f->first.old <= f->first.young);
  for (i = f->first.old; i < f->first.young; i++) {
    Call_action (act, fdata, f->first.table[i].fun);
    Call_action (act, fdata, f->first.table[i].val);
  }

  CAMLassert (f->last.old <= f->last.old);
  for (i = f->last.old; i < f->last.young; i++) {
    Call_action (act, fdata, f->last.table[i].fun);
    if (do_last_val)
      Call_action (act, fdata, f->last.table[i].val);
  }
}

static void generic_final_minor_update (struct domain* d, struct finalisable * final)
{
  uintnat i, j, k;
  uintnat todo_count = 0;
  struct caml_final_info *fi = d->state->final_info;

  CAMLassert (final->old <= final->young);
  for (i = final->old; i < final->young; i++){
    CAMLassert (Is_block (final->table[i].val));
    if (Is_minor(final->table[i].val) && caml_get_header_val(final->table[i].val) != 0){
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
    alloc_todo (d, todo_count);
    k = 0;
    j = final->old;
    for (i = final->old; i < final->young; i++) {
      CAMLassert (Is_block (final->table[i].val));
      CAMLassert (Tag_val (final->table[i].val) != Forward_tag);
      if (Is_minor(final->table[j].val) && caml_get_header_val(final->table[i].val) != 0) {
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
  for (i = final->old; i < final->young; i++) {
    CAMLassert (Is_block (final->table[i].val));
    if (Is_minor(final->table[i].val)) {
      CAMLassert (caml_get_header_val(final->table[i].val) == 0);
      final->table[i].val = Op_val(final->table[i].val)[0];
    }
  }
}

void caml_final_update_last_minor (struct domain* d)
{
  generic_final_minor_update(d, &d->state->final_info->last);
}

void caml_final_empty_young (struct domain* d)
{
  struct caml_final_info *f = d->state->final_info;
  f->first.old = f->first.young;
  f->last.old = f->last.young;
}

void caml_final_merge_finalisable (struct finalisable *source, struct finalisable *target)
{
  uintnat new_size;

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
  memmove(target->table + source->young, target->table,
          source->young * sizeof (struct final));
  memcpy(target->table, source->table, source->young * sizeof (struct final));
  target->old += source->young;
  target->young += source->young;
}

static void generic_final_register (struct finalisable *final, value f, value v)
{
  uintnat new_size;

  if (!Is_block(v) || Tag_val(v) == Lazy_tag
#ifdef FLAT_FLOAT_ARRAY
      || Tag_val(v) == Double_tag
#endif
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

struct caml_final_info* caml_alloc_final_info ()
{
  struct caml_final_info* f =
    caml_stat_alloc_noexc (sizeof(struct caml_final_info));
  if(f != NULL)
    memset (f, 0, sizeof(struct caml_final_info));
  return f;
}
