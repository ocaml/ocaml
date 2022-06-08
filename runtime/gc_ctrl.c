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

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/shared_heap.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/runtime_events.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "caml/frame_descriptors.h"
#endif
#include "caml/domain.h"
#include "caml/fiber.h"
#include "caml/globroots.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/fail.h"

uintnat caml_max_stack_wsize;
uintnat caml_fiber_wsz;

extern uintnat caml_major_heap_increment; /* percent or words; see major_gc.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_percent_max;          /*        see compact.c */
extern uintnat caml_allocation_policy;    /*        see freelist.c */
extern uintnat caml_custom_major_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz;   /* see domain.c */

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  intnat majcoll;
  struct gc_stats s;
  caml_compute_gc_stats(&s);
  majcoll = caml_major_cycles_completed;

  res = caml_alloc_tuple (17);
  Store_field (res, 0, caml_copy_double ((double)s.alloc_stats.minor_words));
  Store_field (res, 1, caml_copy_double ((double)s.alloc_stats.promoted_words));
  Store_field (res, 2, caml_copy_double ((double)s.alloc_stats.major_words));
  Store_field (res, 3, Val_long (s.alloc_stats.minor_collections));
  Store_field (res, 4, Val_long (majcoll));
  Store_field (res, 5, Val_long (
    s.heap_stats.pool_words + s.heap_stats.large_words));
  Store_field (res, 6, Val_long (0));
  Store_field (res, 7, Val_long (
    s.heap_stats.pool_live_words + s.heap_stats.large_words));
  Store_field (res, 8, Val_long (
    s.heap_stats.pool_live_blocks + s.heap_stats.large_blocks));
  Store_field (res, 9, Val_long (
    s.heap_stats.pool_words - s.heap_stats.pool_live_words
    - s.heap_stats.pool_frag_words));
  Store_field (res, 10, Val_long (0));
  Store_field (res, 11, Val_long (0));
  Store_field (res, 12, Val_long (s.heap_stats.pool_frag_words));
  Store_field (res, 13, Val_long (0));
  Store_field (res, 14, Val_long (
    s.heap_stats.pool_max_words + s.heap_stats.large_max_words));
  Store_field (res, 15, Val_long (0));
  Store_field (res, 16, Val_long (s.alloc_stats.forced_major_collections));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed (void)
{
  return (Caml_state->stat_minor_words
          + ((double) ((uintnat)Caml_state->young_end -
              (uintnat)Caml_state->young_ptr)) / sizeof(value));
}

CAMLprim value caml_gc_minor_words(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLreturn(caml_copy_double(caml_gc_minor_words_unboxed()));
}

CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords = Caml_state->stat_minor_words
    + ((double) Wsize_bsize ((uintnat)Caml_state->young_end -
        (uintnat) Caml_state->young_ptr)) / sizeof(value);
  double prowords = Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;

  res = caml_alloc_3(0,
    caml_copy_double (minwords),
    caml_copy_double (prowords),
    caml_copy_double (majwords));
  CAMLreturn (res);
}

CAMLprim value caml_gc_get(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  res = caml_alloc_tuple (11);
  Store_field (res, 0, Val_long (Caml_state->minor_heap_wsz));  /* s */
  Store_field (res, 2, Val_long (caml_percent_free));           /* o */
  Store_field (res, 3, Val_long (caml_params->verb_gc));        /* v */
  Store_field (res, 5, Val_long (caml_max_stack_wsize));        /* l */
  Store_field (res, 8, Val_long (caml_custom_major_ratio));     /* M */
  Store_field (res, 9, Val_long (caml_custom_minor_ratio));     /* m */
  Store_field (res, 10, Val_long (caml_custom_minor_max_bsz));  /* n */
  CAMLreturn (res);
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_maj (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_min (uintnat p)
{
  return Max (p, 1);
}

CAMLprim value caml_gc_set(value v)
{
  uintnat newpf;
  uintnat newminwsz;
  uintnat new_custom_maj, new_custom_min, new_custom_sz;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_SET);


  caml_change_max_stack_size (Long_val (Field (v, 5)));

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    caml_gc_message (0x20, "New space overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  }

  /* These fields were added in 4.08.0. */
  if (Wosize_val (v) >= 11){
    new_custom_maj = norm_custom_maj (Long_val (Field (v, 8)));
    if (new_custom_maj != caml_custom_major_ratio){
      caml_custom_major_ratio = new_custom_maj;
      caml_gc_message (0x20, "New custom major ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_major_ratio);
    }
    new_custom_min = norm_custom_min (Long_val (Field (v, 9)));
    if (new_custom_min != caml_custom_minor_ratio){
      caml_custom_minor_ratio = new_custom_min;
      caml_gc_message (0x20, "New custom minor ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_ratio);
    }
    new_custom_sz = Long_val (Field (v, 10));
    if (new_custom_sz != caml_custom_minor_max_bsz){
      caml_custom_minor_max_bsz = new_custom_sz;
      caml_gc_message (0x20, "New custom minor size limit: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_max_bsz);
    }
  }

  /* Minor heap size comes last because it will trigger a minor collection
     (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminwsz = caml_norm_minor_heap_size (Long_val (Field (v, 0)));

  if (newminwsz != Caml_state->minor_heap_wsz) {
    caml_gc_message (0x20, "New minor heap size: %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
  }

  if (newminwsz > caml_minor_heap_max_wsz) {
    caml_gc_log ("update minor heap max: %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk words", newminwsz / 1024);
    caml_update_minor_heap_max(newminwsz);
  }
  CAMLassert(newminwsz <= caml_minor_heap_max_wsz);
  if (newminwsz != Caml_state->minor_heap_wsz) {
    caml_gc_log ("current minor heap size: %"
                 ARCH_SIZET_PRINTF_FORMAT "uk words",
                 Caml_state->minor_heap_wsz / 1024);
    caml_gc_log ("set minor heap size: %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk words", newminwsz / 1024);
    /* FIXME: when (newminwsz > caml_minor_heap_max_wsz) and
       (newminwsz != Caml_state->minor_heap_wsz) are both true,
       the current domain reallocates its own minor heap twice. */
    caml_set_minor_heap_size (newminwsz);
  }

  CAML_EV_END(EV_EXPLICIT_GC_SET);
  return Val_unit;
}

CAMLprim value caml_gc_minor(value v)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MINOR);
  CAMLassert (v == Val_unit);
  caml_minor_collection ();
  value exn = caml_process_pending_actions_exn();
  CAML_EV_END(EV_EXPLICIT_GC_MINOR);
  return caml_raise_if_exception(exn);
}

static value gc_major_exn(void)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR);
  caml_gc_log ("Major GC cycle requested");
  caml_empty_minor_heaps_once();
  caml_finish_major_cycle();
  value exn = caml_process_pending_actions_exn();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR);
  return exn;
}

CAMLprim value caml_gc_major(value v)
{
  CAMLassert (v == Val_unit);
  return caml_raise_if_exception(gc_major_exn());
}

static value gc_full_major_exn(void)
{
  int i;
  value exn = Val_unit;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_FULL_MAJOR);
  caml_gc_log ("Full Major GC requested");
  /* In general, it can require up to 3 GC cycles for a
     currently-unreachable object to be collected. */
  for (i = 0; i < 3; i++) {
    caml_empty_minor_heaps_once();
    caml_finish_major_cycle();
    exn = caml_process_pending_actions_exn();
    if (Is_exception_result(exn)) break;
  }
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_FULL_MAJOR);
  return exn;
}

CAMLprim value caml_gc_full_major(value v)
{
  CAMLassert (v == Val_unit);
  return caml_raise_if_exception(gc_full_major_exn());
}

CAMLprim value caml_gc_major_slice (value v)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR_SLICE);
  CAMLassert (Is_long (v));
  caml_major_collection_slice(Long_val(v));
  value exn = caml_process_pending_actions_exn();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR_SLICE);
  return caml_raise_if_exception(exn);
}

CAMLprim value caml_gc_compaction(value v)
{
  value exn = Val_unit;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_COMPACT);
  CAMLassert (v == Val_unit);
  exn = gc_major_exn();
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
  return exn;
}

CAMLprim value caml_gc_stat(value v)
{
  value res;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_STAT);
  res = gc_full_major_exn();
  if (Is_exception_result(res)) goto out;
  res = caml_gc_quick_stat(Val_unit);
 out:
  CAML_EV_END(EV_EXPLICIT_GC_STAT);
  return caml_raise_if_exception(res);
}

CAMLprim value caml_get_minor_free (value v)
{
  return Val_int
    ((uintnat)Caml_state->young_ptr - (uintnat)Caml_state->young_start);
}

void caml_init_gc (void)
{
  caml_minor_heap_max_wsz =
    caml_norm_minor_heap_size(caml_params->init_minor_heap_wsz);

  caml_max_stack_wsize = caml_params->init_max_stack_wsz;
  caml_fiber_wsz = (Stack_threshold * 2) / sizeof(value);
  caml_percent_free = norm_pfree (caml_params->init_percent_free);
  caml_gc_log ("Initial stack limit: %"
               ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
               caml_max_stack_wsize / 1024 * sizeof (value));

  caml_custom_major_ratio =
      norm_custom_maj (caml_params->init_custom_major_ratio);
  caml_custom_minor_ratio =
      norm_custom_min (caml_params->init_custom_minor_ratio);
  caml_custom_minor_max_bsz = caml_params->init_custom_minor_max_bsz;

  caml_gc_phase = Phase_sweep_and_mark_main;
  #ifdef NATIVE_CODE
  caml_init_frame_descriptors();
  #endif
  caml_init_domains(caml_params->init_minor_heap_wsz);
/*
  caml_major_heap_increment = major_incr;
  caml_percent_free = norm_pfree (percent_fr);
  caml_percent_max = norm_pmax (percent_m);
  caml_init_major_heap (major_heap_size);
  caml_gc_message (0x20, "Initial minor heap size: %luk bytes\n",
                   Caml_state->minor_heap_size / 1024);
  caml_gc_message (0x20, "Initial major heap size: %luk bytes\n",
                   major_heap_size / 1024);
  caml_gc_message (0x20, "Initial space overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  caml_gc_message (0x20, "Initial max overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_max);
  if (caml_major_heap_increment > 1000){
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                     caml_major_heap_increment / 1024);
  }else{
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     caml_major_heap_increment);
  }
  caml_gc_message (0x20, "Initial allocation policy: %d\n",
                   caml_allocation_policy);
*/
}

/* FIXME After the startup_aux.c unification, move these functions there. */

CAMLprim value caml_runtime_variant (value unit)
{
  CAMLassert (unit == Val_unit);
#if defined (DEBUG)
  return caml_copy_string ("d");
#elif defined (CAML_INSTR)
  return caml_copy_string ("i");
#else
  return caml_copy_string ("");
#endif
}

extern int caml_parser_trace;

CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  /* TODO KC */
  return caml_alloc_sprintf ("caml_runtime_parameters not implemented: %d", 0);
}

/* Control runtime warnings */

CAMLprim value caml_ml_enable_runtime_warnings(value vbool)
{
  caml_runtime_warnings = Bool_val(vbool);
  return Val_unit;
}

CAMLprim value caml_ml_runtime_warnings_enabled(value unit)
{
  CAMLassert (unit == Val_unit);
  return Val_bool(caml_runtime_warnings);
}
