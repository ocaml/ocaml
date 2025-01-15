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
#include "caml/gc_stats.h"
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

atomic_uintnat caml_max_stack_wsize;
uintnat caml_fiber_wsz;

extern _Atomic uintnat caml_percent_free; /* see major_gc.c */
extern _Atomic uintnat caml_custom_major_ratio; /* see custom.c */
extern _Atomic uintnat caml_custom_minor_ratio; /* see custom.c */
extern _Atomic uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz; /* see domain.c */

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  intnat majcoll, mincoll, compactions;
  struct gc_stats s;
  caml_compute_gc_stats(&s);
  majcoll = caml_major_cycles_completed;
  mincoll = atomic_load(&caml_minor_collections_count);
  compactions = atomic_load(&caml_compactions_count);

  res = caml_alloc_tuple (17);
  Store_field (res, 0, caml_copy_double ((double)s.alloc_stats.minor_words));
  Store_field (res, 1, caml_copy_double ((double)s.alloc_stats.promoted_words));
  Store_field (res, 2, caml_copy_double ((double)s.alloc_stats.major_words));
  Store_field (res, 3, Val_long (mincoll));
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
  Store_field (res, 13, Val_long (compactions));
  Store_field (res, 14, Val_long (
    s.heap_stats.pool_max_words + s.heap_stats.large_max_words));
  Store_field (res, 15, Val_long (0));
  Store_field (res, 16, Val_long (s.alloc_stats.forced_major_collections));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed (void)
{
  return (Caml_state->stat_minor_words
          + ((double) Wsize_bsize((uintnat)Caml_state->young_end -
                                  (uintnat)Caml_state->young_ptr)));
}

CAMLprim value caml_gc_minor_words(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLreturn(caml_copy_double(caml_gc_minor_words_unboxed()));
}

CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0 (); /* v is ignored */
  CAMLlocal4 (minwords_, prowords_, majwords_, res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;

  minwords_ = caml_copy_double(minwords);
  prowords_ = caml_copy_double(prowords);
  majwords_ = caml_copy_double(majwords);
  res = caml_alloc_3(0, minwords_, prowords_, majwords_);
  CAMLreturn(res);
}

CAMLprim value caml_gc_get(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  res = caml_alloc_tuple (11);
  Store_field (res, 0, Val_long (Caml_state->minor_heap_wsz));          /* s */
  Store_field (res, 2,
    Val_long (atomic_load_relaxed(&caml_percent_free)));                /* o */
  Store_field (res, 3, Val_long (atomic_load_relaxed(&caml_verb_gc)));  /* v */
  Store_field (res, 5, Val_long (caml_max_stack_wsize));                /* l */
  Store_field (res, 8,
    Val_long (atomic_load_relaxed(&caml_custom_major_ratio)));          /* M */
  Store_field (res, 9,
    Val_long (atomic_load_relaxed(&caml_custom_minor_ratio)));          /* m */
  Store_field (res, 10,
    Val_long (atomic_load_relaxed(&caml_custom_minor_max_bsz)));        /* n */
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
  uintnat newminwsz = caml_norm_minor_heap_size (Long_val (Field (v, 0)));
  uintnat newpf = norm_pfree (Long_val (Field (v, 2)));
  uintnat new_verb_gc = Long_val (Field (v, 3));
  uintnat new_max_stack_size = Long_val (Field (v, 5));
  uintnat new_custom_maj = norm_custom_maj (Long_val (Field (v, 8)));
  uintnat new_custom_min = norm_custom_min (Long_val (Field (v, 9)));
  uintnat new_custom_sz = Long_val (Field (v, 10));

  CAML_EV_BEGIN(EV_EXPLICIT_GC_SET);

  caml_change_max_stack_size (new_max_stack_size);

  if (newpf != atomic_load_relaxed(&caml_percent_free)){
    atomic_store_relaxed(&caml_percent_free, newpf);
    CAML_GC_MESSAGE(PARAMS,
                    "New space overhead: %" ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                    caml_percent_free);
  }

  atomic_store_relaxed(&caml_verb_gc, new_verb_gc);

  /* These fields were added in 4.08.0. */
  if (Wosize_val (v) >= 11){
    if (new_custom_maj != atomic_load_relaxed(&caml_custom_major_ratio)){
      atomic_store_relaxed(&caml_custom_major_ratio, new_custom_maj);
      CAML_GC_MESSAGE(PARAMS, "New custom major ratio: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_major_ratio);
    }
    if (new_custom_min != atomic_load_relaxed(&caml_custom_minor_ratio)){
      atomic_store_relaxed(&caml_custom_minor_ratio, new_custom_min);
      CAML_GC_MESSAGE(PARAMS, "New custom minor ratio: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_minor_ratio);
    }
    if (new_custom_sz != atomic_load_relaxed(&caml_custom_minor_max_bsz)){
      atomic_store_relaxed(&caml_custom_minor_max_bsz, new_custom_sz);
      CAML_GC_MESSAGE(PARAMS, "New custom minor size limit: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_minor_max_bsz);
    }
  }

  /* Minor heap size comes last because it will trigger a minor collection
     (thus invalidating [v]) and it can raise [Out_of_memory]. */
  if (newminwsz != Caml_state->minor_heap_wsz) {
    CAML_GC_MESSAGE(PARAMS, "New minor heap size: %"
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
  Caml_check_caml_state();
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MINOR);
  CAMLassert (v == Val_unit);
  caml_minor_collection ();
  caml_result result = caml_process_pending_actions_res();
  CAML_EV_END(EV_EXPLICIT_GC_MINOR);
  return caml_get_value_or_raise(result);
}

static caml_result gc_major_res(int force_compaction)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR);
  caml_gc_log ("Major GC cycle requested");
  caml_empty_minor_heaps_once();
  caml_finish_major_cycle(force_compaction);
  caml_reset_major_pacing();
  caml_result result = caml_process_pending_actions_res();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR);
  return result;
}

CAMLprim value caml_gc_major(value v)
{
  Caml_check_caml_state();
  CAMLassert (v == Val_unit);
  return caml_get_value_or_raise(gc_major_res(0));
}

static caml_result gc_full_major_res(void)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_FULL_MAJOR);
  caml_gc_log ("Full Major GC requested");
  /* In general, it can require up to 3 GC cycles for a
     currently-unreachable object to be collected. */
  for (int i = 0; i < 3; i++) {
    caml_finish_major_cycle(0);
    caml_reset_major_pacing();
    caml_result res = caml_process_pending_actions_res();
    if (caml_result_is_exception(res)) return res;
  }
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_FULL_MAJOR);
  return Result_unit;
}

CAMLprim value caml_gc_full_major(value v)
{
  Caml_check_caml_state();
  CAMLassert (v == Val_unit);
  return caml_get_value_or_raise(gc_full_major_res());
}

CAMLprim value caml_gc_major_slice (value v)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR_SLICE);
  CAMLassert (Is_long (v));
  caml_major_collection_slice(Long_val(v));
  caml_result result = caml_process_pending_actions_res();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR_SLICE);
  return caml_get_value_or_raise(result);
}

CAMLprim value caml_gc_compaction(value v)
{
  Caml_check_caml_state();
  CAML_EV_BEGIN(EV_EXPLICIT_GC_COMPACT);
  CAMLassert (v == Val_unit);
  caml_result result = Result_unit;
  /* We do a full major before this compaction. See [caml_full_major_res] for
     why this needs three iterations. */
  for (int i = 0; i < 3; i++) {
    caml_finish_major_cycle(i == 2);
    caml_reset_major_pacing();
    result = caml_process_pending_actions_res();
    if (caml_result_is_exception(result)) break;
  }
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
  return caml_get_value_or_raise(result);
}

CAMLprim value caml_gc_stat(value v)
{
  caml_result result;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_STAT);
  result = gc_full_major_res();
  if (caml_result_is_exception(result)) goto out;
  result = Result_value(caml_gc_quick_stat(Val_unit));
 out:
  CAML_EV_END(EV_EXPLICIT_GC_STAT);
  return caml_get_value_or_raise(result);
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
  atomic_store_relaxed(&caml_percent_free,
                       norm_pfree (caml_params->init_percent_free));
  caml_gc_log ("Initial stack limit: %"
               ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
               caml_params->init_max_stack_wsz / 1024 * sizeof (value));

  atomic_store_relaxed(&caml_custom_major_ratio,
                       norm_custom_maj (caml_params->init_custom_major_ratio));
  atomic_store_relaxed(&caml_custom_minor_ratio,
                       norm_custom_min (caml_params->init_custom_minor_ratio));
  atomic_store_relaxed(&caml_custom_minor_max_bsz,
                       caml_params->init_custom_minor_max_bsz);

  caml_gc_phase = Phase_sweep_and_mark_main;
  #ifdef NATIVE_CODE
  caml_init_frame_descriptors();
  #endif
  caml_init_domains(caml_params->max_domains,
                    caml_params->init_minor_heap_wsz);
  caml_init_gc_stats(caml_params->max_domains);

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

CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  return caml_alloc_sprintf
      ("b=%d,c=%"F_Z"u,e=%"F_Z"u,l=%"F_Z"u,M=%"F_Z"u,m=%"F_Z"u,n=%"F_Z"u,"
       "o=%"F_Z"u,p=%d,s=%"F_S"u,t=%"F_Z"u,v=%"F_Z"u,V=%"F_Z"u,W=%"F_Z"u",
       /* b */ (int) Caml_state->backtrace_active,
       /* c */ caml_params->cleanup_on_exit,
       /* e */ caml_params->runtime_events_log_wsize,
       /* l */ caml_max_stack_wsize,
       /* M */ caml_custom_major_ratio,
       /* m */ caml_custom_minor_ratio,
       /* n */ caml_custom_minor_max_bsz,
       /* o */ caml_percent_free,
       /* p */ Caml_state->parser_trace,
       /* R */ /* missing */
       /* s */ Caml_state->minor_heap_wsz,
       /* t */ caml_params->trace_level,
       /* v */ caml_verb_gc,
       /* V */ caml_params->verify_heap,
       /* W */ caml_runtime_warnings
       );
#undef F_Z
#undef F_S
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
