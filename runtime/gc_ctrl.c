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

/* TODO: incorporate timings for instrumented runtime */

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
#include "caml/mlvalues.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "frame_descriptors.h"
#else
#include "caml/fiber.h"
#endif
#include "caml/domain.h"
#include "caml/globroots.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/domain.h"
#include "caml/eventlog.h"
#include "caml/fail.h"

uintnat caml_max_stack_size;
uintnat caml_fiber_wsz;

extern uintnat caml_major_heap_increment; /* percent or words; see major_gc.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_percent_max;          /*        see compact.c */
extern uintnat caml_allocation_policy;    /*        see freelist.c */
extern uintnat caml_custom_major_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_max_bsz; /* see custom.c */

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  struct gc_stats s;
  caml_sample_gc_stats(&s);
  intnat majcoll = Caml_state->stat_major_collections;

  res = caml_alloc_tuple (16);
  Store_field (res, 0, caml_copy_double ((double)s.minor_words));
  Store_field (res, 1, caml_copy_double ((double)s.promoted_words));
  Store_field (res, 2, caml_copy_double ((double)s.major_words));
  Store_field (res, 3, Val_long (s.minor_collections));
  Store_field (res, 4, Val_long (majcoll));
  Store_field (res, 5, Val_long (
    s.major_heap.pool_words + s.major_heap.large_words));
  Store_field (res, 6, Val_long (0));
  Store_field (res, 7, Val_long (
    s.major_heap.pool_live_words + s.major_heap.large_words));
  Store_field (res, 8, Val_long (
    s.major_heap.pool_live_blocks + s.major_heap.large_blocks));
  Store_field (res, 9, Val_long (
    s.major_heap.pool_words - s.major_heap.pool_live_words - s.major_heap.pool_frag_words));
  Store_field (res, 10, Val_long (0));
  Store_field (res, 11, Val_long (0));
  Store_field (res, 12, Val_long (s.major_heap.pool_frag_words));
  Store_field (res, 13, Val_long (0));
  Store_field (res, 14, Val_long (
    s.major_heap.pool_max_words + s.major_heap.large_max_words));
  Store_field (res, 15, Val_long (0));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed()
{
  return (Caml_state->stat_minor_words
          + ((double) (Caml_state->young_end - Caml_state->young_ptr)) / sizeof(value));
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
                    + ((double) Wsize_bsize (Caml_state->young_end -
                                            Caml_state->young_ptr)) / sizeof(value);
  double prowords = Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words + (double) Caml_state->allocated_words;

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
#ifndef NATIVE_CODE
  Store_field (res, 5, Val_long (caml_max_stack_size));         /* l */
#else
  Store_field (res, 5, Val_long (0));
#endif
  Store_field (res, 8, Val_long (caml_custom_major_ratio));     /* M */
  Store_field (res, 9, Val_long (caml_custom_minor_ratio));     /* m */
  Store_field (res, 10, Val_long (caml_custom_minor_max_bsz));  /* n */
  CAMLreturn (res);

  CAMLreturn (res);
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

/*static uintnat norm_pmax (uintnat p)
{
  return p;
}*/

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

#ifndef NATIVE_CODE
  caml_change_max_stack_size (Long_field (v, 5));
#endif

  newpf = norm_pfree (Long_field (v, 2));
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    caml_gc_message (0x20, "New space overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  }

  /* Minor heap size comes last because it will trigger a minor collection
     (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminwsz = caml_norm_minor_heap_size (Long_field (v, 0));
  if (newminwsz != Caml_state->minor_heap_wsz){
    caml_gc_message (0x20, "New minor heap size: %"
                     ARCH_SIZET_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
    caml_set_minor_heap_size (newminwsz);
  }

  /* These fields were added in 4.08.0. */
  if (Wosize_val (v) >= 11){
    new_custom_maj = norm_custom_maj (Field (v, 8));
    if (new_custom_maj != caml_custom_major_ratio){
      caml_custom_major_ratio = new_custom_maj;
      caml_gc_message (0x20, "New custom major ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_major_ratio);
    }
    new_custom_min = norm_custom_min (Field (v, 9));
    if (new_custom_min != caml_custom_minor_ratio){
      caml_custom_minor_ratio = new_custom_min;
      caml_gc_message (0x20, "New custom minor ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_ratio);
    }
    new_custom_sz = Field (v, 10);
    if (new_custom_sz != caml_custom_minor_max_bsz){
      caml_custom_minor_max_bsz = new_custom_sz;
      caml_gc_message (0x20, "New custom minor size limit: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_max_bsz);
    }
  }

  return Val_unit;
}

CAMLprim value caml_gc_minor(value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  caml_minor_collection ();
  CAML_INSTR_TIME (tmr, "explicit/gc_minor");
  return Val_unit;
}

CAMLprim value caml_gc_major(value v)
{
  Assert (v == Val_unit);
  caml_gc_log ("Major GC cycle requested");
  caml_ev_pause(EV_PAUSE_GC);
  caml_empty_minor_heaps_once();
  caml_finish_major_cycle();
  caml_final_do_calls ();
  caml_ev_resume();
  return Val_unit;
}

CAMLprim value caml_gc_full_major(value v)
{
  int i;
  caml_gc_log ("Full Major GC requested");
  caml_ev_pause(EV_PAUSE_GC);
  /* In general, it can require up to 3 GC cycles for a
     currently-unreachable object to be collected. */
  for (i = 0; i < 3; i++) {
    caml_empty_minor_heaps_once();
    caml_finish_major_cycle();
    caml_final_do_calls ();
  }
  caml_ev_resume();
  return Val_unit;
}

CAMLprim value caml_gc_major_slice (value v)
{
  CAMLassert (Is_long (v));
  caml_ev_pause(EV_PAUSE_GC);
  caml_major_collection_slice(Long_val(v));
  caml_handle_gc_interrupt();
  return Val_long (0);
}

CAMLprim value caml_gc_compaction(value v)
{
  return caml_gc_major(v);
}


CAMLprim value caml_gc_stat(value v)
{
  caml_gc_full_major(Val_unit);
  return caml_gc_quick_stat(Val_unit);
}

CAMLprim value caml_get_minor_free (value v)
{
  return Val_int (Caml_state->young_ptr - Caml_state->young_start);
}

void caml_init_gc ()
{
  caml_max_stack_size = caml_params->init_max_stack_wsz;
  caml_fiber_wsz = caml_params->init_fiber_wsz;
  caml_percent_free = norm_pfree (caml_params->init_percent_free);
  caml_gc_log ("Initial stack limit: %luk bytes",
               caml_max_stack_size / 1024 * sizeof (value));

  caml_custom_major_ratio =
      norm_custom_maj (caml_params->init_custom_major_ratio);
  caml_custom_minor_ratio =
      norm_custom_min (caml_params->init_custom_minor_ratio);
  caml_custom_minor_max_bsz = caml_params->init_custom_minor_max_bsz;

  caml_setup_eventlog();
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
