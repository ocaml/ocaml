/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
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

/* Some runtime initialization functions that are common to bytecode
   and native code. */

#include <stdio.h>
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/domain.h"
#include "caml/major_gc.h"
#ifndef NATIVE_CODE
#include "caml/dynlink.h"
#endif
#include "caml/gc_stats.h"
#include "caml/osdeps.h"
#include "caml/shared_heap.h"
#include "caml/startup_aux.h"
#include "caml/prims.h"
#include "caml/signals.h"

#ifdef _WIN32
extern void caml_win32_unregister_overflow_detection (void);
#endif

/* Configuration parameters and flags */

static struct caml_params params;
const struct caml_params* const caml_params = &params;

static void init_startup_params(void)
{
#ifndef NATIVE_CODE
  char_os * cds_file;
#endif

  params.init_percent_free = Percent_free_def;
  params.init_minor_heap_wsz = Minor_heap_def;
  params.init_custom_major_ratio = Custom_major_ratio_def;
  params.init_custom_minor_ratio = Custom_minor_ratio_def;
  params.init_custom_minor_max_bsz = Custom_minor_max_bsz_def;
  params.init_max_stack_wsz = Max_stack_def;
  params.max_domains = Max_domains_def;
  params.runtime_events_log_wsize = Default_runtime_events_log_wsize;

#ifdef DEBUG
  atomic_store_relaxed(&caml_verb_gc, CAML_GC_MSG_VERBOSE | CAML_GC_MSG_MINOR);
#endif
#ifndef NATIVE_CODE
  cds_file = caml_secure_getenv(T("CAML_DEBUG_FILE"));
  if (cds_file != NULL) {
    params.cds_file = caml_stat_strdup_os(cds_file);
  }
  params.section_table = caml_section_table;
  params.section_table_size = caml_section_table_size;
#endif
  params.trace_level = 0;
  params.cleanup_on_exit = 0;
  params.print_magic = 0;
  params.print_config = 0;
  params.event_trace = 0;
}

static void scanmult (char_os *opt, uintnat *var)
{
  char_os mult = ' ';
  unsigned int val = 1;
  sscanf_os (opt, T("=%u%c"), &val, &mult);
  sscanf_os (opt, T("=0x%x%c"), &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * (1024 * 1024); break;
  case 'G':   *var = (uintnat) val * (1024 * 1024 * 1024); break;
  default:    *var = (uintnat) val; break;
  }
}

void caml_parse_ocamlrunparam(void)
{
  init_startup_params();
  uintnat val;

  char_os *opt = caml_secure_getenv (T("OCAMLRUNPARAM"));
  if (opt == NULL) opt = caml_secure_getenv (T("CAMLRUNPARAM"));

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'b': scanmult (opt, &params.backtrace_enabled); break;
      case 'c': scanmult (opt, &params.cleanup_on_exit); break;
      case 'd': scanmult (opt, &params.max_domains); break;
      case 'e': scanmult (opt, &params.runtime_events_log_wsize); break;
      case 'l': scanmult (opt, &params.init_max_stack_wsz); break;
      case 'M': scanmult (opt, &params.init_custom_major_ratio); break;
      case 'm': scanmult (opt, &params.init_custom_minor_ratio); break;
      case 'n': scanmult (opt, &params.init_custom_minor_max_bsz); break;
      case 'o': scanmult (opt, &params.init_percent_free); break;
      case 'p': scanmult (opt, &params.parser_trace); break;
      case 'R': break; /*  see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &params.init_minor_heap_wsz); break;
      case 't': scanmult (opt, &params.trace_level); break;
      case 'v':
        scanmult (opt, &val);
        atomic_store_relaxed(&caml_verb_gc, val);
        break;
      case 'V': scanmult (opt, &params.verify_heap); break;
      case 'W': scanmult (opt, &caml_runtime_warnings); break;
      case ',': continue;
      }
      while (*opt != '\0'){
        if (*opt++ == ',') break;
      }
    }
  }

  /* Validate */
  if (params.max_domains < 1) {
    caml_fatal_error("OCAMLRUNPARAM: max_domains(d) must be at least 1");
  }
  if (params.max_domains > Max_domains_max) {
    caml_fatal_error("OCAMLRUNPARAM: max_domains(d) is too large. "
                     "The maximum value is %d.", Max_domains_max);
  }
}


/* The number of outstanding calls to caml_startup */
static int startup_count = 0;

/* Has the runtime been shut down already? */
static int shutdown_happened = 0;


int caml_startup_aux(int pooling)
{
  if (shutdown_happened == 1)
    caml_fatal_error("caml_startup was called after the runtime "
                     "was shut down with caml_shutdown");

#ifdef DEBUG
  /* Note this must be executed after the call to caml_parse_ocamlrunparam. */
  CAML_GC_MESSAGE(ANY, "### OCaml runtime: debug mode ###\n");
  CAML_GC_MESSAGE(ANY, "### set OCAMLRUNPARAM=v=0 to silence this message\n");
#endif

  /* Second and subsequent calls are ignored,
     since the runtime has already started */
  startup_count++;
  if (startup_count > 1)
    return 0;

  if (pooling)
    caml_stat_create_pool();

  return 1;
}

static void call_registered_value(const char* name)
{
  const value *f = caml_named_value(name);
  if (f != NULL)
    caml_callback_res(*f, Val_unit);
}

CAMLexport void caml_shutdown(void)
{
  Caml_check_caml_state();

  if (startup_count <= 0)
    caml_fatal_error("a call to caml_shutdown has no "
                     "corresponding call to caml_startup");

  /* Do nothing unless it's the last call remaining */
  startup_count--;
  if (startup_count > 0)
    return;

  call_registered_value("Pervasives.do_at_exit");
  call_registered_value("Thread.at_shutdown");
  if (!caml_domain_alone()) {
    caml_gc_log("Some domains have not been joined prior to shutdown");
    caml_stop_all_domains();
  } else {
    /* These calls are not safe to use if there are domains left running */
    caml_domain_terminate(true);
    caml_finalise_freelist();
  }
  caml_free_gc_stats();
  caml_free_locale();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
  if (caml_free_domains())
    caml_stat_destroy_pool();
  caml_terminate_signals();
#if defined(_WIN32) && defined(NATIVE_CODE)
  caml_win32_unregister_overflow_detection();
#endif

  shutdown_happened = 1;
}

void caml_init_exe_name(const char_os* exe_name)
{
  params.exe_name = exe_name;
}
