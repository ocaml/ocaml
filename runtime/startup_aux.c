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
#include "caml/major_gc.h"
#ifndef NATIVE_CODE
#include "caml/dynlink.h"
#endif
#include "caml/osdeps.h"
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
  params.runtime_events_log_wsize = Default_runtime_events_log_wsize;

#ifdef DEBUG
  atomic_store_relaxed(&caml_verb_gc, 0x3F);
#endif
#ifndef NATIVE_CODE
  cds_file = caml_secure_getenv(T("CAML_DEBUG_FILE"));
  if (cds_file != NULL) {
    params.cds_file = caml_stat_strdup_os(cds_file);
  }
#endif
  params.trace_level = 0;
  params.cleanup_on_exit = 0;
  params.print_magic = 0;
  params.print_config = 0;
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
  case 'v':   atomic_store_relaxed((atomic_uintnat *)var, val);
  default:    *var = (uintnat) val; break;
  }
}

void caml_parse_ocamlrunparam(void)
{
  init_startup_params();

  char_os *opt = caml_secure_getenv (T("OCAMLRUNPARAM"));
  if (opt == NULL) opt = caml_secure_getenv (T("CAMLRUNPARAM"));

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'b': scanmult (opt, &params.backtrace_enabled); break;
      case 'c': scanmult (opt, &params.cleanup_on_exit); break;
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
      case 'v': scanmult (opt, (uintnat *)&caml_verb_gc); break;
      case 'V': scanmult (opt, &params.verify_heap); break;
      case 'W': scanmult (opt, &caml_runtime_warnings); break;
      case ',': continue;
      }
      while (*opt != '\0'){
        if (*opt++ == ',') break;
      }
    }
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

  /* Second and subsequent calls are ignored,
     since the runtime has already started */
  startup_count++;
  if (startup_count > 1)
    return 0;

  if (pooling)
    caml_stat_create_pool();

  return 1;
}

static void call_registered_value(char* name)
{
  const value *f = caml_named_value(name);
  if (f != NULL)
    caml_callback_exn(*f, Val_unit);
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
  caml_finalise_heap();
  caml_free_locale();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
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

void caml_init_section_table(const char* section_table,
                             asize_t section_table_size)
{
  params.section_table = section_table;
  params.section_table_size = section_table_size;
}
