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
#include "caml/memprof.h"


#ifdef _WIN32
extern void caml_win32_unregister_overflow_detection (void);
#endif

CAMLexport header_t *caml_atom_table = NULL;

/* Initialize the atom table */
void caml_init_atom_table(void)
{
  caml_stat_block b;
  int i;

  /* PR#9128: We need to give the atom table its own page to make sure
     it does not share a page with a non-value, which would break code
     which depend on the correctness of the page table. For example,
     if the atom table shares a page with bytecode, then functions in
     the runtime may decide to follow a code pointer in a closure, as
     if it were a pointer to a value.

     We add 1 padding at the end of the atom table because the atom
     pointer actually points to the word *following* the corresponding
     entry in the table (the entry is an empty block *header*).
  */
  asize_t request = (256 + 1) * sizeof(header_t);
  request = (request + Page_size - 1) / Page_size * Page_size;
  caml_atom_table =
    caml_stat_alloc_aligned_noexc(request, 0, &b);

  for(i = 0; i < 256; i++) {
#ifdef NATIVE_CODE
    caml_atom_table[i] = Make_header_allocated_here(0, i, Caml_white);
#else
    caml_atom_table[i] = Make_header(0, i, Caml_white);
#endif
  }
  if (caml_page_table_add(In_static_data,
                          caml_atom_table, caml_atom_table + 256 + 1) != 0) {
    caml_fatal_error("not enough memory for initial page table");
  }
}


/* Parse the OCAMLRUNPARAM environment variable. */

uintnat caml_init_percent_free = Percent_free_def;
uintnat caml_init_max_percent_free = Max_percent_free_def;
uintnat caml_init_minor_heap_wsz = Minor_heap_def;
uintnat caml_init_heap_chunk_sz = Heap_chunk_def;
uintnat caml_init_heap_wsz = Init_heap_def;
uintnat caml_init_max_stack_wsz = Max_stack_def;
uintnat caml_init_major_window = Major_window_def;
uintnat caml_init_custom_major_ratio = Custom_major_ratio_def;
uintnat caml_init_custom_minor_ratio = Custom_minor_ratio_def;
uintnat caml_init_custom_minor_max_bsz = Custom_minor_max_bsz_def;
extern int caml_parser_trace;
uintnat caml_trace_level = 0;
int caml_cleanup_on_exit = 0;


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
  char_os *opt = caml_secure_getenv (T("OCAMLRUNPARAM"));
  uintnat p;

  if (opt == NULL) opt = caml_secure_getenv (T("CAMLRUNPARAM"));

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'a': scanmult (opt, &p); caml_set_allocation_policy ((intnat) p);
        break;
      case 'b': scanmult (opt, &p); caml_record_backtrace(Val_bool (p));
        break;
      case 'c': scanmult (opt, &p); caml_cleanup_on_exit = (p != 0); break;
      case 'h': scanmult (opt, &caml_init_heap_wsz); break;
      case 'H': scanmult (opt, &caml_use_huge_pages); break;
      case 'i': scanmult (opt, &caml_init_heap_chunk_sz); break;
      case 'l': scanmult (opt, &caml_init_max_stack_wsz); break;
      case 'M': scanmult (opt, &caml_init_custom_major_ratio); break;
      case 'm': scanmult (opt, &caml_init_custom_minor_ratio); break;
      case 'n': scanmult (opt, &caml_init_custom_minor_max_bsz); break;
      case 'o': scanmult (opt, &caml_init_percent_free); break;
      case 'O': scanmult (opt, &caml_init_max_percent_free); break;
      case 'p': scanmult (opt, &p); caml_parser_trace = (p != 0); break;
      case 'R': break; /*  see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &caml_init_minor_heap_wsz); break;
      case 't': scanmult (opt, &caml_trace_level); break;
      case 'v': scanmult (opt, &caml_verb_gc); break;
      case 'w': scanmult (opt, &caml_init_major_window); break;
      case 'W': scanmult (opt, &caml_runtime_warnings); break;
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
  caml_memprof_shutdown();
  caml_free_locale();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
  caml_stat_destroy_pool();
#if defined(_WIN32) && defined(NATIVE_CODE)
  caml_win32_unregister_overflow_detection();
#endif

  shutdown_happened = 1;
}
