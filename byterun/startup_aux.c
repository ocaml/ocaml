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
#include "caml/osdeps.h"
#include "caml/startup_aux.h"


/* Initialize the atom table */

CAMLexport header_t caml_atom_table[256];
void caml_init_atom_table(void)
{
  int i;
  for(i = 0; i < 256; i++) {
#ifdef NATIVE_CODE
    caml_atom_table[i] = Make_header_allocated_here(0, i, Caml_white);
#else
    caml_atom_table[i] = Make_header(0, i, Caml_white);
#endif
  }
  if (caml_page_table_add(In_static_data,
                          caml_atom_table, caml_atom_table + 256) != 0) {
    caml_fatal_error("Fatal error: not enough memory for initial page table");
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
extern int caml_parser_trace;
uintnat caml_trace_level = 0;


static void scanmult (char *opt, uintnat *var)
{
  char mult = ' ';
  unsigned int val = 1;
  sscanf (opt, "=%u%c", &val, &mult);
  sscanf (opt, "=0x%x%c", &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * (1024 * 1024); break;
  case 'G':   *var = (uintnat) val * (1024 * 1024 * 1024); break;
  default:    *var = (uintnat) val; break;
  }
}

void caml_parse_ocamlrunparam(void)
{
  char *opt = caml_secure_getenv ("OCAMLRUNPARAM");
  uintnat p;

  if (opt == NULL) opt = caml_secure_getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 'a': scanmult (opt, &p); caml_set_allocation_policy (p); break;
      case 'b': scanmult (opt, &p); caml_record_backtrace(Val_bool (p)); break;
      case 'h': scanmult (opt, &caml_init_heap_wsz); break;
      case 'H': scanmult (opt, &caml_use_huge_pages); break;
      case 'i': scanmult (opt, &caml_init_heap_chunk_sz); break;
      case 'l': scanmult (opt, &caml_init_max_stack_wsz); break;
      case 'o': scanmult (opt, &caml_init_percent_free); break;
      case 'O': scanmult (opt, &caml_init_max_percent_free); break;
      case 'p': scanmult (opt, &p); caml_parser_trace = p; break;
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
