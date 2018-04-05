/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Damien Doligez, Jane Street Group, LLC                */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STARTUP_AUX_H
#define CAML_STARTUP_AUX_H

#ifdef CAML_INTERNALS

#include "config.h"

/* readonly after startup */
struct caml_params {
  const char* exe_name;
  const char* const* main_argv;

  /* for meta.c */
  const char* section_table;
  asize_t section_table_size;

  const char* cds_file;

  uintnat verb_gc;
  uintnat parser_trace;
  uintnat trace_level;
  uintnat eventlog_enabled;
  uintnat verify_heap;
  uintnat print_stats;

  uintnat init_percent_free;
  uintnat init_max_percent_free;
  uintnat init_minor_heap_wsz;
  uintnat init_heap_chunk_sz;
  uintnat init_heap_wsz;
  uintnat init_max_stack_wsz;
  uintnat init_fiber_wsz;
  uintnat profile_slop_wsz;

  uintnat backtrace_enabled_init;
  uintnat runtime_warnings;
  uintnat cleanup_on_exit;
};

extern const struct caml_params* const caml_params;

extern void caml_parse_ocamlrunparam (void);
extern int caml_parse_command_line (char_os **argv);

/* Common entry point to caml_startup.
   Returns 0 if the runtime is already initialized.
   If [pooling] is 0, [caml_stat_*] functions will not be backed by a pool. */
extern int caml_startup_aux (int pooling);

void caml_init_argv(const char* exe_name, char** main_argv);
void caml_init_section_table(const char* section_table,
                             asize_t section_table_size);
value caml_maybe_print_stats (value v);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_AUX_H */
