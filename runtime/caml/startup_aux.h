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

extern void caml_init_locale(void);
extern void caml_free_locale(void);

/* readonly after startup */
struct caml_params {
  const char_os* exe_name;

  /* for meta.c */
  const char* section_table;
  asize_t section_table_size;

  const char_os* cds_file;

  uintnat parser_trace;
  uintnat trace_level;
  uintnat runtime_events_log_wsize;
  uintnat verify_heap;
  uintnat print_magic;
  uintnat print_config;

  uintnat init_percent_free;
  uintnat init_minor_heap_wsz;
  uintnat init_custom_major_ratio;
  uintnat init_custom_minor_ratio;
  uintnat init_custom_minor_max_bsz;

  uintnat init_max_stack_wsz;

  uintnat backtrace_enabled;
  uintnat runtime_warnings;
  uintnat cleanup_on_exit;
};

extern const struct caml_params* const caml_params;

extern void caml_parse_ocamlrunparam (void);

/* Common entry point to caml_startup.
   Returns 0 if the runtime is already initialized.
   If [pooling] is 0, [caml_stat_*] functions will not be backed by a pool. */
extern int caml_startup_aux (int pooling);

void caml_init_exe_name(const char_os* exe_name);
void caml_init_section_table(const char* section_table,
                             asize_t section_table_size);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_AUX_H */
