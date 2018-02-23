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

void caml_init_atom_table (void);

CAMLdata uintnat caml_init_percent_free;
CAMLdata uintnat caml_init_max_percent_free;
CAMLdata uintnat caml_init_minor_heap_wsz;
CAMLdata uintnat caml_init_heap_chunk_sz;
CAMLdata uintnat caml_init_heap_wsz;
CAMLdata uintnat caml_init_max_stack_wsz;
CAMLdata uintnat caml_init_major_window;
CAMLdata uintnat caml_trace_level;
CAMLdata uintnat caml_cleanup_on_exit;

void caml_parse_ocamlrunparam (void);

/* Common entry point to caml_startup.
   Returns 0 if the runtime is already initialized.
   If [pooling] is 0, [caml_stat_*] functions will not be backed by a pool. */
int caml_startup_aux (int pooling);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_AUX_H */
