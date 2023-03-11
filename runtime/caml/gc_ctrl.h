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

#ifndef CAML_GC_CTRL_H
#define CAML_GC_CTRL_H

#ifdef CAML_INTERNALS

#include "misc.h"

CAMLextern uintnat caml_max_stack_wsize;
CAMLextern uintnat caml_fiber_wsz;
CAMLextern uintnat caml_major_cycles_completed;

void caml_init_gc (void);
value caml_gc_stat(value);
value caml_gc_major(value);


#define caml_stat_top_heap_wsz caml_top_heap_words(Caml_state->shared_heap)
#define caml_stat_compactions 0
#define caml_stat_heap_wsz Wsize_bsize(caml_heap_size(Caml_state->shared_heap))
#define caml_stat_heap_chunks caml_heap_blocks(Caml_state->shared_heap)
#define caml_stat_major_collections caml_major_cycles_completed
#define caml_stat_promoted_words Caml_state->stat_promoted_words
#define caml_allocated_words Caml_state->allocated_words
#define caml_stat_major_words Caml_state->stat_major_words
#define caml_stat_minor_words Caml_state->stat_minor_words

#ifdef DEBUG
void caml_heap_check (void);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_CTRL_H */
