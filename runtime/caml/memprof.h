/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Jacques-Henri Joudan, projet Gallium, INRIA Paris          */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MEMPROF_H
#define CAML_MEMPROF_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"

#ifdef WITH_STATMEMPROF

extern void caml_memprof_renew_minor_sample(void);

extern value* caml_memprof_young_limit;

extern value caml_memprof_track_alloc_shr(tag_t tag, value block);
extern void caml_memprof_postpone_track_alloc_shr(value block);
extern void caml_memprof_track_young(tag_t tag, uintnat wosize);
extern void caml_memprof_track_interned(header_t* block, header_t* blockend);
extern void caml_memprof_handle_postponed();

#ifdef NATIVE_CODE
extern void caml_memprof_call_gc_end(double exceeded_by);
extern double caml_memprof_call_gc_begin(void);
#endif

/* Exported only for saving and restoring in threads. */
extern int caml_memprof_suspended;
extern void caml_memprof_set_suspended(int new_suspended);

#endif

#endif

#endif /* CAML_MEMPROF_H */
