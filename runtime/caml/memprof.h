/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          */
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
#include "roots.h"

extern void caml_memprof_update_suspended(_Bool);

extern value caml_memprof_handle_postponed_exn(void);

extern void caml_memprof_track_alloc_shr(value block);
extern void caml_memprof_track_custom(value block, mlsize_t bytes);
extern void caml_memprof_track_young(uintnat wosize, int from_caml,
                                     int nallocs, unsigned char* alloc_lens);
extern void caml_memprof_track_interned(header_t* block, header_t* blockend);

extern void caml_memprof_renew_minor_sample(caml_domain_state *state);

extern void caml_memprof_after_minor_gc(caml_domain_state *state);
extern void caml_memprof_after_minor_gc_global(void);

extern void caml_memprof_after_major_gc(caml_domain_state *state);

extern void caml_memprof_oldify_young_roots(void);
extern void caml_memprof_do_roots(scanning_action f);
extern void caml_memprof_update_clean_phase(void);

/* Multi-domain support. */

extern void caml_memprof_new_domain(caml_domain_state *parent,
                                    caml_domain_state *domain);
extern void caml_memprof_delete_domain(caml_domain_state *domain);

typedef struct memprof_thread_s *memprof_thread_t;

CAMLextern memprof_thread_t caml_memprof_main_thread(caml_domain_state *domain);
CAMLextern memprof_thread_t caml_memprof_new_thread(caml_domain_state *domain);
CAMLextern void caml_memprof_leave_thread(void);
CAMLextern void caml_memprof_enter_thread(memprof_thread_t);
CAMLextern void caml_memprof_delete_thread(memprof_thread_t);

#endif

#endif /* CAML_MEMPROF_H */
