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

/* Track allocations */
extern void caml_memprof_track_alloc_shr(value block);
extern void caml_memprof_track_custom(value block, mlsize_t bytes);
extern void caml_memprof_track_young(uintnat wosize, int from_caml,
                                     int nallocs, unsigned char* alloc_lens);

/* GC interface */

extern void caml_memprof_scan_roots(scanning_action f,
                                    scanning_action_flags fflags,
                                    void* fdata,
                                    caml_domain_state *domain,
                                    _Bool young,
                                    _Bool global);

extern void caml_memprof_after_minor_gc(caml_domain_state *state, _Bool global);

extern void caml_memprof_after_major_gc(caml_domain_state *state, _Bool global);

extern void caml_memprof_set_suspended(int);

extern value caml_memprof_handle_postponed_exn(void);

extern void caml_memprof_renew_minor_sample(void);

CAMLextern struct caml_memprof_th_ctx caml_memprof_main_ctx;

CAMLextern struct caml_memprof_th_ctx* caml_memprof_new_th_ctx(void);
CAMLextern void caml_memprof_leave_thread(void);
CAMLextern void caml_memprof_enter_thread(struct caml_memprof_th_ctx*);
CAMLextern void caml_memprof_delete_th_ctx(struct caml_memprof_th_ctx*);

typedef void (*th_ctx_action)(struct caml_memprof_th_ctx*, void*);

/* This hook is not modified after other domains are spawned. */
extern void (*caml_memprof_th_ctx_iter_hook)(th_ctx_action, void*);

#endif

#endif /* CAML_MEMPROF_H */
