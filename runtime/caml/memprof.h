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

/*** Track allocations ***/

/* Possibly track an allocation on the major heap. Called on every
 * major-heap allocation, after the header has been written. */

/* Possibly track an allocation of a custom block. Called on every
 * custom block allocation, after the header has been written. */
enum { CAML_MEMPROF_SRC_NORMAL = 0,
       CAML_MEMPROF_SRC_MARSHAL = 1, /* interning */
       CAML_MEMPROF_SRC_CUSTOM = 2 /* custom memory */ };


void caml_memprof_sample_block(value block, size_t allocated_words,
                               size_t sampled_words, int source);

/* Track a minor heap "Comballoc" (combined allocation). Called when
 * the memprof trigger is hit (before the allocation is actually
 * performed, which may require a GC). `allocs` and `alloc_lens`
 * describe the combined allocation. Runs allocation callbacks. */

extern void caml_memprof_sample_young(uintnat wosize, int from_caml,
                                      int allocs, unsigned char* alloc_lens);


/*** GC interface ***/

/* Apply `f(fdata, r, &r)` to each GC root `r` within memprof data
 * structures for the domain `state`.
 *
 * `fflags` is used to decide whether to only scan roots which may
 * point to minor heaps (the `SCANNING_ONLY_YOUNG_VALUES` flag).
 *
 * If `weak` is false then only scan strong roots. If `weak`
 * is true then also scan weak roots.
 *
 * If `global` is false then only scan roots for `state`. If `global`
 * is true then also scan roots shared between all domains. */

extern void caml_memprof_scan_roots(scanning_action f,
                                    scanning_action_flags fflags,
                                    void* fdata,
                                    caml_domain_state *state,
                                    _Bool weak,
                                    _Bool global);

/* Update memprof data structures for the domain `state`, to reflect
 * survival and promotion, after a minor GC is completed.
 *
 * If `global` is false then only update structures for `state`. If
 * `global` is true then also update structures shared between all
 * domains. */

extern void caml_memprof_after_minor_gc(caml_domain_state *state, _Bool global);

/* Update memprof data structures for the domain `state`, to reflect
 * survival, after a minor GC is completed.
 *
 * If `global` is false then only update structures for `state`. If
 * `global` is true then also update structures shared between all
 * domains. */

extern void caml_memprof_after_major_gc(caml_domain_state *state, _Bool global);


/*** Callbacks ***/

/* Run any pending callbacks for the current domain (or adopted from a
 * terminated domain). */

extern value caml_memprof_run_callbacks_exn(void);

/* Suspend or unsuspend profiling */
extern void caml_memprof_update_suspended(_Bool);

/* Freshly set sampling point on minor heap */
extern void caml_memprof_renew_minor_sample(caml_domain_state *state);

/* Multi-domain support. */

extern void caml_memprof_new_domain(caml_domain_state *parent,
                                    caml_domain_state *domain);
extern void caml_memprof_delete_domain(caml_domain_state *domain);

/* Multi-thread support */

typedef struct memprof_thread_s *memprof_thread_t;

CAMLextern memprof_thread_t caml_memprof_main_thread(caml_domain_state *domain);
CAMLextern memprof_thread_t caml_memprof_new_thread(caml_domain_state *domain);
CAMLextern void caml_memprof_enter_thread(memprof_thread_t);
CAMLextern void caml_memprof_delete_thread(memprof_thread_t);

#endif

#endif /* CAML_MEMPROF_H */
