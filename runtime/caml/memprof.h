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

/*** Sample allocations ***/

/* [Gc.Memprof.allocation_source] */

enum { CAML_MEMPROF_SRC_NORMAL = 0,
       CAML_MEMPROF_SRC_MARSHAL = 1, /* interning */
       CAML_MEMPROF_SRC_CUSTOM = 2 /* custom memory */ };

/* Respond to the allocation of any block. Does not call callbacks.
 * `block` is the allocated block, to be tracked by memprof if
 * sampled. `allocated_words` is the number of words allocated, to be
 * passed to the allocation callback. `sampled_words` is the number of
 * words to use when computing the number of samples (this will
 * normally be one more than `allocated words` due to the header word,
 * but may not be for out-of-heap memory). `source` is one of the
 * `CAML_MEMPROF_SRC_* constants above. */

void caml_memprof_sample_block(value block, size_t allocated_words,
                               size_t sampled_words, int source);

/* Sample a minor heap "Comballoc" (combined allocation). Called when
 * the memprof trigger is hit (before the allocation is actually
 * performed, which may require a GC). `allocs` and `alloc_lens`
 * describe the combined allocation. Runs allocation callbacks. */

extern void caml_memprof_sample_young(uintnat wosize, int from_caml,
                                      int allocs, unsigned char* alloc_lens);

/* Suspend or unsuspend sampling (for the current thread). */

extern void caml_memprof_update_suspended(_Bool);


/*** GC interface ***/

/* Apply `f(fdata, r, &r)` to each GC root `r` within memprof data
 * structures for the domain `state`.
 *
 * `fflags` is used to decide whether to only scan roots which may
 * point to minor heaps (the `SCANNING_ONLY_YOUNG_VALUES` flag).
 *
 * If `weak` is false then only scan strong roots. If `weak`
 * is true then also scan weak roots. */

extern void caml_memprof_scan_roots(scanning_action f,
                                    scanning_action_flags fflags,
                                    void* fdata,
                                    caml_domain_state *state,
                                    _Bool weak);

/* Update memprof data structures for the domain `state`, to reflect
 * survival and promotion, after a minor GC is completed. */

extern void caml_memprof_after_minor_gc(caml_domain_state *state);

/* Update memprof data structures for the domain `state`, to reflect
 * survival, after a minor GC is completed. */

extern void caml_memprof_after_major_gc(caml_domain_state *state);

/* Freshly computes state->memprof_young_trigger. *Does not* set the
 * young limit. */

extern void caml_memprof_set_trigger(caml_domain_state *state);

/*** Callbacks ***/

/* Run any pending callbacks for the current domain (or adopted from a
 * terminated domain). */

extern caml_result caml_memprof_run_callbacks_res(void);


/*** Multi-domain support. ***/

/* Notify memprof of the creation of a new domain `domain`. If there
 * was an existing domain (from which to inherit profiling behaviour),
 * it is passed in `parent`. Called before the new domain allocates
 * anything, and before the parent domain continues. Also creates
 * memprof thread state for the initial thread of the domain. */

extern void caml_memprof_new_domain(caml_domain_state *parent,
                                    caml_domain_state *domain);

/* Notify memprof that the domain `domain` is terminating. Called
 * after the last allocation by the domain. */

extern void caml_memprof_delete_domain(caml_domain_state *domain);


/*** Multi-thread support ***/

/* Opaque type of memprof state for a single thread. */

typedef struct memprof_thread_s *memprof_thread_t;

/* Notify memprof that a new thread is being created. Returns a
 * pointer to memprof state for the new thread. */

CAMLextern memprof_thread_t caml_memprof_new_thread(caml_domain_state *domain);

/* Obtain the memprof state for the initial thread of a domain. Called
 * when there is only one such thread. */

CAMLextern memprof_thread_t caml_memprof_main_thread(caml_domain_state *domain);

/* Notify memprof that the current domain is switching to the given
 * thread. */

CAMLextern void caml_memprof_enter_thread(memprof_thread_t);

/* Notify memprof that the given thread is being deleted. */

CAMLextern void caml_memprof_delete_thread(memprof_thread_t);

#endif /* CAML_INTERNALS */

#endif /* CAML_MEMPROF_H */
