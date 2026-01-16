/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Gabriel Scherer, projet Partout, INRIA Saclay              */
/*                                                                        */
/*   Copyright 2022 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_GC_STATS_H
#define CAML_GC_STATS_H

#ifdef CAML_INTERNALS

#include "domain.h"

/* The Gc module provides two kind of runtime statistics:
   - Heap stats: statistics about heap memory, see shared_heap.c
   - Allocation stats: statistics about past allocations and collections,
     see major_gc.c

   To avoid synchronization costs, each domain tracks its own statistics.
   We never access the "live stats" of other domains running concurrently,
   only their "sampled stats", a past version of the live stats saved
   at a safepoint -- during stop-the-world minor collections and
   on domain termination.
*/

struct heap_stats {
  intnat pool_words;
  intnat pool_max_words;
  intnat pool_live_words;
  intnat pool_live_blocks;
  intnat pool_frag_words;
  intnat large_words;
  intnat large_max_words;
  intnat large_blocks;
};

/* Note: accumulating stats then removing them is not a no-op, as
   accumulating updates maximum values.

   For example, if pool_words and pool_max_words are initially 0,
   adding 10 then removing 10 will reset pool_words to 0 but leave
   pool_max_words at 10. */
void caml_accum_heap_stats(struct heap_stats* acc, const struct heap_stats* s);
void caml_remove_heap_stats(struct heap_stats* acc, const struct heap_stats* s);

struct alloc_stats {
  uint64_t minor_words;
  uint64_t promoted_words;
  uint64_t major_words;
  uint64_t forced_major_collections;
};
void caml_accum_alloc_stats(
  struct alloc_stats* acc,
  const struct alloc_stats* s);
void caml_collect_alloc_stats_sample(
  caml_domain_state *local,
  struct alloc_stats *sample);

struct gc_stats {
  struct alloc_stats alloc_stats;
  struct heap_stats heap_stats;
};

void caml_orphan_alloc_stats(caml_domain_state *);

/* Update the sampled stats of a domain from its live stats.
   May only be called during STW, so that it does not race
   with mutators calling [caml_compute_gc_stats]. */
void caml_collect_gc_stats_sample_stw(caml_domain_state *domain);

/* Compute global runtime stats.

   The result is an approximation, it uses the live stats of the
   current domain but the sampled stats of other domains. */
void caml_compute_gc_stats(struct gc_stats* buf);

void caml_init_gc_stats (uintnat max_domains);

void caml_free_gc_stats(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_STATS_H */
