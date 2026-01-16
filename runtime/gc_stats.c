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

#define CAML_INTERNALS

#include "caml/gc_stats.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/platform.h"
#include "caml/shared_heap.h"
#include "caml/startup_aux.h"

Caml_inline intnat intnat_max(intnat a, intnat b) {
  return (a > b ? a : b);
}

void caml_accum_heap_stats(struct heap_stats* acc, const struct heap_stats* h)
{
  acc->pool_words += h->pool_words;
  acc->pool_max_words = intnat_max(acc->pool_max_words, acc->pool_words);
  acc->pool_max_words = intnat_max(acc->pool_max_words, h->pool_max_words);
  acc->pool_live_words += h->pool_live_words;
  acc->pool_live_blocks += h->pool_live_blocks;
  acc->pool_frag_words += h->pool_frag_words;
  acc->large_words += h->large_words;
  acc->large_max_words = intnat_max(acc->large_max_words, acc->large_words);
  acc->large_max_words = intnat_max(acc->large_max_words, h->large_max_words);
  acc->large_blocks += h->large_blocks;
}

void caml_remove_heap_stats(struct heap_stats* acc, const struct heap_stats* h)
{
  acc->pool_words -= h->pool_words;
  acc->pool_live_words -= h->pool_live_words;
  acc->pool_live_blocks -= h->pool_live_blocks;
  acc->pool_frag_words -= h->pool_frag_words;
  acc->large_words -= h->large_words;
  acc->large_blocks -= h->large_blocks;
}

void caml_accum_alloc_stats(
  struct alloc_stats* acc,
  const struct alloc_stats* s)
{
  acc->minor_words += s->minor_words;
  acc->promoted_words += s->promoted_words;
  acc->major_words += s->major_words;
  acc->forced_major_collections += s->forced_major_collections;
}

void caml_collect_alloc_stats_sample(
  caml_domain_state *local,
  struct alloc_stats *sample)
{
  sample->minor_words = local->stat_minor_words;
  sample->promoted_words = local->stat_promoted_words;
  sample->major_words = local->stat_major_words;
  sample->forced_major_collections = local->stat_forced_major_collections;
}

void caml_reset_domain_alloc_stats(caml_domain_state *local)
{
  local->stat_minor_words = 0;
  local->stat_promoted_words = 0;
  local->stat_major_words = 0;
  local->stat_forced_major_collections = 0;
}

/* We handle orphaning allocation stats here,
   whereas orphaning of heap stats is done in shared_heap.c */
static caml_plat_mutex orphan_lock = CAML_PLAT_MUTEX_INITIALIZER;
static struct alloc_stats orphaned_alloc_stats = {0,};

void caml_accum_orphan_alloc_stats(struct alloc_stats *acc)
{
  /* This is called from the collector as well as from the mutator. */
  caml_plat_lock_blocking(&orphan_lock);
  caml_accum_alloc_stats(acc, &orphaned_alloc_stats);
  caml_plat_unlock(&orphan_lock);
}

void caml_orphan_alloc_stats(caml_domain_state *domain) {
  struct alloc_stats alloc_stats;

  /* move alloc stats from the domain to [alloc_stats] */
  caml_collect_alloc_stats_sample(domain, &alloc_stats);
  caml_reset_domain_alloc_stats(domain);

  /* push them into the orphan stats */
  /* This is called from the collector as well as from the mutator. */
  caml_plat_lock_blocking(&orphan_lock);
  caml_accum_alloc_stats(&orphaned_alloc_stats, &alloc_stats);
  caml_plat_unlock(&orphan_lock);
}

/* The "sampled stats" of a domain are a recent copy of its
   domain-local stats, accessed without synchronization and only
   updated ("sampled") during stop-the-world events -- each minor
   collection, major cycle (which potentially includes compaction),
   all of these events could happen during domain termination. */
static struct gc_stats* sampled_gc_stats;

void caml_init_gc_stats (uintnat max_domains)
{
  sampled_gc_stats =
    caml_stat_calloc_noexc(max_domains, sizeof(struct gc_stats));
  if (sampled_gc_stats == NULL)
    caml_fatal_error("Failed to allocate sampled_gc_stats");
}

void caml_free_gc_stats(void)
{
  if (sampled_gc_stats != NULL)
    caml_stat_free(sampled_gc_stats);
  sampled_gc_stats = NULL;
}

/* Update the sampled stats for the given domain during a STW section. */
void caml_collect_gc_stats_sample_stw(caml_domain_state* domain)
{
  struct gc_stats* stats = &sampled_gc_stats[domain->id];
  if (caml_domain_terminating(domain)) {
    /* If the domain is terminating, we should not update the sample
       with accurate domain-local data, but instead clear the sample
       so that a new domain spawning there in the future can start
       with empty stats.

       The current stats will also be 'orphaned' during domain
       termination, so they will remain accounted for in the global
       statistics. (Orphaning right now would be correct but
       insufficient as further stat updates may come after the current
       STW section.)  */
    memset(stats, 0, sizeof(*stats));
  } else {
    caml_collect_alloc_stats_sample(domain, &stats->alloc_stats);
    caml_collect_heap_stats_sample(domain->shared_heap, &stats->heap_stats);
  }
}

/* Compute global stats for the whole runtime. */
void caml_compute_gc_stats(struct gc_stats* buf)
{
  intnat pool_max = 0, large_max = 0;
  int my_id = Caml_state->id;
  memset(buf, 0, sizeof(*buf));

  caml_accum_orphan_heap_stats(&buf->heap_stats);
  caml_accum_orphan_alloc_stats(&buf->alloc_stats);

  /* The instantaneous maximum heap size cannot be computed
     from per-domain statistics, and would be very expensive
     to maintain directly. Here, we just sum the per-domain
     maxima, which is completely wrong.

     FIXME: maybe maintain coarse global maxima?

     The summation starts here from the orphan-heap maxima.
  */
  pool_max = buf->heap_stats.pool_max_words;
  large_max = buf->heap_stats.large_max_words;

  for (int i = 0; i < caml_params->max_domains; i++) {
    /* For allocation stats, we use the live stats of the current domain
       and the sampled stats of other domains.

       For the heap stats, we always used the sampled stats. */
    struct gc_stats* s = &sampled_gc_stats[i];
    if (i != my_id) {
      caml_accum_alloc_stats(&buf->alloc_stats, &s->alloc_stats);
      caml_accum_heap_stats(&buf->heap_stats, &s->heap_stats);
    }
    else {
      struct alloc_stats alloc_stats;
      caml_collect_alloc_stats_sample(Caml_state, &alloc_stats);
      caml_accum_alloc_stats(&buf->alloc_stats, &alloc_stats);
      caml_accum_heap_stats(&buf->heap_stats, &s->heap_stats);
      //FIXME use live heap stats instead of sampled heap stats below?
    }
    pool_max += s->heap_stats.pool_max_words;
    large_max += s->heap_stats.large_max_words;
  }
  buf->heap_stats.pool_max_words = pool_max;
  buf->heap_stats.large_max_words = large_max;
}
