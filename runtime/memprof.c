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

/* Open questions on the multicore memprof design:
 *
 * 1. Would it be better to adopt whole entries arrays, rather than
 * individual entries? Domains and configs would have linked lists of
 * entries arrays. This would mean that individual entries are only
 * ever moved by a particular domain, and never when a callback on
 * them is running. This would reduce possible race conditions and the
 * tricky control flow around thread suspension. A thread running a
 * callback could have a pointer to the entry (just as the entry has a
 * pointer to the thread), rather than the slightly twisty
 * flags-and-index system.
 */

/* General Design
 *
 * Data Structures
 *
 * Each block of memory tracked by memprof is represented by an
 * "entry" structure (entry_s, *entry_t). It tracks the state of the
 * block of memory, and its progress through the various callbacks.
 *
 * A resizable table of entry structures is called an "entries"
 * (entries_s, *entries_t). It uses indexes to track parts of the
 * table which may need particular actions taken.
 *
 * The memprof state of a particular systhread is a "thread state"
 * (memprof_thread_s, *memprof_thread_t). This structure exists
 * whether or not systhreads is initialized (one per domain), and
 * whether or not memprof is running. It has an entries table, for
 * blocks allocated by this thread whose allocation callback has not
 * yet completed.
 *
 * The memprof state of a domain is a "domain state"
 * (memprof_domain_s, *memprof_domain_t). This structure exists
 * whether or not memprof is running. It has an entries table, for
 * blocks allocated in this domain whose allocation callbacks have
 * completed, and for blocks allocated in this domain whose allocating
 * threads have exited before calling the allocation callbacks. It has
 * a linked list of thread states for all the threads in the domain,
 * and a pointer to the current thread state.
 *
 * Memprof in any given domain has a "configuration"
 * (memprof_config_s, *memprof_config_t). This structure is only
 * created when a profile is started, and survives until the profile
 * is discarded. It has an entries table, for blocks allocated in any
 * domain using the profile which has since terminated, or to which
 * entries are transferred when the profile is stopped. It has a
 * linked list of domain states for all the domains using this
 * profile.
 */

#define CAML_INTERNALS

#include <stdbool.h>
#include <string.h>

#include "caml/memprof.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/signals.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/shared_heap.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/frame_descriptors.h"
#include "caml/weak.h"
#include "caml/stack.h"
#include "caml/misc.h"
#include "caml/compact.h"
#include "caml/printexc.h"
#include "caml/runtime_events.h"

/* type aliases for the hierarchy of structures for managing memprof status. */

typedef struct memprof_config_s memprof_config_s, *memprof_config_t;
typedef struct memprof_domain_s memprof_domain_s, *memprof_domain_t;
typedef struct memprof_thread_s memprof_thread_s, *memprof_thread_t;

/* [Gc.Memprof.allocation_source] */

/* At present (since OCaml 5), SRC_MARSHAL can't be produced, because
 * unmarshalling uses the regular allocation functions. TODO: Work out
 * whether this type is needed at all. */

enum { SRC_NORMAL = 0, SRC_MARSHAL = 1, SRC_CUSTOM = 2 };

/* Callback indexes. Majors and minors are not distinguished here. */

#define CB_NONE          0
#define CB_ALLOC         1
#define CB_PROMOTE       2
#define CB_DEALLOC       3

/* Maximum value of a callback index */
#define CB_MAX           CB_DEALLOC

/* How many bits required for a callback index */
#define CB_BITS          2

/* the mask for a given callback index */
#define CB_MASK(cb) (1 << ((cb) - 1))

/* TODO: figure out what this Placeholder stuff is actually doing on trunk.
 *
 * During the alloc callback for a minor allocation made from C, the
 * block being sampled has not yet been allocated. Instead, we use an
 * int-tagged value in the block field, distinguished both by the int
 * tag and some magic higher bits. */

#define Placeholder_magic 0x04200000
#define Placeholder_offs(offset) (Val_long((offset & 0xFFFF) \
                                           + Placeholder_magic))
#define Offs_placeholder(block) (Long_val(block) & 0xFFFF)
#define Is_placeholder(block) \
  (Is_long(block) && (Long_val(block) & ~(uintnat)0xFFFF) == Placeholder_magic)

/* Structure for each tracked allocation. Six words. */

typedef struct {
  /* Memory block being sampled. This is a weak GC root. Note that
   * this may be a placeholder during the allocation callback. (see
   * placeholder macros above). */
  value block;

  /* The value returned by the previous callback for this block, or
   * the callstack (as a value-tagged pointer to the C heap) if the
   * alloc callback has not been called yet.  This is a strong GC
   * root. */
  value user_data;

  /* Number of samples in this block. */
  uintnat samples;

  /* The size of this block. */
  uintnat wosize;

  /* The thread currently running a callback for this entry,
     or NULL if there is none */
  memprof_thread_t running;

  /* Whether this block has been initially allocated in the minor heap. */
  unsigned int alloc_young : 1;

  /* The source of the allocation: normal allocations, marshal or custom_mem. */
  unsigned int source : 2;

  /* Whether this block has been promoted. Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Whether this block has been deallocated. */
  unsigned int deallocated : 1;

  /* Which callback (CB_*) is currently running for this entry. */
  unsigned int callback : CB_BITS;

  /* A mask of callbacks (1 << (CB_* - 1)) which have been called (not
   * necessarily completed) for this entry. */
  unsigned int callbacks : CB_MAX;

  /* Whether this entry is deleted. */
  unsigned int deleted : 1;
} entry_s, *entry_t;

/* A resizable array of entry_s entries. */

typedef struct {
  entry_t t; /* Pointer to array of entry_s structures */
  uintnat min_size, size, live;

  /* Before this position, the [block] and [user_data] fields both
   * point to the major heap ([young <= live]). */
  uintnat young;

  /* There are no blocks to be evicted before this position
   * ([evict <= live]). */
  uintnat evict;

  /* There are no pending callbacks before this position
   * ([next <= live]). */
  uintnat next;

} entries_s, *entries_t;

/* Minimum size of a per-thread entries array */
#define MIN_ENTRIES_THREAD_SIZE 16

/* Minimum size of a per-domain entries array */
#define MIN_ENTRIES_DOMAIN_SIZE 128

/* Minimum size of a per-config entries array */
#define MIN_ENTRIES_CONFIG_SIZE 128

/* number of random variables in a batch */
#define RAND_BLOCK_SIZE 64

/* Per-thread memprof state. */

struct memprof_thread_s {
  /* [suspended] is used for inhibiting memprof callbacks when
     a callback is running or when an uncaught exception handler is
     called. */
  bool suspended;

  /* The index of the tracking entry for which this thread is
   * currently in a callback */
  uintnat callback_index;

  /* Pointer to entries table for the current callback, or NULL if not
   * currently running a callback. */

  entries_t callback_table;

  /* Entries for blocks allocated in this thread whose alloc callback
   * has not yet been called. */
  entries_s entries;

  /* Per-domain memprof information */
  memprof_domain_t domain;

  /* Linked list of thread structures for this domain. Could use a
   * doubly-linked list for performance, but I haven't measured it. */
  memprof_thread_t next;
};

/* Per-domain memprof state */

struct memprof_domain_s {
  /* The owning domain */
  caml_domain_state *caml_state;

  /* Tracking entries for this domain. In the usual case these are
   * entries allocated by a thread in this domain for which the
   * allocation callback has returned: the entry is then transferred
   * to this per-domain table. However, this table will also include
   * (a) entries for threads in this domain which terminated before
   * calling the allocation callback, and (b) entries from other
   * domains which share this domains memprof config but which have
   * terminated (for example, a short-lived domain spawned by this
   * domain after memprof was started). */
  entries_s entries;

  /* Linked list of threads in this domain */
  memprof_thread_t threads;

  /* The current thread's memprof state */
  memprof_thread_t current;

  /* The current configuration for this domain. NULL when not profiling. */
  memprof_config_t config;

  /* The next domain in a linked list of domains using this
   * configuration */
  memprof_domain_t next;

  /* Buffer used to compute backtraces */
  backtrace_slot *callstack_buffer;
  size_t callstack_buffer_len;

  /* ---- random number generation state ---- */

  /* RAND_BLOCK_SIZE separate xoshiro+128 state vectors, defined in this
   * column-major order so that SIMD-aware compilers can parallelize the
   * algorithm. */
  uint32_t xoshiro_state[4][RAND_BLOCK_SIZE];

  /* Array of computed geometric random variables */

  uintnat rand_geom_buff[RAND_BLOCK_SIZE];
  uint32_t rand_pos;

  /* Surplus amount of the current sampling distance, not consumed by
   * previous allocations. Still a legitimate sample of a geometric
   * random variable. */
  uintnat next_rand_geom;

};

/* Memprof configuration. May be shared between domains. Retains a
 * table of entries, for callbacks remaining after a profile has been
 * stopped, or orphaned when a domain has been terminated. */

struct memprof_config_s {
  /* Whether memprof has been stopped. */
  bool stopped;

  /* [lambda] is the mean number of samples for each allocated word
   * (including block headers). Non-negative. Usually a very small value
   * such as 1e-4 or 1e-5. */
  double lambda;

  /* Precomputed value of [1/log(1-lambda)], for fast sampling of
   * geometric distribution. For small lambda this is like [-1/lambda].
   * Dummy if [lambda = 0]. */
  float one_log1m_lambda;

  /* [callstack_size] is the maximum number of stack frames to provide
   * to the tracker callback for each sampled event. */
  intnat callstack_size;

  /* [tracker] is a tuple of callbacks to call for various events: See
   * [Gc.Memprof.tracker]. This is a strong GC root. */
  value tracker;

  /* Linked list of domains using this configuration. */
  memprof_domain_t domains;

  /* next configuration in a linked list. */
  memprof_config_t next;

  /* Entries for blocks allocated with this configuration, after the
   * profile has been stopped, or for which the allocating domains
   * have terminated. */
  entries_s entries;
};

memprof_config_t configs = NULL;

/**** Initializing and clearing entries tables ****/

static void entries_init(entries_t entries, uintnat min_size)
{
  entries->t = NULL;
  entries->min_size = min_size;
  entries->size = 0;
  entries->live = 0;
  entries->young = 0;
  entries->evict = 0;
  entries->next = 0;
}

static void entries_clear(entries_t entries)
{
  /* maintain invariants */
  entries->size = entries->live = entries->young =
    entries->evict = entries->next = 0;
  if (entries->t) {
    caml_stat_free(entries->t);
    entries->t = NULL;
  }
}

/**** Statistical sampling ****/

/* We use a low-quality SplitMix64 PRNG to initialize state vectors
 * for a high-quality high-performance 32-bit PRNG (xoshiro128+). That
 * PRNG generates uniform random 32-bit numbers, which we use in turn
 * to generate geometric random numbers parameterized by [config->lambda].
 * This is all coded in such a way that compilers can readily use SIMD
 * optimisations.
 */

/* splitmix64 PRNG, used to initialize the xoshiro+128 state
 * vectors. Closely based on the public-domain implementation
 * by Sebastiano Vigna https://xorshift.di.unimi.it/splitmix64.c */

Caml_inline uint64_t splitmix64_next(uint64_t* x)
{
  uint64_t z = (*x += 0x9E3779B97F4A7C15ull);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
  return z ^ (z >> 31);
}

/* Initialize all the xoshiro+128 state vectors. */

static void xoshiro_init(memprof_domain_t domain)
{
  int i;
  uint64_t splitmix64_state = 42;
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    uint64_t t = splitmix64_next(&splitmix64_state);
    domain->xoshiro_state[0][i] = t & 0xFFFFFFFF;
    domain->xoshiro_state[1][i] = t >> 32;
    t = splitmix64_next(&splitmix64_state);
    domain->xoshiro_state[2][i] = t & 0xFFFFFFFF;
    domain->xoshiro_state[3][i] = t >> 32;
  }
}

/* xoshiro128+ PRNG. See Blackman & Vigna; "Scrambled linear
 * pseudorandom number generators"; ACM Trans. Math. Softw., 47:1-32,
 * 2021:
 * "xoshiro128+ is our choice for 32-bit floating-point generation." */

Caml_inline uint32_t xoshiro_next(memprof_domain_t domain, int i)
{
  uint32_t res = domain->xoshiro_state[0][i] + domain->xoshiro_state[3][i];
  uint32_t t = domain->xoshiro_state[1][i] << 9;
  domain->xoshiro_state[2][i] ^= domain->xoshiro_state[0][i];
  domain->xoshiro_state[3][i] ^= domain->xoshiro_state[1][i];
  domain->xoshiro_state[1][i] ^= domain->xoshiro_state[2][i];
  domain->xoshiro_state[0][i] ^= domain->xoshiro_state[3][i];
  domain->xoshiro_state[2][i] ^= t;
  t = domain->xoshiro_state[3][i];
  domain->xoshiro_state[3][i] = (t << 11) | (t >> 21);
  return res;
}

/* Computes [log((y+0.5)/2^32)], up to a relatively good precision,
 * and guarantee that the result is negative, in such a way that SIMD
 * can parallelize it. The average absolute error is very close to
 * 0. */

Caml_inline float log_approx(uint32_t y)
{
  /* Use a type pun to break y+0.5 into biased exponent (in the range
   * 126-159) and mantissa (a float in [1,2)).
   */

  union { float f; int32_t i; } u;

  /* This may discard up to eight low bits of y. The sign bit of u.f
   * (and u.i) is always clear, as y is non-negative. The other bits
   * of y (disregarding the leading 1) end up in the mantissa */

  u.f = y + 0.5f;

  /* exp is the biased exponent, as a float. Given that u.f ranges
   * between 0.5 and 2^32, and the bias of 127, exp is in [126, 159].
   */


  float exp = u.i >> 23;

  /* Set the biased exponent to 127, i.e. exponent of zero, and obtain
   * the resulting float, the mantissa, as x. */

  u.i = (u.i & 0x7FFFFF) | 0x3F800000;

  float x = u.f;

  /* y+0.5 = x * 2^(exp-127), so if f(x) ~= log(x) - 159*log(2), then
   * log((y+0.5)/2^32) ~= f(x) + exp * log(2). We choose the unique
   * degree-3 polynomial f such that :

       - Its average value is that of the desired function  in [1, 2]
             (so the sampling has the right mean when lambda is small).
       - f(1) = f(2) - log(2), so that it is continuous at the exponent steps.
       - f(1) = -1e-5, so the function is everywhere negative.
       - The maximum absolute error is minimized in [1, 2].

    The actual maximum absolute error is around 7e-4. Computed with sollya.
    */

  return (-111.70172433407f +
          x * (2.104659476859f +
               x * (-0.720478916626f +
                    x * 0.107132064797f)) +
          0.6931471805f*exp);
}

/* This function regenerates [RAND_BLOCK_SIZE] geometric random
 * variables at once. Doing this by batches help us gain performances:
 * many compilers (e.g., GCC, CLang, ICC) will be able to use SIMD
 * instructions to get a performance boost.
 */
#ifdef SUPPORTS_TREE_VECTORIZE
__attribute__((optimize("tree-vectorize")))
#endif

static void rand_batch(memprof_domain_t domain)
{
  int i;
  float one_log1m_lambda = domain->config->one_log1m_lambda;

  /* Instead of using temporary buffers, we could use one big loop,
     but it turns out SIMD optimizations of compilers are more fragile
     when using larger loops.  */
  uint32_t A[RAND_BLOCK_SIZE];
  float B[RAND_BLOCK_SIZE];

  /* Generate uniform variables in A using the xoshiro128+ PRNG. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    A[i] = xoshiro_next(domain, i);

  /* Generate exponential random variables by computing logarithms. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    B[i] = 1 + log_approx(A[i]) * one_log1m_lambda;

  /* We do the final flooring for generating geometric
     variables. Compilers are unlikely to use SIMD instructions for
     this loop, because it involves a conditional and variables of
     different sizes (32 and 64 bits). */
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    double f = B[i];
    CAMLassert (f >= 1);
    /* [Max_long+1] is a power of two => no rounding in the test. */
    if (f >= Max_long+1)
      domain->rand_geom_buff[i] = Max_long;
    else domain->rand_geom_buff[i] = (uintnat)f;
  }

  domain->rand_pos = 0;
}

/* Simulate a geometric random variable of parameter [lambda].
 * The result is clipped in [1..Max_long] */
static uintnat rand_geom(memprof_domain_t domain)
{
  uintnat res;
  CAMLassert(domain->config->lambda > 0.);
  if (domain->rand_pos == RAND_BLOCK_SIZE) rand_batch(domain);
  res = domain->rand_geom_buff[domain->rand_pos++];
  CAMLassert(1 <= res && res <= Max_long);
  return res;
}

/* Simulate a binomial random variable of parameters [len] and
 * [lambda]. This tells us how many times a single block allocation is
 * sampled.  This sampling algorithm has running time linear with [len
 * * lambda].  We could use a more involved algorithm, but this should
 * be good enough since, in the typical use case, [lambda] << 0.01 and
 * therefore the generation of the binomial variable is amortized by
 * the initialialization of the corresponding block.
 *
 * If needed, we could use algorithm BTRS from the paper:
 *   Hormann, Wolfgang. "The generation of binomial random variates."
 *   Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.
 */
static uintnat rand_binom(memprof_domain_t domain, uintnat len)
{
  uintnat res;
  CAMLassert(domain->config->lambda > 0. && len < Max_long);
  for (res = 0; domain->next_rand_geom < len; res++)
    domain->next_rand_geom += rand_geom(domain);
  domain->next_rand_geom -= len;
  return res;
}

/**** Managing data structures for tracked blocks. ****/

/* Reallocate the [es] entry array if it is either too small or too
   large.
   [grow] is the number of free cells needed.
   Returns true if reallocation succeeded --[es->size] is at
   least [es->live+grow]--, and false otherwise. */
static bool ensure_entries(entries_t es, uintnat grow)
{
  uintnat new_size, new_live = es->live + grow;
  entry_t new_t;
  if (new_live <= es->size &&
     (4*new_live >= es->size || es->size == es->min_size)) {
    /* No need to grow or shrink */
    return true;
  }
  new_size = new_live * 2;
  if (new_size < es->min_size)
    new_size = es->min_size;
  new_t = caml_stat_resize_noexc(es->t, new_size * sizeof(entry_s));
  if (new_t == NULL) return false;
  es->t = new_t;
  es->size = new_size;
  return true;
}

#define Invalid_index (~(uintnat)0)

/* Create and initialize a new entry in an entries table, and return
 * its index (or Invalid_index if allocation fails). */

Caml_inline uintnat new_tracked(entries_t es,
                                uintnat samples, uintnat wosize,
                                int source, int is_young,
                                value block, value user_data)
{
  if (!ensure_entries(es, 1))
    return Invalid_index;
  uintnat i = es->live ++;
  entry_t e = es->t + i;
  e->block = block;
  e->user_data = user_data;
  e->samples = samples;
  e->wosize = wosize;
  e->running = NULL;
  e->alloc_young = is_young;
  e->source = source;
  e->promoted = 0;
  e->deallocated = 0;
  e->callback = CB_NONE;
  e->callbacks = 0;
  e->deleted = 0;
  return i;
}

/* Mark a given entry in an entries table as "deleted" */

static void mark_deleted(entries_t es, uintnat i)
{
  entry_t e = &es->t[i];
  e->deleted = 1;
  e->user_data = Val_unit;
  e->block = Val_unit;
  if (i < es->evict) es->evict = i;
}

/* Transfer all entries from one entries table to another. Return the
 * previous size of the destination table (which is now the offset of
 * the transferred entries). */

static void entries_transfer(entries_t from, entries_t to)
{
  uintnat offset = to->live;

  if (from->live == 0) {
    return;
  }
  ensure_entries(to, from->live);
  for (uintnat j = 0; j < from->live; ++j) {
    to->t[offset + j] = from->t[j];
    if (from->t[j].running) { /* TODO: ATOMIC */
      from->t[j].running->callback_table = to;
      from->t[j].running->callback_index = offset+j;
    }
  }
  if (to->young == offset) {
    to->young = offset + from->young;
  }
  if (to->evict == offset) {
    to->evict = offset + from->evict;
  }
  if (to->next == offset) {
    to->next = offset + from->next;
  }
  ensure_entries(from, -from->live);
  from->young = from->evict = from->next = from->live = 0;
}

/* Remove any deleted entries from [es], updating [es->young] and
   [es->next] if necessary */
static void evict_deleted(entries_t es)
{
  uintnat i, j;

  /* Two-finger algorithm */
  j = i = es->evict;
  while (i < es->live) {
    if (!es->t[i].deleted) { /* preserve this entry */
      if (i != j) {
        es->t[j] = es->t[i];
        /* if a callback is currently being run on this entry,
         * make sure its index is updated */
        /* TODO: ATOMIC */
        memprof_thread_t runner = es->t[i].running;
        if (runner != NULL) {
          CAMLassert(runner->callback_table == es);
          CAMLassert(runner->callback_index == i);
          runner->callback_index = j;
        }
      }
      ++ j;
    }
    ++ i;
    if (es->young == i) es->young = j;
    if (es->next == i) es->next = j;
  }
  es->evict = es->live = j;
  CAMLassert(es->next <= es->live);
  CAMLassert(es->young <= es->live);
  ensure_entries(es, 0);
}

/**** Create and destroy thread state structures ****/

static memprof_thread_t thread_create(memprof_domain_t domain)
{
  memprof_thread_t thread = caml_stat_alloc(sizeof(memprof_thread_s));
  if (!thread) {
    return NULL;
  }
  thread->suspended = false;
  thread->callback_index = 0;
  thread->callback_table = NULL;
  entries_init(&thread->entries, MIN_ENTRIES_THREAD_SIZE);

  /* attach to domain record */
  thread->domain = domain;
  thread->next = domain->threads;
  domain->threads = thread;

  return thread;
}

static void thread_destroy(memprof_thread_t thread)
{
  memprof_domain_t domain = thread->domain;

  /* If the thread is running a callback, delete that callback entry. */
  if (thread->callback_table) {
    mark_deleted(thread->callback_table, thread->callback_index);
  }

  if (domain->current == thread) {
    domain->current = NULL;
  }
  /* remove thread from the per-domain list. Could go faster if we
   * used a doubly-linked list, but that's premature optimisation
   * at this point. */
  memprof_thread_t *p = &domain->threads;
  while (*p != thread) {
    p = &(*p)->next;
  }

  *p = thread->next;

  /* transfer any surviving entries to the domain's table */
  entries_transfer(&thread->entries, &domain->entries);
  entries_clear(&thread->entries);
  caml_stat_free(thread);
}

/**** Create and destroy domain state structures ****/

static void domain_destroy(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->threads;
  while (thread) {
    memprof_thread_t next = thread->next;
    thread_destroy(thread);
    thread = next;
  }

  memprof_config_t config = domain->config;
  if (config) {
    /* remove domain from the per-config list. Could go faster if we
     * used a doubly-linked list, but that's premature optimisation
     * at this point. */
    memprof_domain_t *p = &config->domains;
    while (*p != domain) {
      p = &(*p)->next;
    }
    *p = domain->next;

    /* transfer any surviving entries to the config's table */
    entries_transfer(&domain->entries, &config->entries);
  }
  entries_clear(&domain->entries);
  caml_stat_free(domain);
}

static memprof_domain_t domain_create(caml_domain_state *caml_state)
{
  memprof_domain_t domain = caml_stat_alloc(sizeof(memprof_domain_s));
  if (!domain) {
    return NULL;
  }

  domain->caml_state = caml_state;
  entries_init(&domain->entries, MIN_ENTRIES_DOMAIN_SIZE);
  domain->threads = NULL;
  domain->current = NULL;
  domain->config = NULL;
  domain->callstack_buffer = NULL;
  domain->callstack_buffer_len = 0;

  /* Don't initialize random number generators etc until we need them
   * - see domain_prepare_to_sample() */

  /* create initial thread for domain */
  memprof_thread_t thread = thread_create(domain);
  if (thread) {
    domain->current = thread;
  } else {
    domain_destroy(domain);
    domain = NULL;
  }
  return domain;
}

/* Initialize any per-domain state required to actually perform
 * sampling, as opposed to our internal data-structure management.
 */

static void domain_prepare_to_sample(memprof_domain_t domain)
{
  xoshiro_init(domain);
  domain->rand_pos = RAND_BLOCK_SIZE;
  if (domain->config->lambda > 0) {
    /* next_rand_geom can be zero if the next word is to be sampled,
     * but rand_geom always returns a value >= 1. Subtract 1 to correct. */
    domain->next_rand_geom = rand_geom(domain) - 1;
  }

  caml_memprof_renew_minor_sample(domain->caml_state);
}

/*** Create and destroy configuration records ***/

static memprof_config_t config_create(double lambda, intnat callstack_size,
                                      value tracker, memprof_domain_t domain)
{
  memprof_config_t config = caml_stat_alloc(sizeof(memprof_config_s));
  if (!config) {
    return NULL;
  }
  config->stopped = false;
  config->lambda = lambda;
  if (lambda > 0) {
    config->one_log1m_lambda = lambda == 1 ? 0 : 1/caml_log1p(-lambda);
  }
  config->callstack_size = callstack_size;
  config->tracker = tracker;
  config->domains = domain;
  entries_init(&config->entries, MIN_ENTRIES_CONFIG_SIZE);

  domain->config = config;
  domain->next = NULL;

  /* MAKE ATOMIC */
  config->next = configs;
  configs = config;
  return config;
}

static void config_destroy(memprof_config_t config)
{
  memprof_domain_t domain = config->domains;
  (void)domain;
  CAMLassert(!domain);
  /* remove from linked list of configs: MAKE ATOMIC */
  memprof_config_t *p = &configs;
  while (*p != config) {
    p = &(*p)->next;
  }
  *p = config->next;
  entries_clear(&config->entries);

  caml_stat_free(config);
}

/**** Interface to domain module ***/

void caml_memprof_new_domain(caml_domain_state *parent,
                             caml_domain_state *child)
{
  memprof_domain_t domain = domain_create(child);

  child->memprof = domain;
  /* if parent domain is profiling, child domain should also be profiling */
  if (domain && parent && parent->memprof->config) {
    memprof_config_t config = parent->memprof->config;
    /* MAKE ATOMIC */
    domain->config = config;
    domain->next = config->domains;
    config->domains = domain;

    domain_prepare_to_sample(domain);
  }
}

void caml_memprof_delete_domain(caml_domain_state *domain)
{
  if (!domain->memprof) {
    return;
  }
  domain_destroy(domain->memprof);
  domain->memprof = NULL;
}

/**** Interface with domain action-pending flag ****/

/* If profiling is active in the current domain, and we may have some
 * callbacks pending, set the action pending flag. */

static void set_action_pending_as_needed(memprof_domain_t domain)
{
  if (domain->current->suspended) return;
  if (domain->entries.next < domain->entries.live ||
      domain->current->entries.live > 0)
    caml_set_action_pending(domain->caml_state);
}

/* Set the suspended flag on `domain` to `s`. */

static void update_suspended(memprof_domain_t domain, bool s)
{
  domain->current->suspended = s;
  caml_memprof_renew_minor_sample(domain->caml_state);
  if (!s) set_action_pending_as_needed(domain);
}

/* Set the suspended flag on the current domain to `s`. */

void caml_memprof_update_suspended(bool s) {
  update_suspended(Caml_state->memprof, s);
}

/* Shifts the next sample in the minor heap by [n] words. Essentially,
   this tells the sampler to ignore the next [n] words of the minor
   heap. */
static void shift_sample(caml_domain_state *state, uintnat n)
{
  if (state->memprof_young_trigger - state->young_start > n)
    state->memprof_young_trigger -= n;
  else
    state->memprof_young_trigger = state->young_start;
  caml_reset_young_limit(state);
}

/**** Interface with systhread. ****/

CAMLexport memprof_thread_t caml_memprof_new_thread(caml_domain_state *domain)
{
  CAMLassert(domain->memprof);
  return thread_create(domain->memprof);
}

CAMLexport memprof_thread_t caml_memprof_main_thread(caml_domain_state *domain)
{
  memprof_domain_t memprof_domain = domain->memprof;
  CAMLassert(memprof_domain);
  memprof_thread_t thread = memprof_domain->threads;

  /* There should currently be just one thread in this domain */
  CAMLassert(thread);
  CAMLassert(thread->next = NULL);
  return thread;
}

CAMLexport void caml_memprof_delete_thread(memprof_thread_t thread)
{
  thread_destroy(thread);
}

CAMLexport void caml_memprof_leave_thread(void)
{
  if (Caml_state->memprof) {
    Caml_state->memprof->current = NULL;
  }
}

CAMLexport void caml_memprof_enter_thread(memprof_thread_t thread)
{
  CAMLassert(thread->domain->current == NULL);
  thread->domain->current = thread;
  update_suspended(thread->domain, thread->suspended);
}

/**** Interface to OCaml ****/

CAMLprim value caml_memprof_start(value lv, value szv, value tracker_param)
{
  CAMLparam3(lv, szv, tracker_param);

  CAML_STATIC_ASSERT(CB_MAX < (1 << CB_BITS));

  double l = Double_val(lv);
  intnat sz = Long_val(szv);

  if (sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("Gc.Memprof.start");

  memprof_domain_t domain = Caml_state->memprof;
  if (domain->config) {
    caml_failwith("Gc.Memprof.start: already started.");
  }

  memprof_config_t config = config_create(l, sz, tracker_param, domain);
  if (!config) {
    caml_failwith("Gc.Memprof.start: couldn't allocate configuration object.");
  }

  domain_prepare_to_sample(domain);

  CAMLreturn(Val_ptr(config));
}

CAMLprim value caml_memprof_stop(value unit)
{
  memprof_domain_t domain = Caml_state->memprof;
  memprof_config_t config = domain->config;
  if (!config) {
    caml_failwith("Gc.Memprof.stop: not started.");
  }

  config->stopped = true;

  /* ATOMIC: Possibly we need to do a minor GC and then do this next
   * part with the world stopped, as we need there to be no pointers
   * to the young heap from the config entries table? */

  /* stop memprof for all domains using this config */
  memprof_domain_t d = config->domains;
  while (d) {
    CAMLassert(d->config == config);
    /* Turn off profiling in this domain */
    d->config = NULL; /* MAKE ATOMIC */

    /* Transfer all entries from the domain and threads to the config. */
    entries_transfer(&d->entries, &config->entries);
    memprof_thread_t thread = d->threads;
    while (thread) {
      entries_transfer(&thread->entries, &config->entries);
      thread = thread->next;
    }
    /* "renew_minor_sample" notices that we're not sampling and
     * resets the trigger. */
    caml_memprof_renew_minor_sample(d->caml_state);
    caml_stat_free(d->callstack_buffer);
    d->callstack_buffer = NULL;
    d->callstack_buffer_len = 0;
    d = d->next;
  }
  return Val_unit;
}

CAMLprim value caml_memprof_discard(value v)
{
  memprof_config_t config = Ptr_val(v);
  if (!config) {
    caml_invalid_argument("Gc.Memprof.discard");
  }
  if (!config->stopped) {
    caml_failwith("Gc.Memprof.discard: profile has not been stopped.");
  }
  CAMLassert(!config->domains);

  /* tell any thread still running a callback for this profile that it
   * has been discarded. This can happen, for example, if a callback
   * calls a thread-suspension function such as Thread.delay 1000000.0. */

  for (uintnat i = 0; i < config->entries.live; ++i) {
    entry_t e = &config->entries.t[i];
    if (e->running) {
      memprof_thread_t thread = e->running;
      thread->callback_table = NULL;
    }
  }
  config_destroy(config);
  return Val_unit;
}

/**** Capturing the call stack *****/

typedef struct {
        size_t frames;
        value stack[];
} callstack_stash_s, *callstack_stash_t;

/* This function may be called in a context where the heap is in an
   invalid state, or when the roots are not properly registered
   (basically: when we are sampling a block allocated on the major
   heap by C code). Therefore, we capture the callstack onto the C
   heap, but do not make a copy in the Caml heap until calling the
   allocation callback. */
static value capture_callstack_postponed(memprof_domain_t domain)
{
  value res = Atom(0); /* empty array. */
  size_t frames =
    caml_get_callstack(domain->config->callstack_size,
                       &domain->callstack_buffer,
                       &domain->callstack_buffer_len, -1);
  if (frames) {
    callstack_stash_t stash = caml_stat_alloc_noexc(sizeof(callstack_stash_s)
                                                    + frames * sizeof(value));
    if (stash) {
      stash->frames = frames;
      for (size_t i = 0; i < frames; ++i) {
        stash->stack[i] = Val_backtrace_slot(domain->callstack_buffer[i]);
      }
      res = Val_ptr(stash);
    }
  }
  return res;
}

/* In this version, we are allowed to call the GC, so we use
   [caml_alloc], which is more efficient since it uses the minor
   heap.
   Should be called with [domain->current->suspended] set */
static value capture_callstack(memprof_domain_t domain, int alloc_idx)
{
  CAMLassert(domain->current->suspended);

  size_t frames =
    caml_get_callstack(domain->config->callstack_size,
                       &domain->callstack_buffer,
                       &domain->callstack_buffer_len,
                       alloc_idx);
  value res = caml_alloc(frames, 0);
  for (size_t i = 0; i < frames; ++i) {
    Store_field(res, i, Val_backtrace_slot(domain->callstack_buffer[i]));
  }

  /* discard the buffer if it was large and we only used a small fraction */
  if (domain->callstack_buffer_len > 256 &&
      domain->callstack_buffer_len > frames * 8) {
    caml_stat_free(domain->callstack_buffer);
    domain->callstack_buffer = NULL;
    domain->callstack_buffer_len = 0;
  }
  return res;
}

/**** Running callbacks ****/

/* Run a single callback, in thread `thread`, with configuration
 * `config`, for entry number `i` in table `es`. The callback closure
 * is `cb`, the parameter is `param`, and the "callback index" is
 * `cb_index`. */

Caml_inline value run_callback_exn(memprof_thread_t thread,
                                   memprof_config_t config,
                                   entries_t es, uintnat i,
                                   uintnat cb_index,
                                   value cb, value param)
{
  entry_t e = &es->t[i];
  value res;
  CAMLassert(e->running == NULL);

  thread->callback_table = es;
  thread->callback_index = i;
  e->running = thread;

  e->callback = cb_index;
  e->callbacks |= CB_MASK(cb_index);
  e->user_data = Val_unit;      /* Release root. */

  res = caml_callback_exn(cb, param);

  if (thread->callback_table == NULL) {
    /* caml_memprof_discard was called during the callback; there are
     * no entries to update etc. */
    return Is_exception_result(res) ? res : Val_unit;
  }
  /* The tracked entry can move during the callback (e.g. by another thread
   * evicting deleted entries). If so, thread->callback_index is updated. */
  /* TODO: ATOMIC */
  i = thread->callback_index;
  es = thread->callback_table;
  e = &es->t[i];

  thread->callback_table = NULL;
  CAMLassert(e->running == thread);
  e->running = NULL;
  e->callback = CB_NONE;
  if (Is_exception_result(res) || res == Val_unit) {
    /* Callback raised an exception or returned None or (), discard
       this entry. */
    mark_deleted(es, i);
    return res;
  } else {
    /* Callback returned [Some _]. Store the value in [user_data]. */
    CAMLassert(!Is_exception_result(res) && Is_block(res) && Tag_val(res) == 0
               && Wosize_val(res) == 1);
    e->user_data = Field(res, 0);
    if (Is_block(e->user_data) && Is_young(e->user_data) &&
        i < es->young)
      es->young = i;

    // If we get this far we have not just run a dealloc callback,
    // (because they return unit) so there may be more callbacks to
    // run on this entry.
    if (i < es->next && e->deallocated)
      es->next = i;

    return Val_unit;
  }
}

/* accessors for the OCaml type [Gc.Memprof.tracker] */

#define Alloc_minor(config) (Field(config->tracker, 0))
#define Alloc_major(config) (Field(config->tracker, 1))
#define Promote(config) (Field(config->tracker, 2))
#define Dealloc_minor(config) (Field(config->tracker, 3))
#define Dealloc_major(config) (Field(config->tracker, 4))

/* Run the allocation callback for a given entry of an entries array.
   This assumes that the corresponding [deleted] and
   [running] fields of the entry are both clear.
   Returns:
   - An exception result if the callback raised an exception
   - Val_long(0) == Val_unit == None otherwise
 */
static value run_alloc_callback_exn(memprof_thread_t thread,
                                    memprof_config_t config,
                                    entries_t es, uintnat i)
{
  entry_t e = &es->t[i];
  value sample_info;

  CAMLassert(Is_block(e->block) || Is_placeholder(e->block) || e->deallocated);

  /* Allocate callstack on Caml heap if we couldn't do so at the
   * allocation point; see capture_callstack_postponed(). */
  if (Is_long(e->user_data)) {
    callstack_stash_t stash = Ptr_val(e->user_data);
    e->user_data = caml_alloc(stash->frames, 0);
    for (size_t i = 0; i < stash->frames; ++i) {
      Store_field(e->user_data, i, stash->stack[i]);
    }
    caml_stat_free(stash);
  }

  sample_info = caml_alloc_small(4, 0);
  Field(sample_info, 0) = Val_long(e->samples);
  Field(sample_info, 1) = Val_long(e->wosize);
  Field(sample_info, 2) = Val_long(e->source);
  Field(sample_info, 3) = e->user_data; /* callstack */
  value callback = e->alloc_young ? Alloc_minor(config) : Alloc_major(config);
  return run_callback_exn(thread, config, es, i,
                          CB_ALLOC, callback, sample_info);
}

/* Run any pending callbacks from entries table `es` in thread
 * `thread` with config `config`. */

static value entries_run_callbacks_exn(memprof_thread_t thread,
                                       memprof_config_t config,
                                       entries_t es)
{
  value res = Val_unit;
  /* Note: several callbacks may be called for a single entry */
  while (es->next < es->live) {
    entry_t e = &es->t[es->next];

    if (e->deleted || e->running) {
      /* This entry is already deleted, or is running a callback. Ignore it. */
      ++ es->next;
    } else if (!(e->callbacks & CB_MASK(CB_ALLOC))) {
      /* allocation callback hasn't been run */
      res = run_alloc_callback_exn(thread, config, es, es->next);
      if (Is_exception_result(res)) goto end;
    } else if (e->promoted && !(e->callbacks & CB_MASK(CB_PROMOTE))) {
      /* promoted entry; call promote callback */
      res = run_callback_exn(thread, config, es, es->next,
                             CB_PROMOTE, Promote(config), e->user_data);
      if (Is_exception_result(res)) goto end;
    } else if (e->deallocated && !(e->callbacks & CB_MASK(CB_DEALLOC))) {
      /* deallocated entry; call dealloc callback */
      value cb = (e->promoted || !e->alloc_young) ?
        Dealloc_major(config) : Dealloc_minor(config);
      res = run_callback_exn(thread, config, es, es->next,
                             CB_DEALLOC, cb, e->user_data);
      if (Is_exception_result(res)) goto end;
    } else {
      /* There is nothing to do with this entry. */
      ++ es->next;
    }
  }
end:
  evict_deleted(es);
  return res;
}

/* Run all pending callbacks for the current thread and domain, and
 * any orphaned callbacks from terminated domains. */

value caml_memprof_run_callbacks_exn(void)
{
  memprof_domain_t domain = Caml_state->memprof;
  memprof_thread_t thread = domain->current;
  if (thread->suspended) return Val_unit;

  update_suspended(domain, true);

  /* run per-thread callbacks */
  value res = entries_run_callbacks_exn(thread, domain->config,
                                        &thread->entries);
  if (Is_exception_result(res)) goto end;
  /* move entries from allocating thread to owning domain, so their
   * subsequent callbacks may be run by any thread in the domain. */
  entries_transfer(&thread->entries, &domain->entries);

  /* run per-domain callbacks */
  res = entries_run_callbacks_exn(thread, domain->config, &domain->entries);
  if (Is_exception_result(res)) goto end;

  /* run orphaned per-config callbacks */
  memprof_config_t config = configs;
  /* TODO: ATOMIC */
  while(config) {
    res = entries_run_callbacks_exn(thread, config, &config->entries);
    if (Is_exception_result(res)) goto end;
    config = config->next;
  }

end:
  update_suspended(domain, false);
  return res;
}

/**** Sampling procedures ****/

Caml_inline bool running(memprof_domain_t domain)
{
  memprof_config_t config = domain->config;
  memprof_thread_t thread = domain->current;
  return (config
          && !config->stopped
          && config->lambda > 0
          && !thread->suspended);
}

/* Respond to the allocation of new block [block], size [wosize], with
 * [samples] samples. [src] is one of the [SRC_] enum values
 * ([Gc.Memprof.allocation_source]).
 */

static void maybe_track_block(memprof_domain_t domain,
                              value block, uintnat samples,
                              uintnat wosize, int src)
{
  if (samples == 0) return;

  value callstack = capture_callstack_postponed(domain);
  new_tracked(&domain->current->entries, samples, wosize, src,
              Is_young(block), block, callstack);
  set_action_pending_as_needed(domain);
}

void caml_memprof_track_alloc_shr(value block)
{
  memprof_domain_t domain = Caml_state->memprof;
  if (!running(domain))
    return;

  maybe_track_block(domain, block, rand_binom(domain, Whsize_val(block)),
                    Wosize_val(block), SRC_NORMAL);
}

void caml_memprof_track_custom(value block, mlsize_t bytes)
{
  memprof_domain_t domain = Caml_state->memprof;
  if (!running(domain))
    return;

  maybe_track_block(domain, block, rand_binom(domain, Wsize_bsize(bytes)),
                    Wsize_bsize(bytes), SRC_CUSTOM);
}

/* Called when exceeding the threshold for the next sample in the
   minor heap. */
void caml_memprof_track_young(uintnat wosize, int from_caml,
                              int nallocs, unsigned char* encoded_alloc_lens)
{
  memprof_domain_t domain = Caml_state->memprof;
  memprof_config_t config = domain->config;
  memprof_thread_t thread = domain->current;
  uintnat whsize = Whsize_wosize(wosize);
  value callstack, res = Val_unit;
  int alloc_idx = 0, i, allocs_sampled = 0;
  intnat alloc_ofs, trigger_ofs;
  double saved_lambda = config->lambda;

  /* If this condition is false, then [memprof_young_trigger]
     should be equal to [young_alloc_start]. But this function
     is only called with
     [young_alloc_start <= young_ptr < memprof_young_trigger],
     which is contradictory. */
  CAMLassert(running(domain));

  if (!from_caml) {
    unsigned samples = 1 +
      rand_binom(domain,
                 Caml_state->memprof_young_trigger - 1 - Caml_state->young_ptr);
    CAMLassert(encoded_alloc_lens == NULL);    /* No Comballoc in C! */
    caml_memprof_renew_minor_sample(Caml_state);
    maybe_track_block(domain, Val_hp(Caml_state->young_ptr), samples,
                      wosize, SRC_NORMAL);
    return;
  }

  /* We need to call the callbacks for this sampled block. Since each
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [Caml_state->memprof_young_trigger]. Instead,
     we remember that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  CAMLassert(Caml_state->young_ptr < Caml_state->memprof_young_trigger &&
             Caml_state->memprof_young_trigger <=
               Caml_state->young_ptr + whsize);
  trigger_ofs = Caml_state->memprof_young_trigger - Caml_state->young_ptr;
  alloc_ofs = whsize;

  /* Restore the minor heap in a valid state for calling the callbacks.
     We should not call the GC before these two instructions. */
  Caml_state->young_ptr += whsize;
  update_suspended(domain, true); // updates the memprof trigger

  /* Perform the sampling of the block in the set of Comballoc'd
     blocks, insert them in the entries array, and run the
     callbacks. */
  for (alloc_idx = nallocs - 1; alloc_idx >= 0; alloc_idx--) {
    unsigned alloc_wosz = encoded_alloc_lens == NULL ? wosize :
      Wosize_encoded_alloc_len(encoded_alloc_lens[alloc_idx]);
    /* How many times is this single block sampled? */
    unsigned samples = 0;
    alloc_ofs -= Whsize_wosize(alloc_wosz);
    while (alloc_ofs < trigger_ofs) {
      samples++;
      trigger_ofs -= rand_geom(domain);
    }

    if (samples > 0) {
      uintnat t_idx;

      callstack = capture_callstack(domain, alloc_idx);
      t_idx = new_tracked(&thread->entries, samples, alloc_wosz, SRC_NORMAL, 1,
                          Placeholder_offs(alloc_ofs), callstack);
      if (t_idx == Invalid_index) continue;
      res = run_alloc_callback_exn(thread, config, &thread->entries, t_idx);

      /* Has [caml_memprof_stop] been called during the callback? */
      bool stopped = config->stopped;
      if (stopped) {
        allocs_sampled = 0;
        config = domain->config;
        double new_lambda = config ? config->lambda : 0.0;
        if (saved_lambda != new_lambda) {
          /* [lambda] changed during the callback. We need to refresh
             [trigger_ofs]. */
          saved_lambda = new_lambda;
          trigger_ofs = new_lambda == 0. ? 0 :
                           alloc_ofs - (rand_geom(domain) - 1);
        }
      }
      if (Is_exception_result(res)) break;
      if (!stopped) allocs_sampled++;
    }
  }

  CAMLassert(alloc_ofs == 0 || Is_exception_result(res));
  CAMLassert(allocs_sampled <= nallocs);

  if (!Is_exception_result(res)) {
    /* The callbacks did not raise. The allocation will take place.
       We now restore the minor heap. */
    if (Caml_state->young_ptr - whsize < Caml_state->young_trigger) {
      CAML_EV_COUNTER(EV_C_FORCE_MINOR_MEMPROF, 1);
      /* TODO: is this actually the correct way to force a minor GC? */
      caml_poll_gc_work();
    }

    /* Re-allocate the blocks in the minor heap. We should not call the
       GC after this. */
    Caml_state->young_ptr -= whsize;

    /* Make sure this block is not going to be sampled again. */
    shift_sample(Caml_state, whsize);
  }

  /* Since [thread->entries] is local to the current thread, we know
     for sure that the allocated entries are the [alloc_sampled] last
     entries of [thread->entries]. Transfer them all to the per-domain
     entries table. */

  for (i = 0; i < allocs_sampled; i++) {
    uintnat idx = thread->entries.live-allocs_sampled+i;
    if (thread->entries.t[idx].deleted) continue;
    if (ensure_entries(&domain->entries, 1)) {
      /* Transfer the entry to the global array. */
      entry_t e = &domain->entries.t[domain->entries.live];
      domain->entries.live++;
      *e = thread->entries.t[idx];

      if (Is_exception_result(res)) {
        /* The allocations are cancelled because of an exception,
           but this callback has already been called. We simulate a
           deallocation. */
        e->block = Val_unit;
        e->deallocated = 1;
      } else {
        /* If all the allocation callbacks have succeeded, we start
           the tracking of this block.

           Subtlety: we are actually writing [t->block] with a pointer
           to an invalid (uninitialized) block. This is correct
           because the allocation and initialization happens right
           after returning from [caml_memprof_track_young]. */
        e->block = Val_hp(Caml_state->young_ptr + Offs_placeholder(e->block));

        /* Avoid setting the action pending flag in the common case. */
        if (domain->entries.next == domain->entries.live - 1)
          domain->entries.next = domain->entries.live;
      }
    }
    mark_deleted(&thread->entries, idx);
  }

  evict_deleted(&thread->entries);
  /* We need to reset the suspended flag *after* flushing
     [thread->entries], to make sure the flag is not set back to 1. */
  update_suspended(domain, false);

  if (Is_exception_result(res))
    caml_raise(Extract_exception(res));

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  return;
}

/* Renew the next sample in the minor heap. This needs to be called
 * after each minor sample and after each minor collection. In
 * practice, this is called at each minor sample, at each minor
 * collection, and when sampling is suspended and unsuspended. Extra
 * calls do not change the statistical properties of the sampling
 * because of the memorylessness of the geometric distribution. */

void caml_memprof_renew_minor_sample(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  value *trigger = state->young_start;
  if (running(domain)) {
    uintnat geom = rand_geom(domain);
    if (state->young_ptr - state->young_start > geom) {
      trigger = state->young_ptr - (geom - 1);
    }
  }

  state->memprof_young_trigger = trigger;
  caml_reset_young_limit(state);
}

/**** Handling weak and strong roots for the GC. ****/

/* Type of a function to apply to a single entry */

typedef void (*entry_action)(entry_t, void *);

/* Type of a function to apply to an entries array after iterating
 * over the entries. The second argument is 'young', indicating
 * whether the iteration was just over possibly-young entries. */

typedef void (*entries_action)(entries_t, bool, void *);

/* Type of a function to apply to a config object after iterating over
 * the entries. The second argument is 'young', indicating whether the
 * iteration was just over possibly-young entries. */

typedef void (*config_action)(memprof_config_t, bool, void *);

/* Iterate an entry_action over entries in a single entries table,
 * followed by an entries_action on the whole table.  If `young` is
 * true, only apply to possibly-young entries (usually a small number
 * of entries, often zero). */

static void entries_apply_actions(entries_t entries, bool young,
                                  entry_action f, void *data,
                                  entries_action after)
{
  for (uintnat i = young ? entries->young : 0; i < entries->live; ++i) {
    f(&entries->t[i], data);
  }
  if (after) {
    after(entries, young, data);
  }
}

/* Iterate entry_action/entries_action over all entries managed by a
 * single domain. */

static void domain_apply_actions(memprof_domain_t domain, bool young,
                                 entry_action f, void *data,
                                 entries_action after)
{
  entries_apply_actions(&domain->entries, young, f, data, after);
  memprof_thread_t thread = domain->threads;
  while (thread) {
    entries_apply_actions(&thread->entries, young, f, data, after);
    thread = thread->next;
  }
}

/* Iterate entry_action/entries_action/config_action over any entries
 * shared between all domains, */

static void shared_apply_actions(bool young, entry_action f, void *data,
                                 entries_action after,
                                 config_action config_act)
{
  memprof_config_t config = configs;
  while (config) {
    entries_apply_actions(&config->entries, young, f, data, after);
    if (config_act) {
      config_act(config, young, data);
    }
    config = config->next;
  }
}

/* Root scanning */

struct entry_scan_closure {
  scanning_action f;
  scanning_action_flags fflags;
  void *fdata;
};

/* An entry_action to scan the user_data root */

static void entry_scan(entry_t entry, void *data)
{
  struct entry_scan_closure *closure = data;
  closure->f(closure->fdata, entry->user_data, &entry->user_data);
}

/* A config_action is needed to scan the tracker root */

static void config_scan(memprof_config_t config, bool young, void *data)
{
  (void)young;
  struct entry_scan_closure *closure = data;
  closure->f(closure->fdata, config->tracker, &config->tracker);
}

/* Scan all memprof roots for a GC (or for compaction, after that is merged). */

void caml_memprof_scan_roots(scanning_action f,
                             scanning_action_flags fflags,
                             void* fdata,
                             caml_domain_state *state,
                             _Bool young,
                             _Bool global)
{
  struct entry_scan_closure closure = {f, fflags, fdata};
  memprof_domain_t domain = state->memprof;
  domain_apply_actions(domain, young, entry_scan, &closure, NULL);
  if (global) {
    shared_apply_actions(young, entry_scan, &closure, NULL, config_scan);
  }
}

/* Post-GC actions: we have to notice when tracked blocks die or get promoted */

/* An entry_action to scan a single entry after a minor GC. Notices
 * when a young tracked block has died or been promoted. */

static void update_entry_after_minor_gc(entry_t entry, void *data)
{
  (void)data;
  CAMLassert(Is_block(entry->block) || entry->deleted || entry->deallocated ||
             Is_placeholder(entry->block));
  if (Is_block(entry->block) && Is_young(entry->block)) {
    if (Hd_val(entry->block) == 0) {
      /* Block has been promoted */
      entry->block = Field(entry->block, 0);
      entry->promoted = 1;
    } else {
      /* Block is dead */
      entry->block = Val_unit;
      entry->deallocated = 1;
    }
  }
}

/* An entries_action for use after a minor GC. */

static void update_entries_after_minor_gc(entries_t entries,
                                          bool young,
                                          void *data)
{
  (void)data;
  (void)young;
  /* There are no 'young' entries left */
  entries->young = entries->live;
}

/* Update all memprof structures for a given domain, at the end of a
 * minor GC. If `global` is set, also update any shared or orphaned
 * structures. */

void caml_memprof_after_minor_gc(caml_domain_state *state, bool global)
{
  memprof_domain_t domain = state->memprof;
  domain_apply_actions(domain, true, update_entry_after_minor_gc,
                       NULL, update_entries_after_minor_gc);
  if (global) {
    shared_apply_actions(true, update_entry_after_minor_gc, NULL,
                         update_entries_after_minor_gc, NULL);
  }
}

static void update_entry_after_major_gc(entry_t entry, void *data)
{
  (void)data;
  CAMLassert(Is_block(entry->block) || entry->deleted || entry->deallocated ||
             Is_placeholder(entry->block));
  if (Is_block(entry->block) && !Is_young(entry->block)) {
    CAMLassert(!entry->alloc_young || entry->promoted);
    if (is_unmarked(entry->block)) {
      entry->block = Val_unit;
      entry->deallocated = 1;
    }
  }
}

void caml_memprof_after_major_gc(caml_domain_state *state, bool global)
{
  memprof_domain_t domain = state->memprof;
  domain_apply_actions(domain, false, update_entry_after_major_gc,
                       NULL, NULL);
  if (global) {
    shared_apply_actions(true, update_entry_after_major_gc, NULL, NULL, NULL);
  }
  set_action_pending_as_needed(domain);
}
