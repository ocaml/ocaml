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

#define CAML_INTERNALS

#include <math.h>
#include <stdbool.h>
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/frame_descriptors.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/runtime_events.h"
#include "caml/shared_heap.h"

/* Design
 *
 * 1. Data Design
 *
 * 1.1. Configuration
 *
 * A Gc.Memprof.t value (a "profile" from the OCaml point of view) is
 * a block on the OCaml heap containing the profile configuration. As
 * a profile may be shared between threads and domains, keeping it on
 * the OCaml heap allows us not to worry about its liveness - pointers
 * to it from memprof data structures are simply treated as GC roots.
 * The "status" field in this object allows distinct domains to safely
 * `stop` and `discard` (with atomic reads and writes).
 *
 * 1.2. Entries
 *
 * Each block of memory tracked by memprof is represented by an
 * "entry" structure (entry_s, *entry_t). It tracks the state of the
 * block of memory, and its progress through the various callbacks.
 *
 * A resizable table of entry structures is called an "entries" table
 * (entries_s, *entries_t). It tracks ranges of those entries which
 * may (a) be ripe for running a callback, (b) be marked for deletion,
 * or (c) contain pointers to the minor heap (to be scanned in a minor
 * collection). As processing each of these actions proceeds linearly
 * through the table, this tracking is done simply by keeping the
 * lowest possible entry index for each purpose. The code to perform
 * each action (running a callback, evicting a deleted entry, or
 * scanning a pointer) checks whether an entry does require the action
 * before performing it.
 *
 * The entries table also has a pointer to the configuration object on
 * the OCaml heap, for the profile under which all the entries in the
 * table were sampled. This allows callbacks on the table to be run at
 * any later time, regardless of the currently-sampling profile of the
 * particular domain running the callback. A consequence is that all
 * entries in a table must be from the same profile.
 *
 * After a profile is "discarded", entries may still exist for blocks
 * allocated in that profile, but no callbacks will be called for it
 * (those entries themselves will be discarded lazily).
 *
 * There is code for iterating over entries in a table, which is used
 * when scanning for GC roots or updating tables to reflect GC activity
 * (see below).
 *
 * 1.3. Threads
 *
 * The memprof state of a particular systhread is a "thread state"
 * (memprof_thread_s, *memprof_thread_t). It has an entries table, for
 * blocks allocated by this thread whose allocation callback has not
 * yet completed. All allocation callbacks are guaranteed to be called
 * by the thread performing the allocation (in the rare circumstance in
 * which this is impossible, the tracking entry is discarded).
 *
 * This thread state structure exists whether or not the systhreads
 * module is initialized (one thread state per domain), and whether or
 * not memprof is running.
 *
 * 1.4. Domains
 *
 * The memprof state of a domain is a "domain state"
 * (memprof_domain_s, *memprof_domain_t). It has an entries table, for
 * blocks allocated in this domain whose allocation callbacks have
 * completed. If a domain terminates, or starts a new profile, while
 * it still has tracked entries from a previous profile, those tracked
 * entries become "orphaned" (see below).
 *
 * The domain state has a linked list of thread states for all the
 * threads in the domain, and a pointer to the current thread state.
 *
 * This structure exists whether or not memprof is running. A pointer
 * to it is kept in the caml_domain_state.
 *
 * 1.5. Orphans
 *
 * When sampling is stopped for a profile, all domains and threads
 * continue to manage the entry tables for it as before, but without
 * sampling and creating new entries. However, if a domain _starts_ a
 * profile while it has entries (tracked blocks) from a previous
 * profile which has not been "discarded", it moves those entries to
 * its "orphans" list - a linked list of entry tables - for subsequent
 * processing.
 *
 * If a domain is terminated, all its current and orphaned entries
 * (and those of its threads) are moved to a global `orphans`
 * list. This list, and its protective lock `orphans_lock`, are the
 * only memprof global variables. No domain processes the entries in
 * the global orphans list directly: the first domain to look at the
 * list (either at a collection or when checking for pending
 * callbacks) adopts all entry tables on it into its own orphans list,
 * and then processes them as its own.
 *
 * 2. Synchronisation
 *
 * Mostly threads and domains are free to run callbacks on their own
 * allocated blocks without explicitly synchronising. Care is taken
 * not to assume that the memprof state of any given thread or entry
 * in a domain is preserved outside of memprof code, as another thread
 * in the same domain may run and modify that state, but we assume
 * that the systhreads module effectively serializes entries to
 * memprof within a single domain (for these purposes, entering and
 * returning from a callback is treated as leaving and re-entering
 * memprof code).
 *
 * However, there are some structures shared between domains. The main
 * such structure is the profile configuration object on the Caml
 * heap. The only field written in this object is the status field,
 * used to communicate between domains sharing the profile, when a
 * profile is stopped or discarded. This field is inspected or set
 * atomically by the `Status` and `Set_status` macros. If a profile is
 * found to be discarded (`CONFIG_STATUS_DISCARDED`) then no domain
 * need take any action on it (and we can lazily discard any state
 * from it).
 *
 * The only other data shared between domains is the global orphans
 * list. As noted above, this is protected by a single global lock,
 * `orphans_lock`. Because an entry table only gets onto the global
 * orphans list when its owning domain terminates (at which point all
 * threads of that domain have terminated), and a table is adopted
 * from the global orphans list before being processed, all callbacks
 * and other entry table processing is performed by a thread of the
 * domain which owns the entry table. (and actions of those threads
 * are serialized by `systhreads`).
 *
 * 3. Interface with GC
 *
 * 3.1. Root scanning
 *
 * Memprof may have a large number of strong GC roots: one per tracked
 * block, pointing to the tracking information ('minor or 'major, in
 * the Gc.Memprof.tracker sense), plus the pointer to a config block
 * in every entries table. Rather than manually registering and
 * deregistering all of these, the GC calls caml_memprof_scan_roots()
 * to scan them, in either minor or major collections. This function
 * is called by all domains in parallel. A single domain adopts any
 * global orphaned entries tables, and then each domain scans its own
 * roots.
 *
 * 3.2. Updating block status.
 *
 * After a major or minor GC, memprof has to check tracked blocks to
 * discover whether they have survived the GC, or (for a minor GC)
 * whether they have been promoted to the major heap. This is done by
 * caml_memprof_after_minor_gc() and caml_memprof_after_major_gc(),
 * which share the system for iterating over entries tables as used by
 * caml_memprof_scan_roots(). Again, these functions are called by all
 * domains in parallel; a single domain starts by adopting any global
 * orphaned entries tables, and then each domain updates its own
 * entries.
 *
 * 3.3. Compaction
 *
 * GC compaction may move all objects in the major heap, so all
 * memprof roots must be scanned and potentially updated, including
 * the weak roots (i.e. pointers to the tracked blocks). This is done
 * by the same caml_memprof_scan_roots() function as root scanning in
 * regular GCs, using a boolean argument to indicate that weak roots
 * should also be scanned.
 *
 * 4. Random Number Generation
 *
 * 4.1. Requirements
 *
 * We sample every word of allocation with the same probability
 * (lambda, usually very small) - a Bernoulli trial. For the
 * allocation of a block on the shared heap, or any allocation from
 * the C runtime, we need to know how many samples we make of that
 * block (usually zero). This is a **binomial random variable**,
 * parameterized by lambda and N (the number of words in the block,
 * including the header).
 *
 * For allocations by Caml on the minor heap, we use the existing GC
 * trigger mechanism, to cause Caml to enter the runtime when "the
 * next sample" is due. The amount of allocation before "the next
 * sample" is a **geometric random variable**, parameterized by
 * lambda.
 *
 * 4.2. Implementation
 *
 * We focus on generating geometric pseudo-random numbers (PRNs), and
 * simulate binomial PRNs for parameters (lambda, N) by counting
 * geometric PRNs for lambda which sum to no more than N.
 *
 * We use a high-quality high-performance 32-bit uniform PRNG
 * (xoshiro128+), with per-domain state vectors. We initialize the
 * per-domain state vector with a low-quality PRNG (SplitMX64), seeded
 * separately for each domain.
 *
 * To convert from a uniform PRN `u` to a geometric PRN `g`, we compute
 *
 *          g = floor(1 + log(u) / log(1-lambda))
 *
 * where we treat u as uniformly distributed in [0,1]. We pre-compute
 * 1/log(1-lambda) (called `one_log1m_lambda` here), and compute
 * log(u) using a combination of type punning and a 3rd-degree
 * polynomial (see `log_approx()`).
 *
 * For further efficiency we generate geometric PRNs in blocks, and
 * the generating code is designed to be vectorizable.
 *
 * 5. Backtraces
 *
 * We have to be able to sample the current backtrace at any
 * allocation point, and pass it (as a Caml array) to the allocation
 * callback. We assume that in most cases these backtraces have short
 * lifetimes, so we don't want to allocate them on the shared
 * heap. However, we can't always allocate them directly on the Caml
 * minor heap, as some allocations (e.g. allocating in the shared heap
 * from the runtime) may take place at points at which GC is not safe
 * (and so minor-heap allocation is not permitted).  In those cases we
 * "stash" the backtrace on the C heap, and copy it onto the Caml heap
 * when we are about to call the allocation callback.
 *
 * 6. Sampling
 *
 * We sample allocation for all threads in a domain which has a
 * currently sampling profile, except when such a thread is running a
 * memprof callback, which "suspends" sampling on that thread.
 *
 * Allocation sampling divides into two cases: one simple and one
 * complex.
 *
 * 6.1. Simple Sampling
 *
 * When sampling an allocation by the runtime (as opposed to
 * allocation by Caml), an entry is added to the thread's entry table,
 * for subsequent processing. No allocation callback is called at
 * allocation time, because the heap may not be consistent so
 * allocation by the callback is not safe (see "Backtraces").
 *
 * 6.2. Minor Heap Caml Allocation Sampling
 *
 * Caml code allocates on the minor heap by pointer-bumping, and only
 * drops into the runtime if the `young_ptr` allocation pointer hits
 * the `young_trigger`, usually triggering a garbage collection. When
 * profiling, we set the trigger at the next word which we want to
 * sample (see "Random Number Generation"), thus allowing us to enter
 * memprof code at the approporiate allocation point. However,
 * sampling the allocation is more complex in this case for several
 * reasons:
 *
 * - Deferred allocation. A sampled block is not actually allocated
 *   until the runtime returns to the GC poll point in Caml code,
 *   after the memprof sampling code has run. So we have to predict
 *   the address of the sampled block for the entry record, to track
 *   its future promotion or collection. Until the allocation callback
 *   has run, instead of the allocated block address, the entry holds
 *   the offset in words of the block within the combined allocation,
 *   and the entry's `offset` field is set.
 *
 * - Combined allocations. A single GC poll point in Caml code may
 *   combine the allocation of several distinct blocks, each of which
 *   may be sampled independently. We create an entry for each sampled
 *   block and then run all allocation callbacks.
 *
 * - Prompt allocation callbacks. We call allocation callbacks
 *   directly from memprof as we sample the allocated blocks. These
 *   callbacks could be deferred (as are the ones in the "Simple
 *   Sampling" case), but that would require twice as many entries
 *   into memprof code. So the allocation callback is called before
 *   the sampled block is actually allocated (see above), and several
 *   allocation callbacks may be called at any given GC poll point
 *   (due to combined allocations). We take care to arrange heap
 *   metadata such that it is safe to run allocation callbacks (which
 *   may allocate and trigger minor and major GCs).
 *
 * - Other callbacks. In order to call the allocation callbacks from
 *   the poll point, we process the thread's entries table. This may
 *   call other callbacks for the same thread (specifically: deferred
 *   "Simple Sampling" callbacks).
 *
 * - Callback effects. Any callback may raise an exception, stop
 *   sampling, start a new profile, and/or discard a profile.
 *
 *   If a callback raises an exception, none of the allocations from
 *   the current poll point will take place. However, some allocation
 *   callbacks may already have been called. If so, we mark those
 *   entries as "deallocated", so that matching deallocation callbacks
 *   will run. We simply delete any tracking entry from the current
 *   poll point which has not yet run an allocation callback. Then we
 *   propagate the exception up to Caml.
 *
 *   If a callback stops sampling, subsequent allocations from the
 *   current poll point will not be sampled.
 *
 *   If a callback stops sampling and starts a new profile, none of
 *   the allocations from the current poll point are subsequently
 *   tracked (through promotion and/or deallocation), as it's not
 *   possible to reconstruct the allocation addresses of the tracking
 *   entries, so they are simply deleted (or marked as deallocated, as
 *   in the exceptional case). The new profile effectively begins with
 *   the following poll point or other allocation.
 *
 * Most of this complexity is managed in caml_memprof_sample_young().
 *
 * 7. Callbacks
 *
 * Some callbacks are run at allocation time, for allocations from
 * Caml (see under "Sampling" above). Other allocation callbacks, and
 * all post-allocation callbacks, are run during
 * `caml_memprof_run_callbacks_res()`, which is called by the
 * runtime's general pending-action mechanism at poll points.
 *
 * We set the domain's action-pending flag when we notice we have
 * pending callbacks. Caml drops into the runtime at a poll point, and
 * calls `caml_memprof_run_callbacks_res()`, whenever the
 * action-pending flag is set, whether or not memprof set it. So
 * memprof maintains its own per-domain `pending` flag, to avoid
 * suspending/unsuspending sampling, and checking all the entries
 * tables, when there are no pending callbacks.
 *
 * This is particularly important because when we unsuspend sampling,
 * we reset the young-limit, which has the side-effect of setting the
 * domain's action-pending flag. TODO: consider changing
 * `caml_reset_young_limit` so it doesn't do this.
 *
 * Allocation callbacks are always run by the thread which made the
 * allocation, unless that thread terminates before running the
 * callback, in which case it is inherited by the domain.
 *
 * Callbacks are run by iterating through candidate entries in a entry
 * table. See under "Entries" above. A single entry may have more than
 * one callback to run (if, for example, it has been promoted *and*
 * garbage collected since the last time callbacks for that entry were
 * run) - they are run in the natural order.
 */

/* number of random variables in a batch */
#define RAND_BLOCK_SIZE 64

/* type aliases for the hierarchy of structures for managing memprof status */

typedef struct entry_s entry_s, *entry_t;
typedef struct entries_s entries_s, *entries_t;
typedef struct memprof_domain_s memprof_domain_s, *memprof_domain_t;
typedef struct memprof_thread_s memprof_thread_s, *memprof_thread_t;
typedef struct memprof_orphan_table_s memprof_orphan_table_s,
  *memprof_orphan_table_t;

/* A memprof configuration is held in an object on the Caml heap, of
 * type Gc.Memprof.t. Here we define getter macros for each field, and
 * a setter macro for the status field (which is updated). */

#define CONFIG_FIELDS 9

#define CONFIG_FIELD_STATUS        0
#define CONFIG_FIELD_LAMBDA        1
#define CONFIG_FIELD_1LOG1ML       2
#define CONFIG_FIELD_STACK_FRAMES  3
#define CONFIG_FIELD_ALLOC_MINOR   4
#define CONFIG_FIELD_ALLOC_MAJOR   5
#define CONFIG_FIELD_PROMOTE       6
#define CONFIG_FIELD_DEALLOC_MINOR 7
#define CONFIG_FIELD_DEALLOC_MAJOR 8

#define CONFIG_FIELD_FIRST_CALLBACK CONFIG_FIELD_ALLOC_MINOR
#define CONFIG_FIELD_LAST_CALLBACK CONFIG_FIELD_DEALLOC_MAJOR

#define CONFIG_STATUS_SAMPLING 0
#define CONFIG_STATUS_STOPPED 1
#define CONFIG_STATUS_DISCARDED 2

#define CONFIG_NONE Val_unit

#define Status(config)          Int_val(Field(config, CONFIG_FIELD_STATUS))
#define Sampling(config)        ((config != CONFIG_NONE) && \
                                 (Status(config) == CONFIG_STATUS_SAMPLING))

/* The 'status' field is the only one we ever update. */

#define Set_status(config, stat) \
  Store_field(config, CONFIG_FIELD_STATUS, Val_int(stat))

/* lambda: the fraction of allocated words to sample.  0 <= lambda <= 1 */
#define Lambda(config) \
  Double_val(Field(config, CONFIG_FIELD_LAMBDA))

/* 1/ln(1-lambda), pre-computed for use in the geometric RNG */
#define One_log1m_lambda(config) \
  Double_val(Field(config, CONFIG_FIELD_1LOG1ML))

/* If lambda is zero or very small, computing one_log1m_lambda
 * underflows.  It should always be treated as negative infinity in
 * that case, (effectively turning sampling off). */
#define MIN_ONE_LOG1M_LAMBDA (-INFINITY)

#define Min_lambda(config) \
  (One_log1m_lambda(config) == MIN_ONE_LOG1M_LAMBDA)

/* The number of stack frames to record for each allocation site */
#define Callstack_size(config) \
  Int_val(Field(config, CONFIG_FIELD_STACK_FRAMES))

/* callbacks */
#define Alloc_minor(config)   Field(config, CONFIG_FIELD_ALLOC_MINOR)
#define Alloc_major(config)   Field(config, CONFIG_FIELD_ALLOC_MAJOR)
#define Promote(config)       Field(config, CONFIG_FIELD_PROMOTE)
#define Dealloc_minor(config) Field(config, CONFIG_FIELD_DEALLOC_MINOR)
#define Dealloc_major(config) Field(config, CONFIG_FIELD_DEALLOC_MAJOR)

/* Callback indexes. "Major" and "minor" are not distinguished here. */

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

/* Structure for each tracked allocation. Six words (with many spare
 * bits in the final word). */

struct entry_s {
  /* Memory block being sampled. This is a weak GC root. Note that
   * during the allocation callback of a block allocated directly by OCaml,
   * this may be a comballoc offset (and the `offset` flag set). */
  value block;

  /* The value returned by the previous callback for this block, or
   * the callstack (as a value-tagged pointer to the C heap) if the
   * alloc callback has not been called yet.  This is a strong GC
   * root. */
  value user_data;

  /* Number of samples in this block. */
  size_t samples;

  /* The size of this block, in words (not including the header). */
  size_t wosize;

  /* The thread currently running a callback for this entry,
   * or NULL if there is none */
  memprof_thread_t runner;

  /* The source of the allocation: normal allocations, interning,
   * or custom_mem (CAML_MEMPROF_SRC_*). */
  unsigned int source : 2;

  /* Is `block` actually an offset? */
  bool offset : 1;

  /* Was this block initially allocated in the minor heap? */
  bool alloc_young : 1;

  /* Has this block been promoted? Implies [alloc_young]. */
  bool promoted : 1;

  /* Has this block been deallocated? */
  bool deallocated : 1;

  /* Has this entry been marked for deletion. */
  bool deleted : 1;

  /* Which callback (CB_*) is currently running for this entry.
   * Useful when debugging. */
  unsigned int callback : CB_BITS;

  /* A mask of callbacks (1 << (CB_* - 1)) which have been called (not
   * necessarily completed) for this entry. */
  unsigned int callbacks : CB_MAX;

  /* There are a number of spare bits here for future expansion,
   * without increasing the size of an entry */
};

/* A resizable array of entry_s entries. */

struct entries_s {
  entry_t t; /* Pointer to array of entry_s structures */
  size_t min_capacity, capacity, size; /* array allocation management */

  /* Before this position, the [block] and [user_data] fields both
   * point to the major heap ([young <= size]). */
  size_t young;

  /* There are no blocks to be evicted before this position
   * ([evict <= size]). */
  size_t evict;

  /* There are no pending callbacks before this position
   * ([active <= size]). */
  size_t active;

  /* The profiling configuration under which these blocks were
   * allocated. A strong GC root. */
  value config;
};

/* Per-thread memprof state. */

/* Minimum capacity of a per-thread entries array */
#define MIN_ENTRIES_THREAD_CAPACITY 16

/* Minimum capacity of a per-domain entries array */
#define MIN_ENTRIES_DOMAIN_CAPACITY 128

/* Minimum capacity of an orphaned entries array */
#define MIN_ENTRIES_ORPHAN_CAPACITY 16

struct memprof_thread_s {
  /* [suspended] is used for inhibiting memprof callbacks when
     a callback is running or when an uncaught exception handler is
     called. */
  bool suspended;

  /* The index of the entry in `running_table` for which this thread is
   * currently in a callback */
  size_t running_index;

  /* Pointer to entries table for the current callback, or NULL if not
   * currently running a callback. */
  entries_t running_table;

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
   * entries for threads in this domain which terminated before
   * calling the allocation callback.  entries.config is the current
   * memprof configuration for this domain. */
  entries_s entries;

  /* Orphaned entries - either from previous profiles run in this
   * domain or adopted from terminated domains. */
  memprof_orphan_table_t orphans;

  /* true if there may be callbacks to be processed on the orphans list. */
  bool orphans_pending;

  /* true if there may be any callbacks pending for this domain */
  bool pending;

  /* Linked list of threads in this domain */
  memprof_thread_t threads;

  /* The current thread's memprof state. */
  memprof_thread_t current;

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

struct memprof_orphan_table_s {
  /* An orphaned entries table */
  entries_s entries;

  /* next orphaned table in a linked list. */
  memprof_orphan_table_t next;
};

/* List of orphaned entry tables not yet adopted by any domain. */
static memprof_orphan_table_t orphans = NULL;

/* lock controlling access to `orphans` and writes to `orphans_present` */
static caml_plat_mutex orphans_lock = CAML_PLAT_MUTEX_INITIALIZER;

/* Flag indicating non-NULL orphans. Only modified when holding orphans_lock. */
static atomic_uintnat orphans_present;

/**** Initializing and clearing entries tables ****/

static void entries_init(entries_t es, size_t min_capacity, value config)
{
  es->t = NULL;
  es->min_capacity = min_capacity;
  es->capacity = es->size = es->young = es->evict = es->active = 0;
  es->config = config;
}

static void entries_clear(entries_t es)
{
  if (es->t) {
    caml_stat_free(es->t);
    es->t = NULL;
  }
  es->capacity = es->size = es->young = es->evict = es->active = 0;
  es->config = CONFIG_NONE;
}

/**** Managing entries. ****/

/* When an entries table needs to grow, grow it by this factor */
#define ENTRIES_GROWTH_FACTOR 2

/* Do not shrink an entries table until it is this much too large */
#define ENTRIES_SHRINK_FACTOR 4

/* Reallocate the [es] entries table if it is either too small or too
 * large. [grow] is the number of free cells needed.
 * Returns false if reallocation was necessary but failed, and truer
 * otherwise. */

static bool entries_ensure(entries_t es, size_t grow)
{
  if (es->capacity == 0 && grow == 0) {
    /* Don't want min_capacity for an unused table. */
    return true;
  }
  size_t new_size = es->size + grow;
  if (new_size <= es->capacity &&
     (ENTRIES_SHRINK_FACTOR * new_size >= es->capacity ||
      es->capacity == es->min_capacity)) {
    /* No need to grow or shrink */
    return true;
  }
  size_t new_capacity = new_size * ENTRIES_GROWTH_FACTOR;
  if (new_capacity < es->min_capacity)
    new_capacity = es->min_capacity;
  entry_t new_t = caml_stat_resize_noexc(es->t, new_capacity * sizeof(entry_s));
  if (new_t == NULL) return false;
  es->t = new_t;
  es->capacity = new_capacity;
  return true;
}

#define Invalid_index (~(size_t)0)

/* Create and initialize a new entry in an entries table, and return
 * its index (or Invalid_index if allocation fails). */

Caml_inline size_t new_entry(entries_t es,
                             value block, value user_data,
                             size_t wosize, size_t samples,
                             int source, bool is_young,
                             bool offset)
{
  if (!entries_ensure(es, 1))
    return Invalid_index;
  size_t i = es->size ++;
  entry_t e = es->t + i;
  e->block = block;
  e->user_data = user_data;
  e->samples = samples;
  e->wosize = wosize;
  e->runner = NULL;
  e->source = source;
  e->offset = offset;
  e->alloc_young = is_young;
  e->promoted = false;
  e->deallocated = false;
  e->deleted = false;
  e->callback = CB_NONE;
  e->callbacks = 0;
  return i;
}

/* Mark a given entry in an entries table as "deleted". Do not call on
 * an entry with a currently-running callback. */

static void entry_delete(entries_t es, size_t i)
{
  entry_t e = &es->t[i];

  CAMLassert(!e->runner);

  e->deleted = true;
  e->offset = false;
  e->user_data = Val_unit;
  e->block = Val_unit;
  if (i < es->evict) es->evict = i;
}

/* Remove any deleted entries from [es], updating [es->young] and
 * [es->active] if necessary. */

static void entries_evict(entries_t es)
{
  size_t i, j;

  /* The obvious linear compaction algorithm */
  j = i = es->evict;

  while (i < es->size) {
    if (!es->t[i].deleted) { /* keep this entry */
      if (i != j) {
        es->t[j] = es->t[i];
        if (es->t[i].runner) {
          memprof_thread_t runner = es->t[i].runner;
          CAMLassert(runner->running_table == es);
          CAMLassert(runner->running_index == i);
          runner->running_index = j;
        }
      }
      ++ j;
    }
    ++ i;
    if (es->young == i) es->young = j;
    if (es->active == i) es->active = j;
  }
  es->evict = es->size = j;
  CAMLassert(es->active <= es->size);
  CAMLassert(es->young <= es->size);

  entries_ensure(es, 0);
}

/* Remove any offset entries from [es]. Ones which have completed an
 * allocation callback but not a deallocation callback are marked as
 * deallocated. Others are marked as deleted.
 *
 * This is called before moving entries from a thread's entries table
 * to that of the domain, when we're about to orphan all the domain's
 * entries. This can occur if we stop a profile and start another one
 * during an allocation callback (either directly in the callback or
 * on another thread while the callback is running). We'll never be
 * able to connect an offset entry to its allocated block (the block
 * will not be actually allocated until the callback completes, if at
 * all), but some callbacks may already have been run for it. If no
 * callbacks have been run, we simply mark the entry as deleted. If
 * the allocation callback has been run, the best we can do is
 * probably to fake deallocating the block, so that alloc/dealloc
 * callback counts correspond.
 *
 * Note: no callbacks apart from the allocation callback can run on an
 * offset entry (as the block has not yet been allocated, it cannot be
 * promoted or deallocated). */

static void entries_clear_offsets(entries_t es)
{
  for (size_t i = 0; i < es->size; ++i) {
    entry_t e = &es->t[i];
    if (e->offset) {
      if (e->callbacks & CB_MASK(CB_ALLOC)) {
        /* Have called just the allocation callback */
        CAMLassert(e->callbacks == CB_MASK(CB_ALLOC));
        e->block = Val_unit;
        e->offset = false;
        e->deallocated = true;
        if (i < es->active) es->active = i;
      } else {
        /* Haven't yet called any callbacks */
        CAMLassert(e->runner == NULL);
        CAMLassert(e->callbacks == 0);
        entry_delete(es, i);
      }
    }
  }
  entries_evict(es);
}

/* Remove any entries from [es] which are not currently running a
 * callback. */

static void entries_clear_inactive(entries_t es)
{
  CAMLassert (es->config == CONFIG_NONE);
  for (size_t i = 0; i < es->size; ++i) {
    if (es->t[i].runner == NULL) {
      entry_delete(es, i);
    }
  }
  entries_evict(es);
}

static value validated_config(entries_t es);

/* Transfer all entries from one entries table to another, excluding
 * ones which have not run any callbacks (these are deleted).
 * Return `false` if allocation fails. */

static bool entries_transfer(entries_t from, entries_t to)
{
  if (from->size == 0)
    return true;

  (void)validated_config(from); /* For side-effect, so we can check ... */
  (void)validated_config(to);   /* ... that the configs are equal. */
  CAMLassert(from->config == to->config);

  if (!entries_ensure(to, from->size))
    return false;

  size_t delta = to->size;
  to->size += from->size;

  for (size_t i = 0; i < from->size; ++i) {
    if (from->t[i].callbacks == 0) {
      /* Very rare: transferring an entry which hasn't called its
       * allocation callback. We just delete it. */
      entry_delete(from, i);
    }
    to->t[i + delta] = from->t[i];
    memprof_thread_t runner = from->t[i].runner;
    if (runner) { /* unusual */
      CAMLassert(runner->running_table == from);
      CAMLassert(runner->running_index == i);
      runner->running_table = to;
      runner->running_index = i + delta;
    }
  }

  if (to->young == delta) {
    to->young = from->young + delta;
  }
  if (to->evict == delta) {
    to->evict = from->evict + delta;
  }
  if (to->active == delta) {
    to->active = from->active + delta;
  }
  /* Reset `from` to empty, and allow it to shrink */
  from->young = from->evict = from->active = from->size = 0;
  entries_ensure(from, 0);
  return true;
}

/* If es->config points to a DISCARDED configuration, update
 * es->config to CONFIG_NONE. Return es->config. */

static value validated_config(entries_t es)
{
  if ((es->config != CONFIG_NONE) &&
      (Status(es->config) == CONFIG_STATUS_DISCARDED)) {
    es->config = CONFIG_NONE;
    entries_clear_inactive(es);
  }
  return es->config;
}

/* Return current sampling configuration for a thread. If it's been
 * discarded, then reset it to CONFIG_NONE and return that. */

static value thread_config(memprof_thread_t thread)
{
  return validated_config(&thread->entries);
}

/*** Create and destroy orphan tables ***/

/* Orphan any surviving entries from a domain or its threads (after
 * first discarding any deleted and offset entries), onto the domain's
 * orphans list. This copies the domain's table itself, to avoid
 * copying the potentially live array.
 *
 * Returns false if allocation fails, true otherwise. */

static bool orphans_create(memprof_domain_t domain)
{
  /* Clear offset entries and count survivors in threads tables. */
  size_t total_size = 0;
  memprof_thread_t thread = domain->threads;
  while (thread) {
    entries_clear_offsets(&thread->entries);
    total_size += thread->entries.size;
    thread = thread->next;
  }
  entries_t es = &domain->entries;
  entries_evict(es); /* remove deleted entries */
  total_size += es->size;

  if (!total_size) /* No entries to orphan */
    return true;

  memprof_orphan_table_t ot = caml_stat_alloc(sizeof(memprof_orphan_table_s));
  if (!ot)
    return false;

  entries_init(&ot->entries, MIN_ENTRIES_ORPHAN_CAPACITY,
               domain->entries.config);
  if (!entries_ensure(&ot->entries, total_size)) {
    /* Couldn't allocate entries table - failure */
    caml_stat_free(ot);
    return false;
  }

  /* Orphan surviving entries; these transfers will succeed
   * because we pre-sized the table. */
  (void)entries_transfer(&domain->entries, &ot->entries);
  thread = domain->threads;
  while(thread) {
    /* May discard entries which haven't run allocation callbacks */
    (void)entries_transfer(&thread->entries, &ot->entries);
    thread = thread->next;
  }
  ot->next = domain->orphans;
  domain->orphans = ot;
  return true;
}

/* Abandon all a domain's orphans to the global list. */

static void orphans_abandon(memprof_domain_t domain)
{
  /* Find the end of the domain's orphans list */
  memprof_orphan_table_t ot = domain->orphans;
  if (!ot)
    return;

  while(ot->next) {
    ot = ot->next;
  }

  caml_plat_lock_blocking(&orphans_lock);
  ot->next = orphans;
  orphans = domain->orphans;
  atomic_store_release(&orphans_present, 1);
  caml_plat_unlock(&orphans_lock);
  domain->orphans = NULL;
}

/* Adopt all global orphans to the given domain. */

static void orphans_adopt(memprof_domain_t domain)
{
  if (!atomic_load_acquire(&orphans_present))
    return; /* No orphans to adopt */

  /* Find the end of the domain's orphans list */
  memprof_orphan_table_t *p = &domain->orphans;
  while(*p) {
    p = &(*p)->next;
  }

  caml_plat_lock_blocking(&orphans_lock);
  if (orphans) {
    *p = orphans;
    orphans = NULL;
    atomic_store_release(&orphans_present, 0);
  }
  caml_plat_unlock(&orphans_lock);
}

/* Destroy an orphan table. */

static void orphans_destroy(memprof_orphan_table_t ot)
{
  entries_clear(&ot->entries);
  caml_stat_free(ot);
}

/* Traverse a domain's orphans list, clearing inactive entries from
 * discarded tables and removing any table which is empty, and update
 * the orphans_pending flag. */

static void orphans_update_pending(memprof_domain_t domain)
{
  memprof_orphan_table_t *p = &domain->orphans;
  bool pending = false;

  while(*p) {
    memprof_orphan_table_t ot = *p;
    memprof_orphan_table_t next = ot->next;
    value config = validated_config(&ot->entries);
    if (config == CONFIG_NONE) { /* remove inactive entries */
      entries_clear_inactive(&ot->entries);
    }
    if (ot->entries.size == 0) {
      orphans_destroy(ot);
      *p = next;
    } else { /* any pending entries in this table? */
      pending |= (ot->entries.active < ot->entries.size);
      p = &ot->next;
    }
  }
  domain->orphans_pending = pending;
}

/**** Statistical sampling ****/

/* We use a low-quality SplitMix64 PRNG to initialize state vectors
 * for a high-quality high-performance 32-bit PRNG (xoshiro128+). That
 * PRNG generates uniform random 32-bit numbers, which we use in turn
 * to generate geometric random numbers parameterized by [lambda].
 * This is all coded in such a way that compilers can readily use SIMD
 * optimisations. */

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

static void xoshiro_init(memprof_domain_t domain, uint64_t seed)
{
  uint64_t splitmix64_state = seed;
  for (int i = 0; i < RAND_BLOCK_SIZE; i++) {
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
 * 0.
 *
 * Uses a type pun to break y+0.5 into biased exponent `exp` (an
 * integer-valued float in the range [126, 159]) and mantissa `x` (a
 * float in [1,2)). This may discard up to eight low bits of y.
 *
 * Then y+0.5 = x * 2^(exp-127), so if f(x) ~= log(x) - 159*log(2),
 * log((y+0.5)/2^32) ~= f(x) + exp * log(2).
 *
 * We use sollya to find the unique degree-3 polynomial f such that :
 *
 *    - Its average value is that of log(x) - 159*log(2) for x in [1, 2)
 *          (so the sampling has the right mean when lambda is small).
 *    - f(1) = f(2) - log(2), so the approximation is continuous.
 *    - The error at x=1 is -1e-5, so the approximation is always negative.
 *    - The maximum absolute error is minimized in [1, 2) (the actual
 *      maximum absolute error is around 7e-4). */

Caml_inline float log_approx(uint32_t y)
{
  union { float f; int32_t i; } u;
  u.f = y + 0.5f;
  float exp = (float)(u.i >> 23);
  u.i = (u.i & 0x7FFFFF) | 0x3F800000;
  float x = u.f;
  return (-111.70172433407f +
          x * (2.104659476859f +
               x * (-0.720478916626f +
                    x * 0.107132064797f)) +
          0.6931471805f * exp);
}

/* This function regenerates [RAND_BLOCK_SIZE] geometric random
 * variables at once. Doing this by batches help us gain performances:
 * many compilers (e.g., GCC, CLang, ICC) will be able to use SIMD
 * instructions to get a performance boost. */

#ifdef SUPPORTS_TREE_VECTORIZE
__attribute__((optimize("tree-vectorize")))
#endif

static void rand_batch(memprof_domain_t domain)
{
  float one_log1m_lambda = One_log1m_lambda(domain->entries.config);

  /* Instead of using temporary buffers, we could use one big loop,
     but it turns out SIMD optimizations of compilers are more fragile
     when using larger loops.  */
  uint32_t A[RAND_BLOCK_SIZE];
  float B[RAND_BLOCK_SIZE];

  /* Generate uniform variables in A using the xoshiro128+ PRNG. */
  for (int i = 0; i < RAND_BLOCK_SIZE; i++)
    A[i] = xoshiro_next(domain, i);

  /* Generate exponential random variables by computing logarithms. */
  for (int i = 0; i < RAND_BLOCK_SIZE; i++)
    B[i] = 1 + log_approx(A[i]) * one_log1m_lambda;

  /* We do the final flooring for generating geometric
     variables. Compilers are unlikely to use SIMD instructions for
     this loop, because it involves a conditional and variables of
     different sizes (32 and 64 bits). */
  for (int i = 0; i < RAND_BLOCK_SIZE; i++) {
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
  CAMLassert(One_log1m_lambda(domain->entries.config) <= 0.);
  if (domain->rand_pos == RAND_BLOCK_SIZE)
    rand_batch(domain);
  res = domain->rand_geom_buff[domain->rand_pos++];
  CAMLassert(1 <= res);
  CAMLassert(res <= Max_long);
  return res;
}

/* Initialize per-domain PRNG, so we're ready to sample. */

static void rand_init(memprof_domain_t domain)
{
  domain->rand_pos = RAND_BLOCK_SIZE;
  if (domain->entries.config != CONFIG_NONE
      && !Min_lambda(domain->entries.config)) {
    /* next_rand_geom can be zero if the next word is to be sampled,
     * but rand_geom always returns a value >= 1. Subtract 1 to correct. */
    domain->next_rand_geom = rand_geom(domain) - 1;
  }
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
 *  Hormann, Wolfgang. "The generation of binomial random variates."
 *  Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.
 */

static uintnat rand_binom(memprof_domain_t domain, uintnat len)
{
  uintnat res;
  CAMLassert(len < Max_long);
  for (res = 0; domain->next_rand_geom < len; res++)
    domain->next_rand_geom += rand_geom(domain);
  domain->next_rand_geom -= len;
  return res;
}

/**** Create and destroy thread state structures ****/

/* Create a thread state structure attached to `domain`. */

static memprof_thread_t thread_create(memprof_domain_t domain)
{
  memprof_thread_t thread = caml_stat_alloc(sizeof(memprof_thread_s));
  if (!thread) {
    return NULL;
  }
  thread->suspended = false;
  thread->running_index = 0;
  thread->running_table = NULL;
  entries_init(&thread->entries, MIN_ENTRIES_THREAD_CAPACITY,
               domain->entries.config);

  /* attach to domain record */
  thread->domain = domain;
  thread->next = domain->threads;
  domain->threads = thread;

  return thread;
}

/* Destroy a thread state structure.  If the thread's entries table is
 * not empty (because allocation failed when transferring it to the
 * domain) then its entries will be lost. */

static void thread_destroy(memprof_thread_t thread)
{
  memprof_domain_t domain = thread->domain;

  /* A thread cannot be destroyed while inside a callback, as
   * Thread.exit works by raising an exception, taking us out of the
   * callback, and a domain won't terminate while any thread is
   * alive. */
  CAMLassert (!thread->running_table);
  /* We would like to assert (thread->entries.size == 0), but this may
   * not be true if allocation failed when transferring the thread's
   * entries to its domain (in which case we are about to lose those
   * entries. */
  entries_clear(&thread->entries);

  if (domain->current == thread) {
    domain->current = NULL;
  }
  /* remove thread from the per-domain list. Could go faster if we
   * used a doubly-linked list, but that's premature optimisation
   * at this point. */
  memprof_thread_t *p = &domain->threads;
  while (*p != thread) {
    CAMLassert(*p); /* checks that thread is on the list */
    p = &(*p)->next;
  }
  *p = thread->next;

  caml_stat_free(thread);
}

/**** Create and destroy domain state structures ****/

/* Destroy a domain state structure. In the usual case, this will
 * orphan any entries belonging to the domain or its threads onto the
 * global orphans list. However, if there is an allocation failure,
 * some or all of those entries may be lost. */

static void domain_destroy(memprof_domain_t domain)
{
  /* Orphan any entries from the domain or its threads, then abandon
   * all orphans to the global table. If creating the orphans table
   * fails due to allocation failure, we lose the entries. */
  (void)orphans_create(domain);
  orphans_abandon(domain);

  /* Destroy thread structures */
  memprof_thread_t thread = domain->threads;
  while (thread) {
    memprof_thread_t next = thread->next;
    thread_destroy(thread);
    thread = next;
  }

  entries_clear(&domain->entries); /* In case allocation failed */
  caml_stat_free(domain->callstack_buffer);
  caml_stat_free(domain);
}

/* Create a domain state structure */

static memprof_domain_t domain_create(caml_domain_state *caml_state)
{
  memprof_domain_t domain = caml_stat_alloc(sizeof(memprof_domain_s));
  if (!domain) {
    return NULL;
  }

  domain->caml_state = caml_state;
  entries_init(&domain->entries, MIN_ENTRIES_DOMAIN_CAPACITY, CONFIG_NONE);
  domain->orphans = NULL;
  domain->orphans_pending = false;
  domain->pending = false;
  domain->threads = NULL;
  domain->current = NULL;
  domain->callstack_buffer = NULL;
  domain->callstack_buffer_len = 0;

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

/**** Interface with domain action-pending flag ****/

/* If a domain has some callbacks pending, and isn't currently
 * suspended, set the action pending flag. */

static void set_action_pending_as_needed(memprof_domain_t domain)
{
  CAMLassert(domain->current);
  if (domain->current->suspended) return;
  domain->pending = (domain->entries.active < domain->entries.size ||
                     domain->current->entries.size > 0 ||
                     domain->orphans_pending);
  if (domain->pending) {
    caml_set_action_pending(domain->caml_state);
  }
}

/* Set the suspended flag on `domain` to `s`. Has the side-effect of
 * setting the trigger. */

static void update_suspended(memprof_domain_t domain, bool s)
{
  CAMLassert(domain->current);
  domain->current->suspended = s;
  /* If we are unsuspending, set the action-pending flag if
   * we have callbacks to run. */
  if (!s) set_action_pending_as_needed(domain);

  caml_memprof_set_trigger(domain->caml_state);
  caml_reset_young_limit(domain->caml_state);
}

/* Set the suspended flag on the current domain to `s`.
 * Has the side-effect of setting the trigger. */

void caml_memprof_update_suspended(bool s) {
  CAMLassert(Caml_state->memprof);
  update_suspended(Caml_state->memprof, s);
}

/**** Iterating over entries ****/

/* Type of a function to apply to a single entry. Returns true if,
 * following the call, the entry may have a newly-applicable
 * callback. */

typedef bool (*entry_action)(entry_t, void *);

/* Type of a function to apply to an entries array after iterating
 * over the entries. */

typedef void (*entries_action)(entries_t, void *);

/* Iterate an entry_action over entries in a single entries table,
 * followed by an (optional) entries_action on the whole table.  If
 * `young` is true, only apply to possibly-young entries (usually a
 * small number of entries, often zero).
 *
 * This function validates the entries table configuration (which
 * changes it to NONE if DISCARDED). If then it is NONE, this function
 * does nothing else.
 *
 * Assumes that calling `f` does not change entry table indexes. */

static void entries_apply_actions(entries_t entries, bool young,
                                  entry_action f, void *data,
                                  entries_action after)
{
  value config = validated_config(entries);
  if (config == CONFIG_NONE) {
    return;
  }

  for (size_t i = young ? entries->young : 0; i < entries->size; ++i) {
    if (f(&entries->t[i], data) && entries->active > i) {
      entries->active = i;
    }
  }
  if (after) {
    after(entries, data);
  }
}

/* Iterate entry_action/entries_action over all entries managed by a
 * single domain (including those managed by its threads).
 *
 * Assumes that calling `f` does not modify entry table indexes. */

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
  memprof_orphan_table_t ot = domain->orphans;
  while (ot) {
    entries_apply_actions(&ot->entries, young, f, data, after);
    ot = ot->next;
  }
}

/**** GC interface ****/

/* Root scanning */

struct scan_closure {
  scanning_action f;
  scanning_action_flags fflags;
  void *fdata;
  bool weak;
};

/* An entry_action to scan roots */

static bool entry_scan(entry_t e, void *data)
{
  struct scan_closure *closure = data;
  closure->f(closure->fdata, e->user_data, &e->user_data);
  if (closure->weak && !e->offset && (e->block != Val_unit)) {
    closure->f(closure->fdata, e->block, &e->block);
  }
  return false;
}

/* An entries_action to scan the config root */

static void entries_finish_scan(entries_t es, void *data)
{
  struct scan_closure *closure = data;
  closure->f(closure->fdata, es->config, &es->config);
}

/* Function called by either major or minor GC to scan all the memprof roots */

void caml_memprof_scan_roots(scanning_action f,
                             scanning_action_flags fflags,
                             void* fdata,
                             caml_domain_state *state,
                             bool weak)
{
  memprof_domain_t domain = state->memprof;
  CAMLassert(domain);

  /* Adopt all global orphans into this domain. */
  orphans_adopt(domain);

  bool young = (fflags & SCANNING_ONLY_YOUNG_VALUES);
  struct scan_closure closure = {f, fflags, fdata, weak};
  domain_apply_actions(domain, young,
                       entry_scan, &closure, entries_finish_scan);
}

/* Post-GC actions: we have to notice when tracked blocks die or get promoted */

/* An entry_action to update a single entry after a minor GC. Notices
 * when a young tracked block has died or been promoted. */

static bool entry_update_after_minor_gc(entry_t e, void *data)
{
  (void)data;
  CAMLassert(Is_block(e->block)
             || e->deleted || e->deallocated || e->offset);
  if (!e->offset && Is_block(e->block) && Is_young(e->block)) {
    if (Hd_val(e->block) == 0) {
      /* Block has been promoted */
      e->block = Field(e->block, 0);
      e->promoted = true;
    } else {
      /* Block is dead */
      e->block = Val_unit;
      e->deallocated = true;
    }
    return true; /* either promotion or deallocation callback */
  }
  return false; /* no callback triggered */
}

/* An entries_action for use after a minor GC. */

static void entries_update_after_minor_gc(entries_t entries,
                                          void *data)
{
  (void)data;
  /* There are no 'young' entries left */
  entries->young = entries->size;
}

/* Update all memprof structures for a given domain, at the end of a
 * minor GC. */

void caml_memprof_after_minor_gc(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  CAMLassert(domain);

  /* Adopt all global orphans into this domain. */
  orphans_adopt(domain);

  domain_apply_actions(domain, true, entry_update_after_minor_gc,
                       NULL, entries_update_after_minor_gc);
  orphans_update_pending(domain);
  set_action_pending_as_needed(domain);
}

/* An entry_action to update a single entry after a major GC. Notices
 * when a tracked block has died. */

static bool entry_update_after_major_gc(entry_t e, void *data)
{
  (void)data;
  CAMLassert(Is_block(e->block)
             || e->deleted || e->deallocated || e->offset);
  if (!e->offset && Is_block(e->block) && !Is_young(e->block)) {
    /* Either born in the major heap or promoted */
    CAMLassert(!e->alloc_young || e->promoted);
    if (is_unmarked(e->block)) { /* died */
      e->block = Val_unit;
      e->deallocated = true;
      return true; /* trigger deallocation callback */
    }
  }
  return false; /* no callback triggered */
}

/* Note: there's nothing to be done at the table level after a major
 * GC (unlike a minor GC, when we reset the 'young' index), so there
 * is no "entries_update_after_major_gc" function. */

/* Update all memprof structures for a given domain, at the end of a
 * major GC. */

void caml_memprof_after_major_gc(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  CAMLassert(domain);

  /* Adopt all global orphans into this domain. */
  orphans_adopt(domain);

  domain_apply_actions(domain, false, entry_update_after_major_gc,
                       NULL, NULL);
  orphans_update_pending(domain);
  set_action_pending_as_needed(domain);
}

/**** Interface to domain module ***/

void caml_memprof_new_domain(caml_domain_state *parent,
                             caml_domain_state *child)
{
  memprof_domain_t domain = domain_create(child);
  child->memprof = domain;

  if (domain == NULL) /* failure - domain creation will fail */
    return;

  /* domain inherits configuration from parent */
  if (parent) {
    CAMLassert(parent->memprof);
    CAMLassert(domain->current);
    domain->current->entries.config =
      domain->entries.config =
      parent->memprof->entries.config;
  }
  /* Initialize RNG */
  xoshiro_init(domain, (uint64_t)child->id);

  /* If already profiling, set up RNG */
  rand_init(domain);
}

void caml_memprof_delete_domain(caml_domain_state *state)
{
  CAMLassert(state->memprof);

  domain_destroy(state->memprof);
  state->memprof = NULL;
}

/**** Capturing the call stack *****/

/* A "stashed" callstack, allocated on the C heap. */

typedef struct {
        size_t frames;
        backtrace_slot stack[];
} callstack_stash_s, *callstack_stash_t;

/* How large a callstack buffer must be to be considered "large" */
#define CALLSTACK_BUFFER_LARGE 256

/* How much larger a callstack buffer must be, compared to the most
 * recent callstack, to be considered large. */
#define CALLSTACK_BUFFER_FACTOR 8

/* If the per-domain callstack buffer is "large" and we've only used a
 * small part of it, free it. This saves us from C heap bloat due to
 * unbounded lifetime of the callstack buffers (as callstacks may
 * sometimes be huge). */

static void shrink_callstack_buffer(memprof_domain_t domain, size_t frames)
{
  if (domain->callstack_buffer_len > CALLSTACK_BUFFER_LARGE &&
      domain->callstack_buffer_len > frames * CALLSTACK_BUFFER_FACTOR) {
    caml_stat_free(domain->callstack_buffer);
    domain->callstack_buffer = NULL;
    domain->callstack_buffer_len = 0;
  }
}

/* Capture the call stack when sampling an allocation from the
 * runtime. We don't have to account for combined allocations
 * (Comballocs) but we can't allocate the resulting stack on the Caml
 * heap, because the heap may be in an invalid state so we can't cause
 * a GC. Therefore, we capture the callstack onto the C heap, and will
 * copy it onto the Caml heap later, when we're ready to call the
 * allocation callback. The callstack is returned as a Val_ptr value
 * (or an empty array, if allocation fails). */

static value capture_callstack_no_GC(memprof_domain_t domain)
{
  value res = Atom(0); /* empty array. */
  size_t frames =
    caml_get_callstack(Callstack_size(domain->entries.config),
                       &domain->callstack_buffer,
                       &domain->callstack_buffer_len, -1);
  if (frames) {
    callstack_stash_t stash = caml_stat_alloc_noexc(sizeof(callstack_stash_s)
                                                    + frames * sizeof(value));
    if (stash) {
      stash->frames = frames;
      memcpy(stash->stack, domain->callstack_buffer,
             sizeof(backtrace_slot) * frames);
      res = Val_ptr(stash);
    }
  }

  shrink_callstack_buffer(domain, frames);
  return res;
}

/* Capture the call stack when sampling an allocation from Caml. We
 * have to deal with combined allocations (Comballocs), but can
 * allocate the resulting call stack directly on the Caml heap. Should
 * be called with [domain->current->suspended] set, as it allocates.
 * May cause a GC. */

static value capture_callstack_GC(memprof_domain_t domain, int alloc_idx)
{
  CAMLassert(domain->current->suspended);

  size_t frames =
    caml_get_callstack(Callstack_size(domain->entries.config),
                       &domain->callstack_buffer,
                       &domain->callstack_buffer_len,
                       alloc_idx);
  value res = caml_alloc(frames, 0);
  for (size_t i = 0; i < frames; ++i) {
    Field(res, i) = Val_backtrace_slot(domain->callstack_buffer[i]);
  }

  shrink_callstack_buffer(domain, frames);
  return res;
}

/* Given a stashed callstack, copy it to the Caml heap and free the
 * stash. Given a non-stashed callstack, simply return it. */

static value unstash_callstack(value callstack)
{
  CAMLparam1(callstack);
  if (Is_long(callstack)) { /* stashed on C heap */
    callstack_stash_t stash = Ptr_val(callstack);
    callstack = caml_alloc(stash->frames, 0);
    for (size_t i = 0; i < stash->frames; ++i) {
      Field(callstack, i) = Val_backtrace_slot(stash->stack[i]);
    }
    caml_stat_free(stash);
  }
  CAMLreturn(callstack);
}

/**** Running callbacks ****/

/* Runs a single callback, in thread `thread`, for entry number `i` in
 * table `es`. The callback closure is `cb`, the parameter is `param`,
 * and the "callback index" is `cb_index`.
 * Returns unit or an exception result. */

static caml_result run_callback_res(
  memprof_thread_t thread,
  entries_t es, size_t i,
  value cb, value param,
  uintnat cb_index)
{
  entry_t e = &es->t[i];

  if (e->runner) { /* some other thread has got to this callback first */
    return Result_unit;
  }

  thread->running_table = es;
  thread->running_index = i;
  e->runner = thread;

  e->callback = cb_index;
  e->callbacks |= CB_MASK(cb_index);
  e->user_data = Val_unit;      /* Release root. */

  caml_result res = caml_callback_res(cb, param);

  /* The entry may have been moved to another table under our feet,
   * due to the callback or to other threads from this domain. For
   * example, if a new profile is started. */
  es = thread->running_table;
  thread->running_table = NULL;
  i = thread->running_index;

  CAMLassert(es != NULL);
  CAMLassert(i < es->size);
  e = &es->t[i];
  CAMLassert(e->runner == thread);
  e->runner = NULL;
  e->callback = CB_NONE;

  if (validated_config(es) == CONFIG_NONE) {
    /* The profile was discarded during the callback.
     * no entries to update etc. */
    if (!caml_result_is_exception(res))
      return Result_unit;
  }

  if (caml_result_is_exception(res) || res.data == Val_unit) {
    /* Callback raised an exception or returned None or (), discard
       this entry. */
    entry_delete(es, i);
    return res;
  } else {
    value v = res.data;
    /* Callback returned [Some _]. Store the value in [user_data]. */
    CAMLassert(Is_block(v));
    CAMLassert(Tag_val(v) == 0);
    CAMLassert(Wosize_val(v) == 1);
    e->user_data = Some_val(v);
    if (Is_block(e->user_data) && Is_young(e->user_data) &&
        i < es->young)
      es->young = i;

    /* The callback we just ran was not a dealloc (they return unit)
     * so there may be more callbacks to run on this entry.  If the
     * block has been deallocated, or promoted and we were not running
     * a promotion callback, mark this entry as ready to run. */
    if (i < es->active &&
        (e->deallocated ||
         (e->promoted && (cb_index != CB_PROMOTE))))
      es->active = i;

    return Result_unit;
  }
}

/* Run the allocation callback for a given entry of an entries array.
 * Returns Val_unit or an exception result. */

static caml_result run_alloc_callback_res(
  memprof_thread_t thread, entries_t es, size_t i)
{
  entry_t e = &es->t[i];
  CAMLassert(e->deallocated || e->offset || Is_block(e->block));

  e->user_data = unstash_callstack(e->user_data);
  value sample_info = caml_alloc_small(4, 0);
  Field(sample_info, 0) = Val_long(e->samples);
  Field(sample_info, 1) = Val_long(e->wosize);
  Field(sample_info, 2) = Val_long(e->source);
  Field(sample_info, 3) = e->user_data;
  value callback =
    e->alloc_young ? Alloc_minor(es->config) : Alloc_major(es->config);
  return run_callback_res(thread, es, i, callback, sample_info, CB_ALLOC);
}

/* Run any pending callbacks from entries table `es` in thread
 * `thread`. Returns either (a) when a callback raises an exception,
 * or (b) when all pending callbacks have been run. */

static caml_result entries_run_callbacks_res(
  memprof_thread_t thread, entries_t es)
{
  caml_result res = Result_unit;

  /* Note: several callbacks may be called for a single entry. */
  while (es->active < es->size) {
    /* Examine and possibly run a callback on the entry at es->active.
     * Running a callback may change many things, including es->active
     * and es->config. */
    value config = validated_config(es);
    if (config == CONFIG_NONE) break;
    size_t i = es->active;
    entry_t e = &es->t[i];

    if (e->deleted || e->runner) {
      /* This entry is already deleted, or is running a callback. Ignore it. */
      ++ es->active;
    } else if (!(e->callbacks & CB_MASK(CB_ALLOC))) {
      /* allocation callback hasn't been run */
      if (Status(config) == CONFIG_STATUS_SAMPLING) {
        res = run_alloc_callback_res(thread, es, i);
        if (caml_result_is_exception(res)) break;
      } else {
        /* sampling stopped, e.g. by a previous callback; drop this entry */
        entry_delete(es, i);
      }
    } else if (e->promoted && !(e->callbacks & CB_MASK(CB_PROMOTE))) {
      /* promoted entry; call promote callback */
      res = run_callback_res(thread, es, i,
                             Promote(config), e->user_data,
                             CB_PROMOTE);
      if (caml_result_is_exception(res)) break;
    } else if (e->deallocated && !(e->callbacks & CB_MASK(CB_DEALLOC))) {
      /* deallocated entry; call dealloc callback */
      value cb = (e->promoted || !e->alloc_young) ?
        Dealloc_major(config) : Dealloc_minor(config);
      res = run_callback_res(thread, es, i,
                             cb, e->user_data,
                             CB_DEALLOC);
      if (caml_result_is_exception(res)) break;
    } else {
      /* There is nothing to do with this entry. */
      ++ es->active;
    }
  }
  entries_evict(es);
  return res;
}

/* Run any pending callbacks for the current thread and domain, and
 * any orphaned callbacks.
 *
 * Does not use domain_apply_actions() because this can dynamically
 * change the various indexes into an entries table while iterating
 * over it, whereas domain_apply_actions assumes that can't happen. */

caml_result caml_memprof_run_callbacks_res(void)
{
  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  memprof_thread_t thread = domain->current;
  CAMLassert(thread);
  caml_result res = Result_unit;
  if (thread->suspended || !domain->pending) return res;

  orphans_adopt(domain);
  update_suspended(domain, true);

  /* run per-domain callbacks first */
  res = entries_run_callbacks_res(thread, &domain->entries);
  if (caml_result_is_exception(res)) goto end;

  /* run per-thread callbacks for current thread */
  res = entries_run_callbacks_res(thread, &thread->entries);
  if (caml_result_is_exception(res)) goto end;
  /* Move any surviving entries from allocating thread to owning
   * domain, so their subsequent callbacks may be run by any thread in
   * the domain. entries_run_callbacks_res didn't return an exception,
   * so all these entries have had their allocation callbacks run. If
   * this fails due to allocation failure, the entries remain with the
   * thread, which is OK. */
  (void)entries_transfer(&thread->entries, &domain->entries);

  /* now run per-domain orphaned callbacks. */
  memprof_orphan_table_t ot = domain->orphans;
  while (ot) {
    entries_t es = &ot->entries;
    if ((validated_config(es) != CONFIG_NONE) && (es->active < es->size)) {
      /* An orphan table with something to run. */
      res = entries_run_callbacks_res(thread, es);
      if (caml_result_is_exception(res)) goto end;
      /* Orphan tables may be deallocated during callbacks (if a
       * callback discards the profile and then orphans_update_pending
       * runs due to a GC) but a callback from an orphan table can
       * never deallocate _that_ orphan table, so we can continue down
       * the list. */
    }
    ot = ot->next;
  }

 end:
  orphans_update_pending(domain);
  update_suspended(domain, false);
  return res;
}

/**** Sampling ****/

/* Is the current thread currently sampling? */

Caml_inline bool sampling(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->current;

  if (thread && !thread->suspended) {
    value config = thread_config(thread);
    return Sampling(config) && !Min_lambda(config);
  }
  return false;
}

/* Respond to the allocation of a block [block], size [wosize], with
 * [samples] samples. [src] is one of the [CAML_MEMPROF_SRC_] enum values
 * ([Gc.Memprof.allocation_source]). */

static void maybe_track_block(memprof_domain_t domain,
                              value block, size_t samples,
                              size_t wosize, int src)
{
  if (samples == 0) return;

  value callstack = capture_callstack_no_GC(domain);
  (void)new_entry(&domain->current->entries, block, callstack,
                  wosize, samples, src, Is_young(block), false);
  set_action_pending_as_needed(domain);
}

/* Sets the trigger for the next sample in a domain's minor
 * heap. Could race with sampling and profile-stopping code, so do not
 * call from another domain unless the world is stopped (at the time
 * of writing, this is only actually called from this domain). Must be
 * called after each minor sample and after each minor collection. In
 * practice, this is called at each minor sample, at each minor
 * collection, and when sampling is suspended and unsuspended. Extra
 * calls do not change the statistical properties of the sampling
 * because of the memorylessness of the geometric distribution. */

void caml_memprof_set_trigger(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  CAMLassert(domain);
  value *trigger = state->young_start;
  if (sampling(domain)) {
    uintnat geom = rand_geom(domain);
    if (state->young_ptr - state->young_start > geom) {
      trigger = state->young_ptr - (geom - 1);
    }
  }

  CAMLassert(trigger >= state->young_start);
  CAMLassert(trigger <= state->young_ptr);
  state->memprof_young_trigger = trigger;
}

/* Respond to the allocation of any block. Does not call callbacks. */

void caml_memprof_sample_block(value block,
                               size_t allocated_words,
                               size_t sampled_words,
                               int source)
{
  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  CAMLassert(sampled_words >= allocated_words);
  if (sampling(domain)) {
    maybe_track_block(domain, block, rand_binom(domain, sampled_words),
                      allocated_words, source);
  }
}

/* Respond to hitting the memprof trigger on the minor heap. May
 * sample several distinct blocks in the combined allocation. Runs
 * allocation callbacks. */

void caml_memprof_sample_young(uintnat wosize, int from_caml,
                               int allocs, unsigned char* encoded_lens)
{
  CAMLparam0();
  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  memprof_thread_t thread = domain->current;
  CAMLassert(thread);
  entries_t entries = &thread->entries;
  uintnat whsize = Whsize_wosize(wosize);
  CAMLlocalresult(res);
  CAMLlocal1(config);
  config = entries->config;

  /* When a domain is not sampling, the memprof trigger is not
   * set, so we should not come into this function. */
  CAMLassert(sampling(domain));

  if (!from_caml) {
    /* Not coming from Caml, so this isn't a comballoc. We know we're
     * sampling at least once, but maybe more than once. */
    size_t samples = 1 +
      rand_binom(domain,
                 Caml_state->memprof_young_trigger - 1 - Caml_state->young_ptr);
    CAMLassert(encoded_lens == NULL);
    maybe_track_block(domain, Val_hp(Caml_state->young_ptr),
                      samples, wosize, CAML_MEMPROF_SRC_NORMAL);
    caml_memprof_set_trigger(Caml_state);
    caml_reset_young_limit(Caml_state);
    CAMLreturn0;
  }

  /* The memprof trigger lies in (young_ptr, young_ptr + whsize] */
  CAMLassert(Caml_state->young_ptr < Caml_state->memprof_young_trigger);
  CAMLassert(Caml_state->memprof_young_trigger <=
             Caml_state->young_ptr + whsize);

  /* Trigger offset from the base of the combined allocation. We
   * reduce this for each sample in this comballoc. Signed so it can
   * go negative. */
  intnat trigger_ofs =
    Caml_state->memprof_young_trigger - Caml_state->young_ptr;
  /* Sub-allocation offset from the base of the combined
   * allocation. Signed so we can compare correctly against
   * trigger_ofs. */
  intnat alloc_ofs = whsize;

  /* Undo the combined allocation, so that we can allocate callstacks
   * and in callbacks. */
  Caml_state->young_ptr += whsize;

  /* Suspend profiling, so we don't profile allocations of callstacks
   * or in callbacks. Resets trigger. */
  update_suspended(domain, true);

  /* Work through the sub-allocations, high address to low address,
   * identifying which ones are sampled and how many times.  For each
   * sampled sub-allocation, create an entry in the thread's table. */
  size_t new_entries = 0; /* useful for debugging */
  size_t sub_alloc = allocs;
  do {
    -- sub_alloc;
    size_t alloc_wosz =
      encoded_lens == NULL ? wosize :
      Wosize_encoded_alloc_len(encoded_lens[sub_alloc]);
    alloc_ofs -= Whsize_wosize(alloc_wosz); /* base of this sub-alloc */

    /* count samples for this sub-alloc? */
    size_t samples = 0;
    while (alloc_ofs < trigger_ofs) {
      ++ samples;
      trigger_ofs -= rand_geom(domain);
    }

    if (samples) {
      value callstack = capture_callstack_GC(domain, sub_alloc);
      size_t entry =
        new_entry(entries, (value)alloc_ofs, callstack,
                  alloc_wosz, samples, CAML_MEMPROF_SRC_NORMAL,
                  true, true);
      if (entry != Invalid_index) {
        ++ new_entries;
      }
    }
  } while (sub_alloc);

  (void)new_entries; /* this variable is useful to assert */
  CAMLassert(alloc_ofs == 0);
  CAMLassert(trigger_ofs <= 0);
  CAMLassert(new_entries <= allocs);

  /* Run all outstanding callbacks in this thread's table, which
   * includes these recent allocation callbacks. If one of the
   * callbacks stops the profile, the other callbacks will still
   * run. */
  res = entries_run_callbacks_res(thread, entries);

  /* A callback, or another thread of this domain, may have stopped
   * the profile and then started another one. This will result in the
   * entries being transferred to the domain's table which is then
   * orphaned, deleting all offset entries. In this case,
   * thread->config will have changed. We will have run the allocation
   * callbacks up to the one which stopped the old profile. */
  bool restarted = (config != entries->config);

  /* A callback may have raised an exception. In this case, we are
   * going to cancel this whole combined allocation and should delete
   * the newly-created entries (if they are still in our table). */
  bool cancelled = caml_result_is_exception(res);

  if (!cancelled) {
    /* No exceptions were raised, so the allocations will
     * proceed. Make room in the minor heap for the blocks to be
     * allocated. We must not trigger a GC after this point. */
    while (Caml_state->young_ptr - whsize < Caml_state->young_trigger) {
      CAML_EV_COUNTER(EV_C_FORCE_MINOR_MEMPROF, 1);
      caml_poll_gc_work();
    }
    Caml_state->young_ptr -= whsize;
  }

  /* If profiling has been stopped and restarted by these callbacks,
   * the thread's entries table has been transferred to the domain and
   * orphaned, so must be empty. */

  if (restarted) {
    CAMLassert(entries->size == 0);
  }

  /* All deleted entries will have been evicted from the thread's
   * table. This may (often) include the offset entries we've just
   * created (if an allocation callback returns None, for
   * example). Any surviving offset entries will still be at the end
   * of this thread's table. If one of the callbacks has raised an
   * exception, we will not be allocating the blocks, so these entries
   * should be deleted (or marked as deallocated if the allocation
   * callback ran). Otherwise, they must be updated to point to the
   * blocks which will now be allocated. */

  if (cancelled) {
    entries_clear_offsets(entries);
  } else {
    for (size_t i = 0; i < entries->size; ++i) {
      entry_t e = &entries->t[i];
      if (e->offset) { /* an entry we just created */
        e->block = Val_hp(Caml_state->young_ptr + e->block);
        e->offset = false;
        if (i < entries->young) entries->young = i;
      }
    }
    /* There are now no outstanding allocation callbacks in the thread's
     * entries table. Transfer the whole thing to the domain. If this
     * fails due to allocation failure, the entries stay with the thread,
     * which is OK. */
    (void)entries_transfer(entries, &domain->entries);
  }

  /* Unsuspend profiling. Resets trigger. */
  update_suspended(domain, false);

  (void) caml_get_value_or_raise(res);

  CAMLreturn0;
}

/**** Interface with systhread. ****/

CAMLexport memprof_thread_t caml_memprof_new_thread(caml_domain_state *state)
{
  CAMLassert(state->memprof);
  return thread_create(state->memprof);
}

CAMLexport memprof_thread_t caml_memprof_main_thread(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  CAMLassert(domain);
  memprof_thread_t thread = domain->threads;
  CAMLassert(thread);

  /* There should currently be just one thread in this domain */
  CAMLassert(thread->next == NULL);
  return thread;
}

CAMLexport void caml_memprof_delete_thread(memprof_thread_t thread)
{
  /* Transfer entries to the domain. If this fails due to allocation
   * failure, we will lose the entries.  May discard entries which
   * haven't run allocation callbacks. */
  (void)entries_transfer(&thread->entries, &thread->domain->entries);
  thread_destroy(thread);
}

CAMLexport void caml_memprof_enter_thread(memprof_thread_t thread)
{
  CAMLassert(thread);
  thread->domain->current = thread;
  update_suspended(thread->domain, thread->suspended);
}

/**** Interface to OCaml ****/

CAMLprim value caml_memprof_start(value lv, value szv, value tracker)
{
  CAMLparam3(lv, szv, tracker);
  CAMLlocal1(one_log1m_lambda_v);

  double lambda = Double_val(lv);
  intnat sz = Long_val(szv);

  /* Checks that [lambda] is within range (and not NaN). */
  if (sz < 0 || !(lambda >= 0.0 && lambda <= 1.0))
    caml_invalid_argument("Gc.Memprof.start");

  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  CAMLassert(domain->current);

  if (Sampling(thread_config(domain->current))) {
    caml_failwith("Gc.Memprof.start: already started.");
  }

  /* Orphan any surviving tracking entries from a previous profile. */
  if (!orphans_create(domain)) {
    caml_raise_out_of_memory();
  }

  double one_log1m_lambda = lambda == 1.0 ? 0.0 : 1.0/caml_log1p(-lambda);
  /* Buggy implementations of caml_log1p could produce a
   * one_log1m_lambda which is positive infinity or NaN, which would
   * cause chaos in the RNG, so we check against this and set
   * one_log1m_lambda to negative infinity (which we can test for). We
   * preserve the user's value of Lambda for inspection or
   * debugging. */
  if (!(one_log1m_lambda <= 0.0)) { /* catches NaN, +Inf, +ve */
    one_log1m_lambda = MIN_ONE_LOG1M_LAMBDA; /* negative infinity */
  }

  one_log1m_lambda_v = caml_copy_double(one_log1m_lambda);

  value config = caml_alloc_shr(CONFIG_FIELDS, 0);
  caml_initialize(&Field(config, CONFIG_FIELD_STATUS),
                  Val_int(CONFIG_STATUS_SAMPLING));
  caml_initialize(&Field(config, CONFIG_FIELD_LAMBDA), lv);
  caml_initialize(&Field(config, CONFIG_FIELD_1LOG1ML), one_log1m_lambda_v);
  caml_initialize(&Field(config, CONFIG_FIELD_STACK_FRAMES), szv);
  for (int i = CONFIG_FIELD_FIRST_CALLBACK;
       i <= CONFIG_FIELD_LAST_CALLBACK; ++i) {
    caml_initialize(&Field(config, i), Field(tracker,
                                             i - CONFIG_FIELD_FIRST_CALLBACK));
  }
  CAMLassert(domain->entries.size == 0);

  /* Set config pointers of the domain and all its threads */
  domain->entries.config = config;
  memprof_thread_t thread = domain->threads;
  while (thread) {
    CAMLassert(thread->entries.size == 0);
    thread->entries.config = config;
    thread = thread->next;
  }

  /* reset PRNG, generate first batch of random numbers. */
  rand_init(domain);

  caml_memprof_set_trigger(Caml_state);
  caml_reset_young_limit(Caml_state);
  orphans_update_pending(domain);
  set_action_pending_as_needed(domain);

  CAMLreturn(config);
}

CAMLprim value caml_memprof_stop(value unit)
{
  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  memprof_thread_t thread = domain->current;
  CAMLassert(thread);

  /* Final attempt to run allocation callbacks; don't use
   * caml_memprof_run_callbacks_res as we only really need allocation
   * callbacks now. */
  if (!thread->suspended) {
    update_suspended(domain, true);
    caml_result res = entries_run_callbacks_res(thread, &thread->entries);
    update_suspended(domain, false);
    (void) caml_get_value_or_raise(res);
  }

  value config = thread_config(thread);
  if (config == CONFIG_NONE || Status(config) != CONFIG_STATUS_SAMPLING) {
    caml_failwith("Gc.Memprof.stop: no profile running.");
  }
  Set_status(config, CONFIG_STATUS_STOPPED);

  caml_memprof_set_trigger(Caml_state);
  caml_reset_young_limit(Caml_state);

  return Val_unit;
}

CAMLprim value caml_memprof_discard(value config)
{
  uintnat status = Status(config);
  CAMLassert((status == CONFIG_STATUS_STOPPED) ||
             (status == CONFIG_STATUS_SAMPLING) ||
             (status == CONFIG_STATUS_DISCARDED));

  switch (status) {
  case CONFIG_STATUS_STOPPED: /* correct case */
    break;
  case CONFIG_STATUS_SAMPLING:
    caml_failwith("Gc.Memprof.discard: profile not stopped.");
  case CONFIG_STATUS_DISCARDED:
    caml_failwith("Gc.Memprof.discard: profile already discarded.");
  }

  Set_status(config, CONFIG_STATUS_DISCARDED);

  return Val_unit;
}
