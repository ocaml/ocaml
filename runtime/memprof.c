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
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"

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
 */

/* type aliases for the hierarchy of data structures. */

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

  /* Linked list of threads in this domain */
  memprof_thread_t threads;

  /* The current thread's memprof state. Note that there may not be a
     "current thread". TODO: maybe this shouldn't be nullable.
     Nullability costs us some effort and may be meaningless. See call
     site of caml_memprof_leave_thread() in st_stubs.c. */
  memprof_thread_t current;
};

struct memprof_orphan_table_s {
  /* An orphaned entries table */
  entries_s entries;

  /* next orphaned table in a linked list. */
  memprof_orphan_table_t next;
};

/* List of orphaned entry tables not yet adopted by any domain. */
static memprof_orphan_table_t orphans = NULL;

/* lock controlling access to `orphans` variable */
static caml_plat_mutex orphans_lock = CAML_PLAT_MUTEX_INITIALIZER;

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
CAMLunused_start
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
CAMLunused_end

/* Transfer all entries from one entries table to another, excluding
 * ones which have not run any callbacks (these are deleted).
 * Return `false` if allocation fails. */

static bool entries_transfer(entries_t from, entries_t to)
{
  if (from->size == 0)
    return true;

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

/* Return current sampling configuration for a thread. If it's been
 * discarded, then reset it to CONFIG_NONE and return that. */

static value thread_config(memprof_thread_t thread)
{
  value config = thread->entries.config;
  if ((config != CONFIG_NONE) &&
      (Status(config) == CONFIG_STATUS_DISCARDED)) {
    thread->entries.config = config = CONFIG_NONE;
  }
  return config;
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

  caml_plat_lock(&orphans_lock);
  ot->next = orphans;
  orphans = domain->orphans;
  caml_plat_unlock(&orphans_lock);
  domain->orphans = NULL;
}

/* Adopt all global orphans to the given domain. */

CAMLunused_start
static void orphans_adopt(memprof_domain_t domain)
{
  /* Find the end of the domain's orphans list */
  memprof_orphan_table_t *p = &domain->orphans;
  while(*p) {
    p = &(*p)->next;
  }

  caml_plat_lock(&orphans_lock);
  *p = orphans;
  orphans = NULL;
  caml_plat_unlock(&orphans_lock);
}
CAMLunused_end

/* Destroy an orphan table. */

CAMLunused_start
static void orphans_destroy(memprof_orphan_table_t ot)
{
  entries_clear(&ot->entries);
  caml_stat_free(ot);
}
CAMLunused_end

/**** Create and destroy thread state structures ****/

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

/* Destroy a thread data structure.
 * The thread's entries table must be empty. */

static void thread_destroy(memprof_thread_t thread)
{
  memprof_domain_t domain = thread->domain;

  /* A thread cannot be destroyed from inside a callback, as
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
    CAMLassert(*p);
    p = &(*p)->next;
  }

  *p = thread->next;

  caml_stat_free(thread);
}

/**** Create and destroy domain state structures ****/

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

  caml_stat_free(domain);
}

static memprof_domain_t domain_create(caml_domain_state *caml_state)
{
  memprof_domain_t domain = caml_stat_alloc(sizeof(memprof_domain_s));
  if (!domain) {
    return NULL;
  }

  domain->caml_state = caml_state;
  entries_init(&domain->entries, MIN_ENTRIES_DOMAIN_CAPACITY, CONFIG_NONE);
  domain->orphans = NULL;
  domain->threads = NULL;
  domain->current = NULL;

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

/**** GC interface ****/

void caml_memprof_scan_roots(scanning_action f,
                             scanning_action_flags fflags,
                             void* fdata,
                             caml_domain_state *state,
                             bool weak,
                             bool global)
{
  memprof_domain_t domain = state->memprof;

  f(fdata, domain->entries.config, &domain->entries.config);
  memprof_thread_t thread = domain->threads;
  while (thread) {
    f(fdata, thread->entries.config, &thread->entries.config);
    thread = thread->next;
  }
}

void caml_memprof_after_minor_gc(caml_domain_state *state, bool global)
{
}

void caml_memprof_after_major_gc(caml_domain_state *state, bool global)
{
}

/**** Running callbacks ****/

value caml_memprof_run_callbacks_exn(void)
{
  return Val_unit;
}

/**** Interface to domain module ***/

void caml_memprof_new_domain(caml_domain_state *parent,
                             caml_domain_state *child)
{
  memprof_domain_t domain = domain_create(child);

  child->memprof = domain;
  /* domain inherits configuration from parent */
  if (domain && parent) {
    domain->current->entries.config =
      domain->entries.config =
      parent->memprof->entries.config;
  }
}

void caml_memprof_delete_domain(caml_domain_state *state)
{
  if (!state->memprof) {
    return;
  }
  domain_destroy(state->memprof);
  state->memprof = NULL;
}

/**** Interface with domain action-pending flag ****/

/* If profiling is active in the current domain, and we may have some
 * callbacks pending, set the action pending flag. */

static void set_action_pending_as_needed(memprof_domain_t domain)
{
  if (!domain->current ||
      domain->current->suspended) return;
  if (domain->entries.active < domain->entries.size ||
      domain->current->entries.size > 0)
    caml_set_action_pending(domain->caml_state);
}

/* Set the suspended flag on `domain` to `s`. */

static void update_suspended(memprof_domain_t domain, bool s)
{
  if (domain->current) {
    domain->current->suspended = s;
  }
  caml_memprof_renew_minor_sample(domain->caml_state);
  if (!s) set_action_pending_as_needed(domain);
}

/* Set the suspended flag on the current domain to `s`. */

void caml_memprof_update_suspended(bool s) {
  update_suspended(Caml_state->memprof, s);
}

/**** Sampling procedures ****/

Caml_inline bool sampling(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->current;

  if (thread && !thread->suspended) {
    return Sampling(thread_config(thread));
  }
  return false;
}

/* Renew the next sample in a domain's minor heap. Could race with
 * sampling and profile-stopping code, so do not call from another
 * domain unless the world is stopped. Must be called after each minor
 * sample and after each minor collection. In practice, this is called
 * at each minor sample, at each minor collection, and when sampling
 * is suspended and unsuspended. Extra calls do not change the
 * statistical properties of the sampling because of the
 * memorylessness of the geometric distribution. */

void caml_memprof_renew_minor_sample(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  value *trigger = state->young_start;
  if (sampling(domain)) {
    /* set trigger based on geometric distribution */
  }
  CAMLassert((trigger >= state->young_start) &&
             (trigger <= state->young_ptr));
  state->memprof_young_trigger = trigger;
  caml_reset_young_limit(state);
}

/* Respond to the allocation of any block. Does not call callbacks. */

void caml_memprof_sample_block(value block,
                               size_t allocated_words,
                               size_t sampled_words,
                               int source)
{
}

/* Respond to hitting the memprof trigger on the minor heap. May
 * sample several distinct blocks in the combined allocation. Runs
 * allocation callbacks. */

void caml_memprof_sample_young(uintnat wosize, int from_caml,
                               int allocs, unsigned char* encoded_lens)
{
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

  caml_memprof_renew_minor_sample(Caml_state);

  CAMLreturn(config);
}

CAMLprim value caml_memprof_stop(value unit)
{
  memprof_domain_t domain = Caml_state->memprof;
  CAMLassert(domain);
  CAMLassert(domain->current);
  value config = thread_config(domain->current);

  if (config == CONFIG_NONE || Status(config) != CONFIG_STATUS_SAMPLING) {
    caml_failwith("Gc.Memprof.stop: no profile running.");
  }
  Set_status(config, CONFIG_STATUS_STOPPED);

  caml_memprof_renew_minor_sample(Caml_state);

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
