/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                                                                        */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#define _GNU_SOURCE  /* For sched.h CPU_ZERO(3) and CPU_COUNT(3) */
#include "caml/config.h"
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#ifdef HAS_GNU_GETAFFINITY_NP
#include <sched.h>
#ifdef HAS_PTHREAD_NP_H
#include <pthread_np.h>
#endif
#endif
#ifdef HAS_BSD_GETAFFINITY_NP
#include <pthread_np.h>
#include <sys/cpuset.h>
typedef cpuset_t cpu_set_t;
#endif
#ifdef _WIN32
#include <sysinfoapi.h>
#endif
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/callback.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/domain_state.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/gc_ctrl.h"
#include "caml/globroots.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/sync.h"
#include "caml/weak.h"

/* The runtime can run stop-the-world (STW) sections, during which all
   active domains run the same callback in parallel (with a barrier
   mechanism to synchronize within the callback).

   Stop-the-world sections are used to handle duties such as:
    - minor GC
    - major GC phase changes

   Code within the STW callback can have the guarantee that no mutator
   code runs in parallel -- precisely, the guarantee holds only for
   code that is followed by a barrier. Furthermore, new domains being
   spawned are blocked from running any mutator code while a STW
   section is in progress, and terminating domains cannot stop until
   they have participated to all STW sections currently in progress.

   To provide these guarantees:
    - Domains must register as STW participants before running any
      mutator code.
    - STW sections must not trigger other callbacks into mutator code
      (eg. finalisers or signal handlers).

   See the comments on [caml_try_run_on_all_domains_with_spin_work]
   below for more details on the synchronization mechanisms involved.
*/

/* For timely handling of STW requests, domains registered as STW
   participants must be careful to service STW interrupt requests. The
   compiler inserts "poll points" in mutator code, and the runtime
   uses a "backup thread" mechanism during blocking sections.

   When the main C-stack for a domain enters a blocking call,
   a 'backup thread' becomes responsible for servicing the STW
   sections on behalf of the domain. Care is needed to hand off duties
   for servicing STW sections between the main pthread and the backup
   pthread when caml_enter_blocking_section and
   caml_leave_blocking_section are called.

   When the state for the backup thread is BT_IN_BLOCKING_SECTION
   the backup thread will service the STW section.

   The state machine for the backup thread (and its transitions)
   are:

           BT_INIT  <---------------------------------------+
              |                                             |
   (install_backup_thread)                                  |
       [main pthread]                                       |
              |                                             |
              v                                             |
       BT_ENTERING_OCAML  <-----------------+               |
              |                             |               |
(caml_enter_blocking_section)               |               |
       [main pthread]                       |               |
              |                             |               |
              |                             |               |
              |               (caml_leave_blocking_section) |
              |                      [main pthread]         |
              v                             |               |
    BT_IN_BLOCKING_SECTION  ----------------+               |
              |                                             |
     (domain_terminate)                                     |
       [main pthread]                                       |
              |                                             |
              v                                             |
        BT_TERMINATE                               (backup_thread_func)
              |                                      [backup pthread]
              |                                             |
              +---------------------------------------------+

 */
#define BT_IN_BLOCKING_SECTION 0
#define BT_ENTERING_OCAML 1
#define BT_TERMINATE 2
#define BT_INIT 3

/* control of STW interrupts */
struct interruptor {
  atomic_uintnat* interrupt_word;
  caml_plat_mutex lock;
  caml_plat_cond cond;

  int running;
  int terminating;
  /* unlike the domain ID, this ID number is not reused */
  uintnat unique_id;

  atomic_uintnat interrupt_pending;
};

struct dom_internal {
  /* readonly fields, initialised and never modified */
  int id;
  caml_domain_state* state;
  struct interruptor interruptor;

  /* backup thread */
  int backup_thread_running;
  pthread_t backup_thread;
  atomic_uintnat backup_thread_msg;
  caml_plat_mutex domain_lock;
  caml_plat_cond domain_cond;

  /* modified only during STW sections */
  uintnat minor_heap_area_start;
  uintnat minor_heap_area_end;
};
typedef struct dom_internal dom_internal;


static struct {
  atomic_uintnat domains_still_running;
  atomic_uintnat num_domains_still_processing;
  void (*callback)(caml_domain_state*,
                   void*,
                   int participating_count,
                   caml_domain_state** others_participating);
  void* data;
  void (*enter_spin_callback)(caml_domain_state*, void*);
  void* enter_spin_data;

  /* barrier state */
  int num_domains;
  atomic_uintnat barrier;

  caml_domain_state* participating[Max_domains];
} stw_request = {
  ATOMIC_UINTNAT_INIT(0),
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  NULL,
  NULL,
  0,
  ATOMIC_UINTNAT_INIT(0),
  { 0 },
};

static caml_plat_mutex all_domains_lock = CAML_PLAT_MUTEX_INITIALIZER;
static caml_plat_cond all_domains_cond =
    CAML_PLAT_COND_INITIALIZER(&all_domains_lock);
static atomic_uintnat /* dom_internal* */ stw_leader = 0;
static struct dom_internal all_domains[Max_domains];

CAMLexport atomic_uintnat caml_num_domains_running;



/* size of the virtual memory reservation for the minor heap, per domain */
uintnat caml_minor_heap_max_wsz;
/*
  The amount of memory reserved for all minor heaps of all domains is
  Max_domains * caml_minor_heap_max_wsz. Individual domains can allocate
  smaller minor heaps, but when a domain calls Gc.set to allocate a bigger minor
  heap than this reservation, we perform a new virtual memory reservation based
  on the increased minor heap size.

  New domains are created with a minor heap of size
  caml_params->init_minor_heap_wsz.

  To perform a new virtual memory reservation for the heaps, we stop the world
  and do a minor collection on all domains.
  See [stw_resize_minor_heap_reservation].
*/

CAMLexport uintnat caml_minor_heaps_start;
CAMLexport uintnat caml_minor_heaps_end;
static __thread dom_internal* domain_self;

/*
 * This structure is protected by all_domains_lock
 * [0, participating_domains) are all the domains taking part in STW sections
 * [participating_domains, Max_domains) are all those domains free to be used
 */
static struct {
  int participating_domains;
  dom_internal* domains[Max_domains];
} stw_domains = {
  0,
  { 0 }
};

static void add_to_stw_domains(dom_internal* dom) {
  int i;
  CAMLassert(stw_domains.participating_domains < Max_domains);
  for(i=stw_domains.participating_domains; stw_domains.domains[i]!=dom; ++i) {
    CAMLassert(i<Max_domains);
  }

  /* swap passed domain with domain at stw_domains.participating_domains */
  dom = stw_domains.domains[stw_domains.participating_domains];
  stw_domains.domains[stw_domains.participating_domains] =
      stw_domains.domains[i];
  stw_domains.domains[i] = dom;
  stw_domains.participating_domains++;
}

static void remove_from_stw_domains(dom_internal* dom) {
  int i;
  for(i=0; stw_domains.domains[i]!=dom; ++i) {
    CAMLassert(i<Max_domains);
  }
  CAMLassert(i < stw_domains.participating_domains);

  /* swap passed domain to first free domain */
  stw_domains.participating_domains--;
  stw_domains.domains[i] =
      stw_domains.domains[stw_domains.participating_domains];
  stw_domains.domains[stw_domains.participating_domains] = dom;
}

static dom_internal* next_free_domain() {
  if (stw_domains.participating_domains == Max_domains)
    return NULL;

  CAMLassert(stw_domains.participating_domains < Max_domains);
  return stw_domains.domains[stw_domains.participating_domains];
}

#ifdef __APPLE__
/* OSX has issues with dynamic loading + exported TLS.
   This is slower but works */
CAMLexport pthread_key_t caml_domain_state_key;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;

static void caml_make_domain_state_key (void)
{
  (void) pthread_key_create (&caml_domain_state_key, NULL);
}

void caml_init_domain_state_key (void)
{
  pthread_once(&key_once, caml_make_domain_state_key);
}

#else
CAMLexport __thread caml_domain_state* caml_state;
#endif

Caml_inline void interrupt_domain(struct interruptor* s)
{
  atomic_store_rel(s->interrupt_word, (uintnat)(-1));
}

int caml_incoming_interrupts_queued(void)
{
  return atomic_load_acq(&domain_self->interruptor.interrupt_pending);
}

/* must NOT be called with s->lock held */
static void stw_handler(caml_domain_state* domain);
static uintnat handle_incoming(struct interruptor* s)
{
  uintnat handled = atomic_load_acq(&s->interrupt_pending);
  CAMLassert (s->running);
  if (handled) {
    atomic_store_rel(&s->interrupt_pending, 0);

    stw_handler(domain_self->state);
  }
  return handled;
}

static void handle_incoming_otherwise_relax (struct interruptor* self)
{
  if (!handle_incoming(self))
    cpu_relax();
}

void caml_handle_incoming_interrupts(void)
{
  handle_incoming(&domain_self->interruptor);
}

int caml_send_interrupt(struct interruptor* target)
{
  /* signal that there is an interrupt pending */
  atomic_store_rel(&target->interrupt_pending, 1);

  /* Signal the condition variable, in case the target is
     itself waiting for an interrupt to be processed elsewhere */
  caml_plat_lock(&target->lock);
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  interrupt_domain(target);

  return 1;
}

static void caml_wait_interrupt_serviced(struct interruptor* target)
{
  int i;

  /* Often, interrupt handlers are fast, so spin for a bit before waiting */
  for (i=0; i<1000; i++) {
    if (!atomic_load_acq(&target->interrupt_pending)) {
      return;
    }
    cpu_relax();
  }

  {
    SPIN_WAIT {
      if (!atomic_load_acq(&target->interrupt_pending))
        return;
    }
  }
}

asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t bs;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  return Wsize_bsize(bs);
}

/* The current minor heap layout is as follows:

- A contiguous memory block of size
   [caml_minor_heap_max_wsz * Max_domains]
  is reserved by [caml_init_domains]. The boundaries
  of this reserved area are stored in the globals
    [caml_minor_heaps_start]
  and
    [caml_minor_heaps_end].

- Each domain gets a reserved section of this block
  of size [caml_minor_heap_max_wsz], whose boundaries are stored as
    [domain_self->minor_heap_area_start]
  and
    [domain_self->minor_heap_area_end]

  These variables accessed in [stw_resize_minor_heap_reservation],
  synchronized by a global barrier.

- Each domain then commits a segment of size
    [domain_state->minor_heap_wsz]
  starting at
    [domain_state->minor_heap_area_start]
  that it actually uses.

  This is done below in
    [caml_reallocate_minor_heap]
  which is called both at domain-initialization (by [create_domain])
  and if a request comes to change the minor heap size.

  The boundaries of this committed memory area are
     [domain_state->young_start]
   and
     [domain_state->young_end].

  Those [young_{start,end}] variables are never accessed by another
  domain, so they need no synchronization.
*/

Caml_inline void check_minor_heap() {
  caml_domain_state* domain_state = Caml_state;
  CAMLassert(domain_state->young_ptr == domain_state->young_end);

  caml_gc_log("young_start: %p, young_end: %p, minor_heap_area_start: %p,"
      " minor_heap_area_end: %p, minor_heap_wsz: %"
      ARCH_SIZET_PRINTF_FORMAT "u words",
      domain_state->young_start,
      domain_state->young_end,
      (value*)domain_self->minor_heap_area_start,
      (value*)domain_self->minor_heap_area_end,
      domain_state->minor_heap_wsz);
  CAMLassert(
    (/* uninitialized minor heap */
      domain_state->young_start == NULL
      && domain_state->young_end == NULL)
    ||
    (/* initialized minor heap */
      domain_state->young_start == (value*)domain_self->minor_heap_area_start
      && domain_state->young_end <= (value*)domain_self->minor_heap_area_end));
}

static void free_minor_heap() {
  caml_domain_state* domain_state = Caml_state;

  caml_gc_log ("trying to free old minor heap: %"
        ARCH_SIZET_PRINTF_FORMAT "uk words",
        domain_state->minor_heap_wsz / 1024);

  check_minor_heap();

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit(
      (void*)domain_self->minor_heap_area_start,
      domain_state->minor_heap_wsz);

  domain_state->young_start =
    domain_state->young_end =
    domain_state->young_ptr = NULL;
  atomic_store_rel(&domain_state->young_limit,
                   (uintnat) domain_state->young_start);
}

static int allocate_minor_heap(asize_t wsize) {
  caml_domain_state* domain_state = Caml_state;

  check_minor_heap();

  wsize = caml_norm_minor_heap_size(wsize);

  CAMLassert (wsize <= caml_minor_heap_max_wsz);

  caml_gc_log ("trying to allocate minor heap: %"
               ARCH_SIZET_PRINTF_FORMAT "uk words", wsize / 1024);

  if (!caml_mem_commit(
          (void*)domain_self->minor_heap_area_start, Bsize_wsize(wsize))) {
    return -1;
  }

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)domain_self->minor_heap_area_start;
    for (;
      p < (uintnat*)(domain_self->minor_heap_area_start + Bsize_wsize(wsize));
      p++) {
      *p = Debug_free_minor;
    }
  }
#endif

  domain_state->minor_heap_wsz = wsize;

  domain_state->young_start = (value*)domain_self->minor_heap_area_start;
  domain_state->young_end =
      (value*)(domain_self->minor_heap_area_start + Bsize_wsize(wsize));
  domain_state->young_ptr = domain_state->young_end;
  caml_reset_young_limit(domain_state);

  check_minor_heap();
  return 0;
}

int caml_reallocate_minor_heap(asize_t wsize)
{
  free_minor_heap();
  return allocate_minor_heap(wsize);
}

/* This variable is owned by [all_domains_lock]. */
static uintnat next_domain_unique_id = 0;

/* Precondition: you must own [all_domains_lock].

   Specification:
   - returns 0 on the first call
     (we want the main domain to have unique_id 0)
   - returns distinct ids unless there is an overflow
   - never returns 0 again, even in presence of overflow.
 */
static uintnat fresh_domain_unique_id(void) {
    uintnat next = next_domain_unique_id++;

    /* On 32-bit systems, there is a risk of wraparound of the unique
       id counter. We have decided to let that happen and live with
       it, but we still ensure that id 0 is not reused, to avoid
       having new domains believe that they are the main domain. */
    if (next_domain_unique_id == 0)
      next_domain_unique_id++;

    return next;
}

/* must be run on the domain's thread */
static void create_domain(uintnat initial_minor_heap_wsize) {
  dom_internal* d = 0;
  caml_domain_state* domain_state;
  struct interruptor* s;
  uintnat stack_wsize = caml_get_init_stack_wsize();

  CAMLassert (domain_self == 0);

  /* take the all_domains_lock so that we can alter the STW participant
     set atomically */
  caml_plat_lock(&all_domains_lock);

  /* Wait until any in-progress STW sections end. */
  while (atomic_load_acq(&stw_leader)) {
    /* [caml_plat_wait] releases [all_domains_lock] until the current
       STW section ends, and then takes the lock again. */
    caml_plat_wait(&all_domains_cond);
  }

  d = next_free_domain();

  if (d == NULL)
    goto domain_init_complete;

  s = &d->interruptor;
  CAMLassert(!s->running);
  CAMLassert(!s->interrupt_pending);

  domain_self = d;

  /* If the chosen domain slot has not been previously used, allocate a fresh
     domain state. Otherwise, reuse it.

     Reusing the slot ensures that the GC stats are not lost:
     - Heap stats are moved to the free list on domain termination,
       so we don't reuse those stats (caml_init_shared_heap will reset them)
     - But currently there is no orphaning process for allocation stats,
       we just reuse the previous stats from the previous domain
       with the same index.
  */
  if (d->state == NULL) {
    /* FIXME: Never freed. Not clear when to. */
    domain_state = (caml_domain_state*)
      caml_stat_calloc_noexc(1, sizeof(caml_domain_state));
    if (domain_state == NULL)
      goto domain_init_complete;
    d->state = domain_state;
  } else {
    domain_state = d->state;
  }

  SET_Caml_state((void*)domain_state);

  s->unique_id = fresh_domain_unique_id();
  s->interrupt_word = &domain_state->young_limit;
  s->running = 1;
  atomic_fetch_add(&caml_num_domains_running, 1);

  caml_plat_lock(&d->domain_lock);

  domain_state->id = d->id;
  domain_state->unique_id = d->interruptor.unique_id;
  CAMLassert(!d->interruptor.interrupt_pending);

  domain_state->extra_heap_resources = 0.0;
  domain_state->extra_heap_resources_minor = 0.0;

  domain_state->dependent_size = 0;
  domain_state->dependent_allocated = 0;

  /* the minor heap will be initialized by
     [caml_reallocate_minor_heap] below. */
  domain_state->young_start =
    domain_state->young_end =
    domain_state->young_ptr = NULL;

  domain_state->minor_tables = caml_alloc_minor_tables();
  if(domain_state->minor_tables == NULL) {
    goto alloc_minor_tables_failure;
  }

  d->state->shared_heap = caml_init_shared_heap();
  if(d->state->shared_heap == NULL) {
    goto init_shared_heap_failure;
  }

  if (caml_init_major_gc(domain_state) < 0) {
    goto init_major_gc_failure;
  }

  if(caml_reallocate_minor_heap(initial_minor_heap_wsize) < 0) {
    goto reallocate_minor_heap_failure;
  }

  domain_state->dls_root = Val_unit;
  caml_register_generational_global_root(&domain_state->dls_root);

  domain_state->stack_cache = caml_alloc_stack_cache();
  if(domain_state->stack_cache == NULL) {
    goto create_stack_cache_failure;
  }

  domain_state->extern_state = NULL;

  domain_state->intern_state = NULL;

  domain_state->current_stack =
      caml_alloc_main_stack(stack_wsize);
  if(domain_state->current_stack == NULL) {
    goto alloc_main_stack_failure;
  }

  domain_state->c_stack = NULL;
  domain_state->exn_handler = NULL;

  domain_state->action_pending = 0;

  domain_state->gc_regs_buckets = NULL;
  domain_state->gc_regs = NULL;

  domain_state->allocated_words = 0;
  domain_state->swept_words = 0;

  domain_state->local_roots = NULL;

  domain_state->backtrace_buffer = NULL;
  domain_state->backtrace_last_exn = Val_unit;
  domain_state->backtrace_active = 0;
  caml_register_generational_global_root(&domain_state->backtrace_last_exn);

  domain_state->compare_unordered = 0;
  domain_state->oo_next_id_local = 0;

  domain_state->requested_major_slice = 0;
  domain_state->requested_minor_gc = 0;
  domain_state->requested_external_interrupt = 0;

  domain_state->parser_trace = 0;

  if (caml_params->backtrace_enabled) {
    caml_record_backtraces(1);
  }

#ifndef NATIVE_CODE
  domain_state->external_raise = NULL;
  domain_state->trap_sp_off = 1;
  domain_state->trap_barrier_off = 0;
  domain_state->trap_barrier_block = -1;
#endif

  caml_reset_young_limit(domain_state);

  add_to_stw_domains(domain_self);
  goto domain_init_complete;

alloc_main_stack_failure:
create_stack_cache_failure:
  caml_remove_generational_global_root(&domain_state->dls_root);
reallocate_minor_heap_failure:
  caml_teardown_major_gc();
init_major_gc_failure:
  caml_teardown_shared_heap(d->state->shared_heap);
init_shared_heap_failure:
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = NULL;
alloc_minor_tables_failure:
  domain_self = NULL;


domain_init_complete:
  caml_gc_log("domain init complete");
  caml_plat_unlock(&all_domains_lock);
}

CAMLexport void caml_reset_domain_lock(void)
{
  dom_internal* self = domain_self;
  // This is only used to reset the domain_lock state on fork.
  caml_plat_mutex_init(&self->domain_lock);
  caml_plat_cond_init(&self->domain_cond, &self->domain_lock);

  return;
}

/* minor heap initialization and resizing */

static void reserve_minor_heaps() {
  void* heaps_base;
  uintnat minor_heap_reservation_bsize;
  uintnat minor_heap_max_bsz;

  CAMLassert (caml_mem_round_up_pages(Bsize_wsize(caml_minor_heap_max_wsz))
          == Bsize_wsize(caml_minor_heap_max_wsz));

  minor_heap_max_bsz = (uintnat)Bsize_wsize(caml_minor_heap_max_wsz);
  minor_heap_reservation_bsize = minor_heap_max_bsz * Max_domains;

  /* reserve memory space for minor heaps */
  heaps_base = caml_mem_map(minor_heap_reservation_bsize, caml_sys_pagesize,
                1 /* reserve_only */);
  if (heaps_base == NULL)
    caml_fatal_error("Not enough heap memory to reserve minor heaps");

  caml_minor_heaps_start = (uintnat) heaps_base;
  caml_minor_heaps_end = (uintnat) heaps_base + minor_heap_reservation_bsize;

  caml_gc_log("new minor heap reserved from %p to %p",
              (value*)caml_minor_heaps_start, (value*)caml_minor_heaps_end);

  for (int i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    uintnat domain_minor_heap_area = caml_minor_heaps_start +
      minor_heap_max_bsz * (uintnat)i;

    dom->minor_heap_area_start = domain_minor_heap_area;
    dom->minor_heap_area_end =
         domain_minor_heap_area + minor_heap_max_bsz;

    CAMLassert(dom->minor_heap_area_end <= caml_minor_heaps_end);
  }
}

static void unreserve_minor_heaps() {
  uintnat size;

  caml_gc_log("unreserve_minor_heaps");

  for (int i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    CAMLassert(
      /* this domain is not running */
      !dom->interruptor.running
      || (
        /* or its minor heap must already be uninitialized */
        dom->state != NULL
        && dom->state->young_start == NULL
        && dom->state->young_end == NULL
      ));
    /* Note: interruptor.running does not guarantee that dom->state is
       correctly initialized, but domain initialization cannot run
       concurrently with STW sections so we cannot observe partial
       initialization states. */

    /* uninitialize the minor heap area */
    dom->minor_heap_area_start = dom->minor_heap_area_end = 0;
  }

  size = caml_minor_heaps_end - caml_minor_heaps_start;
  CAMLassert (Bsize_wsize(caml_minor_heap_max_wsz) * Max_domains == size);
  caml_mem_unmap((void *) caml_minor_heaps_start, size);
}

static void stw_resize_minor_heap_reservation(caml_domain_state* domain,
                                       void* minor_wsz_data,
                                       int participating_count,
                                       caml_domain_state** participating) {
  barrier_status b;
  uintnat new_minor_wsz = (uintnat) minor_wsz_data;

  caml_gc_log("stw_resize_minor_heap_reservation: "
              "caml_empty_minor_heap_no_major_slice_from_stw");
  caml_empty_minor_heap_no_major_slice_from_stw(domain, NULL,
                                            participating_count, participating);

  caml_gc_log("stw_resize_minor_heap_reservation: free_minor_heap");
  free_minor_heap();

  b = caml_global_barrier_begin ();
  if (caml_global_barrier_is_final(b)) {
    CAML_EV_BEGIN(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
    caml_gc_log("stw_resize_minor_heap_reservation: "
                "unreserve_minor_heaps");

    unreserve_minor_heaps();
    /* new_minor_wsz is page-aligned because caml_norm_minor_heap_size has
       been called to normalize it earlier.
    */
    caml_minor_heap_max_wsz = new_minor_wsz;
    caml_gc_log("stw_resize_minor_heap_reservation: reserve_minor_heaps");
    reserve_minor_heaps();
    /* The call to [reserve_minor_heaps] makes a new reservation,
       and it also updates the reservation boundaries of each domain
       by mutating its [minor_heap_area_start{,_end}] variables.

       Thse variables are synchronized by the fact that we are inside
       a STW section: no other domains are running in parallel, and
       the participating domains will synchronize with this write by
       exiting the barrier, before they read those variables in
       [allocate_minor_heap] below. */
    CAML_EV_END(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
  }
  caml_global_barrier_end(b);

  caml_gc_log("stw_resize_minor_heap_reservation: "
              "allocate_minor_heap");
  /* Note: each domain allocates its own minor heap. This seems
     important to get good NUMA behavior. We don't want a single
     domain to allocate all minor heaps, which could create locality
     issues we don't understand very well. */
  if (allocate_minor_heap(Caml_state->minor_heap_wsz) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap");
  }
}

void caml_update_minor_heap_max(uintnat requested_wsz) {
  caml_gc_log("Changing heap_max_wsz from %" ARCH_INTNAT_PRINTF_FORMAT
              "u to %" ARCH_INTNAT_PRINTF_FORMAT "u.",
              caml_minor_heap_max_wsz, requested_wsz);
  while (requested_wsz > caml_minor_heap_max_wsz) {
    caml_try_run_on_all_domains(
      &stw_resize_minor_heap_reservation, (void*)requested_wsz, 0);
  }
  check_minor_heap();
}

void caml_init_domains(uintnat minor_heap_wsz) {
  int i;

  reserve_minor_heaps();
  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    stw_domains.domains[i] = dom;

    dom->id = i;

    dom->interruptor.interrupt_word = 0;
    caml_plat_mutex_init(&dom->interruptor.lock);
    caml_plat_cond_init(&dom->interruptor.cond,
                        &dom->interruptor.lock);
    dom->interruptor.running = 0;
    dom->interruptor.terminating = 0;
    dom->interruptor.unique_id = 0;
    dom->interruptor.interrupt_pending = 0;

    caml_plat_mutex_init(&dom->domain_lock);
    caml_plat_cond_init(&dom->domain_cond, &dom->domain_lock);
    dom->backup_thread_running = 0;
    dom->backup_thread_msg = BT_INIT;
  }

  create_domain(minor_heap_wsz);
  if (!domain_self) caml_fatal_error("Failed to create main domain");
  CAMLassert (domain_self->state->unique_id == 0);

  caml_init_signal_handling();
}

void caml_init_domain_self(int domain_id) {
  CAMLassert (domain_id >= 0 && domain_id < Max_domains);
  domain_self = &all_domains[domain_id];
  SET_Caml_state(domain_self->state);
}

enum domain_status { Dom_starting, Dom_started, Dom_failed };

struct domain_ml_values {
  value callback;
  value term_mutex;
};

static void init_domain_ml_values(struct domain_ml_values* ml_values,
                                  value callback, value term_mutex)
{
  ml_values->callback = callback;
  ml_values->term_mutex = term_mutex;
  caml_register_generational_global_root(&ml_values->callback);
  caml_register_generational_global_root(&ml_values->term_mutex);
}

static void free_domain_ml_values(struct domain_ml_values* ml_values) {
  caml_remove_generational_global_root(&ml_values->callback);
  caml_remove_generational_global_root(&ml_values->term_mutex);
  caml_stat_free(ml_values);
}

/* This is the structure of the data exchanged between the parent
   domain and child domain during domain_spawn. Some fields are 'in'
   parameters, passed from the parent to the child, others are 'out'
   parameters returned to the parent by the child.
*/
struct domain_startup_params {
  struct interruptor* parent; /* in */
  enum domain_status status; /* in+out:
                                parent and child synchronize on this value. */
  struct domain_ml_values* ml_values; /* in */
  dom_internal* newdom; /* out */
  uintnat unique_id; /* out */
#ifndef _WIN32
  /* signal mask to set after it is safe to do so */
  sigset_t* mask; /* in */
#endif
};

static void* backup_thread_func(void* v)
{
  dom_internal* di = (dom_internal*)v;
  uintnat msg;
  struct interruptor* s = &di->interruptor;

  domain_self = di;
  SET_Caml_state((void*)(di->state));

  msg = atomic_load_acq (&di->backup_thread_msg);
  while (msg != BT_TERMINATE) {
    CAMLassert (msg <= BT_TERMINATE);
    switch (msg) {
      case BT_IN_BLOCKING_SECTION:
        /* Handle interrupts on behalf of the main thread:
         *  - must hold domain_lock to handle interrupts
         *  - need to guarantee no blocking so that backup thread
         *    can be signalled from caml_leave_blocking_section
         */
        if (caml_incoming_interrupts_queued()) {
          if (caml_plat_try_lock(&di->domain_lock)) {
            caml_handle_incoming_interrupts();
            caml_plat_unlock(&di->domain_lock);
          }
        }
        /* Wait safely if there is nothing to do.
         * Will be woken from caml_leave_blocking_section
         */
        caml_plat_lock(&s->lock);
        msg = atomic_load_acq (&di->backup_thread_msg);
        if (msg == BT_IN_BLOCKING_SECTION &&
            !caml_incoming_interrupts_queued())
          caml_plat_wait(&s->cond);
        caml_plat_unlock(&s->lock);
        break;
      case BT_ENTERING_OCAML:
        /* Main thread wants to enter OCaml
         * Will be woken from caml_bt_exit_ocaml
         * or domain_terminate
         */
        caml_plat_lock(&di->domain_lock);
        msg = atomic_load_acq (&di->backup_thread_msg);
        if (msg == BT_ENTERING_OCAML)
          caml_plat_wait(&di->domain_cond);
        caml_plat_unlock(&di->domain_lock);
        break;
      default:
        cpu_relax();
        break;
    };
    msg = atomic_load_acq (&di->backup_thread_msg);
  }

  /* doing terminate */
  atomic_store_rel(&di->backup_thread_msg, BT_INIT);

  return 0;
}

static void install_backup_thread (dom_internal* di)
{
  int err;
#ifndef _WIN32
  sigset_t mask, old_mask;
#endif

  if (di->backup_thread_running == 0) {
    CAMLassert (di->backup_thread_msg == BT_INIT || /* Using fresh domain */
            di->backup_thread_msg == BT_TERMINATE); /* Reusing domain */

    while (atomic_load_acq(&di->backup_thread_msg) != BT_INIT) {
      /* Give a chance for backup thread on this domain to terminate */
      caml_plat_unlock (&di->domain_lock);
      cpu_relax ();
      caml_plat_lock (&di->domain_lock);
    }

#ifndef _WIN32
    /* No signals on the backup thread */
    sigfillset(&mask);
    pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

    atomic_store_rel(&di->backup_thread_msg, BT_ENTERING_OCAML);
    err = pthread_create(&di->backup_thread, 0, backup_thread_func, (void*)di);

#ifndef _WIN32
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

    if (err)
      caml_failwith("failed to create domain backup thread");
    di->backup_thread_running = 1;
    pthread_detach(di->backup_thread);
  }
}

static void caml_domain_stop_default(void)
{
  return;
}

static void caml_domain_external_interrupt_hook_default(void)
{
  return;
}

CAMLexport void (*caml_domain_stop_hook)(void) =
   caml_domain_stop_default;

CAMLexport void (*caml_domain_external_interrupt_hook)(void) =
   caml_domain_external_interrupt_hook_default;

CAMLexport _Atomic caml_timing_hook caml_domain_terminated_hook =
  (caml_timing_hook)NULL;

static void domain_terminate();

static void* domain_thread_func(void* v)
{
  struct domain_startup_params* p = v;
  struct domain_ml_values *ml_values = p->ml_values;
#ifndef _WIN32
  sigset_t mask = *(p->mask);
#endif
  void * signal_stack;

  signal_stack = caml_init_signal_stack();
  if (signal_stack == NULL) {
    caml_gc_log("Failed to create domain: signal stack");
    return 0;
  }

  create_domain(caml_params->init_minor_heap_wsz);
  /* this domain is now part of the STW participant set */
  p->newdom = domain_self;

  /* handshake with the parent domain */
  caml_plat_lock(&p->parent->lock);
  if (domain_self) {
    p->status = Dom_started;
    p->unique_id = domain_self->interruptor.unique_id;
  } else {
    p->status = Dom_failed;
  }
  caml_plat_broadcast(&p->parent->cond);
  caml_plat_unlock(&p->parent->lock);
  /* Cannot access p below here. */

  if (domain_self) {
    install_backup_thread(domain_self);

#ifndef _WIN32
    /* It is now safe for us to handle signals */
    pthread_sigmask(SIG_SETMASK, &mask, NULL);
#endif

    caml_gc_log("Domain starting (unique_id = %"ARCH_INTNAT_PRINTF_FORMAT"u)",
                domain_self->interruptor.unique_id);
    CAML_EV_LIFECYCLE(EV_DOMAIN_SPAWN, getpid());
    caml_callback(ml_values->callback, Val_unit);
    domain_terminate();

    /* This domain currently holds the [term_mutex], and has signaled all the
       waiting domains to be woken up. We unlock the [term_mutex] to release
       the joining domains. The unlock is done after [domain_terminate] to
       ensure that this domain has released all of its runtime state. */
    caml_mutex_unlock(Mutex_val(ml_values->term_mutex));

    /* [ml_values] must be freed after unlocking [term_mutex]. This ensures
       that [term_mutex] is only removed from the root set after [term_mutex]
       is unlocked. Otherwise, there is a risk of [term_mutex] being destroyed
       by [caml_mutex_finalize] finaliser while it remains locked, leading to
       undefined behaviour. */
    free_domain_ml_values(ml_values);
  } else {
    caml_gc_log("Failed to create domain");
  }
  caml_free_signal_stack(signal_stack);
  return 0;
}

CAMLprim value caml_domain_spawn(value callback, value mutex)
{
  CAMLparam2 (callback, mutex);
  struct domain_startup_params p;
  pthread_t th;
  int err;
#ifndef _WIN32
  sigset_t mask, old_mask;
#endif

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support spawning multiple domains");
#endif
  p.parent = &domain_self->interruptor;
  p.status = Dom_starting;

  p.ml_values =
      (struct domain_ml_values*) caml_stat_alloc_noexc(
                                    sizeof(struct domain_ml_values));
  if (!p.ml_values) {
    caml_failwith("failed to create ml values for domain thread");
  }
  init_domain_ml_values(p.ml_values, callback, mutex);

/* We block all signals while we spawn the new domain. This is because
   pthread_create inherits the current signals set, and we want to avoid a
   signal handler being triggered in the new domain before the domain_state is
   fully populated. */
#ifndef _WIN32
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
  p.mask = &old_mask;
#endif
  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
#ifndef _WIN32
  /* We can restore the signal mask we had initially now. */
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  if (err) {
    caml_failwith("failed to create domain thread");
  }

  /* While waiting for the child thread to start up, we need to service any
     stop-the-world requests as they come in. */
  caml_plat_lock(&domain_self->interruptor.lock);
  while (p.status == Dom_starting) {
    if (caml_incoming_interrupts_queued()) {
      caml_plat_unlock(&domain_self->interruptor.lock);
      handle_incoming(&domain_self->interruptor);
      caml_plat_lock(&domain_self->interruptor.lock);
    } else {
      caml_plat_wait(&domain_self->interruptor.cond);
    }
  }
  caml_plat_unlock(&domain_self->interruptor.lock);

  if (p.status == Dom_started) {
    /* successfully created a domain.
       p.ml_values is now owned by that domain */
    pthread_detach(th);
  } else {
    CAMLassert (p.status == Dom_failed);
    /* failed */
    pthread_join(th, 0);
    free_domain_ml_values(p.ml_values);
    caml_failwith("failed to allocate domain");
  }
  /* When domain 0 first spawns a domain, the backup thread is not active, we
     ensure it is started here. */
  install_backup_thread(domain_self);

  CAMLreturn (Val_long(p.unique_id));
}

CAMLprim value caml_ml_domain_id(value unit)
{
  CAMLnoalloc;
  return Val_long(domain_self->interruptor.unique_id);
}

CAMLprim value caml_ml_domain_unique_token (value unit)
{
  return Val_unit;
}

/* sense-reversing barrier */
#define BARRIER_SENSE_BIT 0x100000

barrier_status caml_global_barrier_begin(void)
{
  uintnat b = 1 + atomic_fetch_add(&stw_request.barrier, 1);
  return b;
}

int caml_global_barrier_is_final(barrier_status b)
{
  return ((b & ~BARRIER_SENSE_BIT) == stw_request.num_domains);
}

void caml_global_barrier_end(barrier_status b)
{
  uintnat sense = b & BARRIER_SENSE_BIT;
  if (caml_global_barrier_is_final(b)) {
    /* last domain into the barrier, flip sense */
    atomic_store_rel(&stw_request.barrier, sense ^ BARRIER_SENSE_BIT);
  } else {
    /* wait until another domain flips the sense */
    SPIN_WAIT {
      uintnat barrier = atomic_load_acq(&stw_request.barrier);
      if ((barrier & BARRIER_SENSE_BIT) != sense) break;
    }
  }
}

void caml_global_barrier(void)
{
  barrier_status b = caml_global_barrier_begin();
  caml_global_barrier_end(b);
}

int caml_global_barrier_num_domains(void)
{
  return stw_request.num_domains;
}

static void decrement_stw_domains_still_processing(void)
{
  /* we check if we are the last to leave a stw section
     if so, clear the stw_leader to allow the new stw sections to start.
   */
  intnat am_last =
      atomic_fetch_add(&stw_request.num_domains_still_processing, -1) == 1;

  if( am_last ) {
    /* release the STW lock to allow new STW sections */
    caml_plat_lock(&all_domains_lock);
    atomic_store_rel(&stw_leader, 0);
    caml_plat_broadcast(&all_domains_cond);
    caml_gc_log("clearing stw leader");
    caml_plat_unlock(&all_domains_lock);
  }
}

static void stw_handler(caml_domain_state* domain)
{
  CAML_EV_BEGIN(EV_STW_HANDLER);
  CAML_EV_BEGIN(EV_STW_API_BARRIER);
  {
    SPIN_WAIT {
      if (atomic_load_acq(&stw_request.domains_still_running) == 0)
        break;

      if (stw_request.enter_spin_callback)
        stw_request.enter_spin_callback(domain, stw_request.enter_spin_data);
    }
  }
  CAML_EV_END(EV_STW_API_BARRIER);

  #ifdef DEBUG
  Caml_state->inside_stw_handler = 1;
  #endif
  stw_request.callback(
      domain,
      stw_request.data,
      stw_request.num_domains,
      stw_request.participating);
  #ifdef DEBUG
  Caml_state->inside_stw_handler = 0;
  #endif

  decrement_stw_domains_still_processing();

  CAML_EV_END(EV_STW_HANDLER);

  /* poll the GC to check for deferred work
     we do this here because blocking or waiting threads only execute
     the interrupt handler and do not poll for deferred work*/
  caml_poll_gc_work();
}


#ifdef DEBUG
int caml_domain_is_in_stw(void) {
  return Caml_state->inside_stw_handler;
}
#endif

/* During a stop-the-world (STW), all currently running domains stop
   their usual work and synchronize to all call the same function.

   STW sections use [all_domains_lock] and the variable [stw_leader]
   (0 when no STW section is running, the dom_internal* pointer of the
   STW leader when a STW section is running) to guarantee that no
   domain is running something else:

   - If two STW sections are attempted in parallel, only one will
     manage to take the lock, and the domain starting the other will
     join that winning STW section, without running its own STW
     callback at all. (This is the [_try_] in the function name: if it
     returns 0, the STW section did not run at all, so you should call
     this function in a loop.)

   - Domain initialization code from [create_domain] will not run in
     parallel with a STW section, as [create_domain] starts by
     looping until (1) it has the [all_domains_lock] and (2) there is
     no current STW section (using the [stw_leader] variable).

   - Domain cleanup code runs after the terminating domain may run in
     parallel to a STW section, but only after that domain has safely
     removed itself from the STW participant set: the
     [domain_terminate] function is careful to only leave the STW set
     when (1) it has the [all_domains_lock] and (2) it hasn't received
     any request to participate in a STW section.

   Each domain leaves the section as soon as it is finished running
   the STW section callback. In particular, a mutator may resume while
   some other domains are still in the section. Any code within the STW
   callback that needs to happen before any mutator must be followed
   by a barrier, forcing all STW participants to synchronize.

   Taken together, these properties guarantee that STW sections act as
   a proper exclusion mechanism: for example, some mutable state
   global to all domains can be "protected by STW" if it is only
   mutated within STW section, with a barrier before the next
   read. Such state can be safely updated by domain initialization,
   but additional synchronization would be required to update it
   during domain cleanup.

   Note: in the case of both [create_domain] and [domain_terminate] it
   is important that the loops (waiting for STW sections to finish)
   regularly release [all_domains_lock], to avoid deadlocks scenario
   with in-progress STW sections.
    - For [domain_terminate] we release the lock and join
      the STW section before resuming.
    - For [create_domain] we wait until the end of the section using
      the condition variable [all_domains_cond] over
      [all_domains_lock], which is broadcasted when a STW section
      finishes.
   The same logic would apply for any other situations in which a domain
   wants to join or leave the set of STW participants.
*/
int caml_try_run_on_all_domains_with_spin_work(
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*),
  void (*enter_spin_callback)(caml_domain_state*, void*),
  void* enter_spin_data)
{
  int i;
  caml_domain_state* domain_state = domain_self->state;

  caml_gc_log("requesting STW");

  /* Don't touch the lock if there's already a stw leader
     OR we can't get the lock.

     Note: this read on [stw_leader] is an optimization, giving up
     faster (before trying to take the lock) in contended
     situations. Without this read, [stw_leader] would be protected by
     [all_domains_lock] and could be a non-atomic variable.
  */
  if (atomic_load_acq(&stw_leader) ||
      !caml_plat_try_lock(&all_domains_lock)) {
    caml_handle_incoming_interrupts();
    return 0;
  }

  /* see if there is a stw_leader already */
  if (atomic_load_acq(&stw_leader)) {
    caml_plat_unlock(&all_domains_lock);
    caml_handle_incoming_interrupts();
    return 0;
  }

  /* we have the lock and can claim the stw_leader */
  atomic_store_rel(&stw_leader, (uintnat)domain_self);

  CAML_EV_BEGIN(EV_STW_LEADER);
  caml_gc_log("causing STW");

  /* setup all fields for this stw_request, must have those needed
     for domains waiting at the enter spin barrier */
  stw_request.enter_spin_callback = enter_spin_callback;
  stw_request.enter_spin_data = enter_spin_data;
  stw_request.callback = handler;
  stw_request.data = data;
  atomic_store_rel(&stw_request.barrier, 0);
  atomic_store_rel(&stw_request.domains_still_running, 1);
  stw_request.num_domains = stw_domains.participating_domains;
  atomic_store_rel(&stw_request.num_domains_still_processing,
                     stw_domains.participating_domains);

  if( leader_setup ) {
    leader_setup(domain_state);
  }

#ifdef DEBUG
  {
    int domains_participating = 0;
    for(i=0; i<Max_domains; i++) {
      if(all_domains[i].interruptor.running)
        domains_participating++;
    }
    CAMLassert(domains_participating == stw_domains.participating_domains);
    CAMLassert(domains_participating > 0);
  }
#endif

  /* Next, interrupt all domains */
  for(i = 0; i < stw_domains.participating_domains; i++) {
    dom_internal * d = stw_domains.domains[i];
    stw_request.participating[i] = d->state;
    CAMLassert(!d->interruptor.interrupt_pending);
    if (d->state != domain_state) caml_send_interrupt(&d->interruptor);
  }

  /* Domains now know they are part of the STW.

     Note: releasing the lock will not allow new domain to be created
     in parallel with the rest of the STW section, as new domains
     follow the protocol of waiting on [all_domains_cond] which is
     only broadcast at the end of the STW section.

     The reason we use a condition variable [all_domains_cond] instead
     of just holding the lock until the end of the STW section is that
     the last domain to exit the section (and broadcast the condition)
     is not necessarily the same as the domain starting the section
     (and taking the lock) -- whereas POSIX mutexes must be unlocked
     by the same thread that locked them.
  */
  caml_plat_unlock(&all_domains_lock);

  for(i = 0; i < stw_request.num_domains; i++) {
    int id = stw_request.participating[i]->id;
    caml_wait_interrupt_serviced(&all_domains[id].interruptor);
  }

  /* release from the enter barrier */
  atomic_store_rel(&stw_request.domains_still_running, 0);

  #ifdef DEBUG
  domain_state->inside_stw_handler = 1;
  #endif
  handler(domain_state, data,
          stw_request.num_domains, stw_request.participating);
  #ifdef DEBUG
  domain_state->inside_stw_handler = 0;
  #endif

  /* Note: the last domain passing through this function will
     temporarily take [all_domains_lock] again and use it to broadcast
     [all_domains_cond], waking up any domain waiting to be created. */
  decrement_stw_domains_still_processing();

  CAML_EV_END(EV_STW_LEADER);

  return 1;
}

int caml_try_run_on_all_domains(
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*))
{
  return
      caml_try_run_on_all_domains_with_spin_work(handler,
                                                 data,
                                                 leader_setup, 0, 0);
}

void caml_interrupt_self(void) {
  interrupt_domain(&domain_self->interruptor);
}

void caml_reset_young_limit(caml_domain_state * dom_st)
{
  /* An interrupt might have been queued in the meanwhile; this
     achieves the proper synchronisation. */
  atomic_exchange(&dom_st->young_limit, (uintnat)dom_st->young_start);
  dom_internal * d = &all_domains[dom_st->id];
  if (atomic_load_relaxed(&d->interruptor.interrupt_pending)
      || dom_st->requested_minor_gc
      || dom_st->requested_major_slice
      || atomic_load_relaxed(&dom_st->requested_external_interrupt)
      || dom_st->action_pending) {
    atomic_store_rel(&dom_st->young_limit, (uintnat)-1);
    CAMLassert(caml_check_gc_interrupt(dom_st));
  }
}

void caml_poll_gc_work(void)
{
  CAMLalloc_point_here;

  if (((uintnat)Caml_state->young_ptr - Bhsize_wosize(Max_young_wosize) <
       (uintnat)Caml_state->young_start) ||
      Caml_state->requested_minor_gc) {
    /* out of minor heap or collection forced */
    Caml_state->requested_minor_gc = 0;
    caml_empty_minor_heaps_once();
  }

  if (Caml_state->requested_major_slice) {
    CAML_EV_BEGIN(EV_MAJOR);
    Caml_state->requested_major_slice = 0;
    caml_major_collection_slice(AUTO_TRIGGERED_MAJOR_SLICE);
    CAML_EV_END(EV_MAJOR);
  }

  if (atomic_load_acq(&Caml_state->requested_external_interrupt)) {
    caml_domain_external_interrupt_hook();
  }
  caml_reset_young_limit(Caml_state);
}

void caml_handle_gc_interrupt(void)
{
  CAMLalloc_point_here;

  if (caml_incoming_interrupts_queued()) {
    /* interrupt */
    CAML_EV_BEGIN(EV_INTERRUPT_REMOTE);
    caml_handle_incoming_interrupts();
    CAML_EV_END(EV_INTERRUPT_REMOTE);
  }

  caml_poll_gc_work();
}

CAMLexport int caml_bt_is_in_blocking_section(void)
{
  uintnat status = atomic_load_acq(&domain_self->backup_thread_msg);
  return status == BT_IN_BLOCKING_SECTION;
}

CAMLexport int caml_bt_is_self(void)
{
  return pthread_equal(domain_self->backup_thread, pthread_self());
}

CAMLexport intnat caml_domain_is_multicore (void)
{
  dom_internal *self = domain_self;
  return (!caml_domain_alone() || self->backup_thread_running);
}

CAMLexport void caml_acquire_domain_lock(void)
{
  dom_internal* self = domain_self;
  caml_plat_lock(&self->domain_lock);
  SET_Caml_state(self->state);
}

CAMLexport void caml_bt_enter_ocaml(void)
{
  dom_internal* self = domain_self;

  CAMLassert(caml_domain_alone() || self->backup_thread_running);

  if (self->backup_thread_running) {
    atomic_store_rel(&self->backup_thread_msg, BT_ENTERING_OCAML);
  }
}

CAMLexport void caml_release_domain_lock(void)
{
  dom_internal* self = domain_self;
  SET_Caml_state(NULL);
  caml_plat_unlock(&self->domain_lock);
}

CAMLexport void caml_bt_exit_ocaml(void)
{
  dom_internal* self = domain_self;

  CAMLassert(caml_domain_alone() || self->backup_thread_running);

  if (self->backup_thread_running) {
    atomic_store_rel(&self->backup_thread_msg, BT_IN_BLOCKING_SECTION);
    /* Wakeup backup thread if it is sleeping */
    caml_plat_signal(&self->domain_cond);
  }
}

/* default handler for unix_fork, will be called by unix_fork. */
static void caml_atfork_default(void) {
  caml_reset_domain_lock();
  caml_acquire_domain_lock();
}

CAMLexport void (*caml_atfork_hook)(void) = caml_atfork_default;

static void handover_ephemerons(caml_domain_state* domain_state)
{
  if (domain_state->ephe_info->todo == 0 &&
      domain_state->ephe_info->live == 0)
    return;

  caml_add_to_orphaned_ephe_list(domain_state->ephe_info);
  CAMLassert (domain_state->ephe_info->live == 0);
  CAMLassert (domain_state->ephe_info->todo == 0);
}

static void handover_finalisers(caml_domain_state* domain_state)
{
  struct caml_final_info* f = domain_state->final_info;

  if (f->todo_head != NULL || f->first.size != 0 || f->last.size != 0) {
    /* have some final structures */
    if (caml_gc_phase != Phase_sweep_and_mark_main) {
      /* Force a major GC cycle to simplify constraints for
       * handing over finalisers. */
      caml_finish_major_cycle();
      CAMLassert(caml_gc_phase == Phase_sweep_and_mark_main);
    }
    caml_add_orphaned_finalisers (f);
    /* Create a dummy final info */
    domain_state->final_info = caml_alloc_final_info();
  }
  caml_final_domain_terminate(domain_state);
}

int caml_domain_is_terminating (void)
{
  struct interruptor* s = &domain_self->interruptor;
  return s->terminating;
}

static void domain_terminate (void)
{
  caml_domain_state* domain_state = domain_self->state;
  struct interruptor* s = &domain_self->interruptor;
  int finished = 0;
#ifndef _WIN32
  sigset_t mask;
#endif

  caml_gc_log("Domain terminating");
  s->terminating = 1;

#ifndef _WIN32
  /* Block all signals so that signal handlers do not run on this thread */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
#endif

  /* Join ongoing systhreads, if necessary, and then run user-defined
     termination hooks. No OCaml code can run on this domain after
     this. */
  caml_domain_stop_hook();
  call_timing_hook(&caml_domain_terminated_hook);

  while (!finished) {
    caml_orphan_allocated_words();
    caml_finish_sweeping();

    caml_empty_minor_heaps_once();
    /* Note: [caml_empty_minor_heaps_once] will also join any ongoing
       STW sections that has sent an interrupt to this domain. */

    caml_finish_marking();
    handover_ephemerons(domain_state);
    handover_finalisers(domain_state);

    /* take the all_domains_lock to try and exit the STW participant set
       without racing with a STW section being triggered */
    caml_plat_lock(&all_domains_lock);

    /* The interaction of termination and major GC is quite subtle.
     *
     * At the end of the major GC, we decide the number of domains to mark and
     * sweep for the next cycle. If a STW section has been started, it will
     * require this domain to participate, which in turn could involve a
     * major GC cycle. This would then require finish marking and sweeping
     * again in order to decrement the globals [num_domains_to_mark] and
     * [num_domains_to_sweep] (see major_gc.c).
     */

    if (!caml_incoming_interrupts_queued() &&
        domain_state->marking_done &&
        domain_state->sweeping_done) {

      finished = 1;
      s->terminating = 0;
      s->running = 0;

      /* Remove this domain from stw_domains */
      remove_from_stw_domains(domain_self);

      /* signal the interruptor condition variable
       * because the backup thread may be waiting on it
       */
      caml_plat_lock(&s->lock);
      caml_plat_broadcast(&s->cond);
      caml_plat_unlock(&s->lock);

      CAMLassert (domain_self->backup_thread_running);
      domain_self->backup_thread_running = 0;
    }
    caml_plat_unlock(&all_domains_lock);
  }
  /* We can not touch domain_self->interruptor after here
     because it may be reused */
  caml_remove_generational_global_root(&domain_state->dls_root);
  caml_remove_generational_global_root(&domain_state->backtrace_last_exn);
  caml_stat_free(domain_state->final_info);
  caml_stat_free(domain_state->ephe_info);
  caml_free_intern_state();
  caml_free_extern_state();
  caml_teardown_major_gc();
  CAML_EV_LIFECYCLE(EV_DOMAIN_TERMINATE, getpid());

  caml_teardown_shared_heap(domain_state->shared_heap);
  domain_state->shared_heap = 0;
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = 0;

  caml_orphan_alloc_stats(domain_state);
  /* Heap stats were orphaned by caml_teardown_shared_heap above.
     At this point, the stats of the domain must be empty;
     we also clear the sampled copy.

     Note: We cannot call caml_collect_gc_stats_sample to clear the
     sample at this point as the shared heap is gone. */
  caml_clear_gc_stats_sample(domain_state);

  /* TODO: can this ever be NULL? can we remove this check? */
  if(domain_state->current_stack != NULL) {
    caml_free_stack(domain_state->current_stack);
  }
  caml_free_backtrace_buffer(domain_state->backtrace_buffer);
  caml_free_gc_regs_buckets(domain_state->gc_regs_buckets);

  /* signal the domain termination to the backup thread
     NB: for a program with no additional domains, the backup thread
     will not have been started */
  atomic_store_rel(&domain_self->backup_thread_msg, BT_TERMINATE);
  caml_plat_signal(&domain_self->domain_cond);
  caml_plat_unlock(&domain_self->domain_lock);

  caml_plat_assert_all_locks_unlocked();
  /* This is the last thing we do because we need to be able to rely
     on caml_domain_alone (which uses caml_num_domains_running) in at least
     the shared_heap lockfree fast paths */
  atomic_fetch_add(&caml_num_domains_running, -1);
}

CAMLprim value caml_ml_domain_cpu_relax(value t)
{
  struct interruptor* self = &domain_self->interruptor;
  handle_incoming_otherwise_relax (self);
  return Val_unit;
}

CAMLprim value caml_domain_dls_set(value t)
{
  CAMLnoalloc;
  caml_modify_generational_global_root(&Caml_state->dls_root, t);
  return Val_unit;
}

CAMLprim value caml_domain_dls_get(value unused)
{
  CAMLnoalloc;
  return Caml_state->dls_root;
}

CAMLprim value caml_recommended_domain_count(value unused)
{
  intnat n = -1;

#if defined(HAS_GNU_GETAFFINITY_NP) || defined(HAS_BSD_GETAFFINITY_NP)
  cpu_set_t cpuset;

  CPU_ZERO(&cpuset);
  /* error case fallsback into next method */
  if (pthread_getaffinity_np(pthread_self(), sizeof(cpuset), &cpuset) == 0)
    n = CPU_COUNT(&cpuset);
#endif /* HAS_GNU_GETAFFINITY_NP || HAS_BSD_GETAFFINITY_NP */

#ifdef _SC_NPROCESSORS_ONLN
  if (n == -1)
    n = sysconf(_SC_NPROCESSORS_ONLN);
#endif /* _SC_NPROCESSORS_ONLN */

#ifdef _WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  n = sysinfo.dwNumberOfProcessors;
#endif /* _WIN32 */

  /* At least one, even if system says zero */
  if (n <= 0)
    n = 1;
  else if (n > Max_domains)
    n = Max_domains;

  return (Val_long(n));
}
