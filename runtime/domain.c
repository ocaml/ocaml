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
#include <stdbool.h>
#include <stdio.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <pthread.h>
#include <string.h>
#include <assert.h>
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
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
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
#include "caml/memprof.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"
#include "caml/sync.h"
#include "caml/weak.h"

/* Check that the domain_state structure was laid out without padding,
   since the runtime assumes this in computing offsets */
static_assert(
    offsetof(caml_domain_state, LAST_DOMAIN_STATE_MEMBER) ==
    (Domain_state_num_fields - 1) * 8,
    "");

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
   for servicing STW sections between the main thread and the backup
   thread when caml_enter_blocking_section and
   caml_leave_blocking_section are called.

   When the state for the backup thread is BT_IN_BLOCKING_SECTION
   the backup thread will service the STW section.

   The state machine for the backup thread (and its transitions)
   are:

           BT_INIT  <---------------------------------------+
              |                                             |
   (install_backup_thread)                                  |
       [main thread]                                        |
              |                                             |
              v                                             |
       BT_ENTERING_OCAML  <-----------------+               |
              |                             |               |
(caml_enter_blocking_section)               |               |
        [main thread]                       |               |
              |                             |               |
              |                             |               |
              |               (caml_leave_blocking_section) |
              |                       [main thread]         |
              v                             |               |
    BT_IN_BLOCKING_SECTION  ----------------+               |
              |                                             |
     (caml_domain_terminate)                                |
        [main thread]                                       |
              |                                             |
              v                                             |
        BT_TERMINATE                               (backup_thread_func)
              |                                      [backup thread]
              |                                             |
              +---------------------------------------------+

 */
#define BT_IN_BLOCKING_SECTION 0
#define BT_ENTERING_OCAML 1
#define BT_TERMINATE 2
#define BT_INIT 3

/* control of STW interrupts */
struct interruptor {
  /* The outermost atomic is for synchronization with
     caml_interrupt_all_signal_safe. The innermost atomic is also for
     cross-domain communication.*/
  _Atomic(atomic_uintnat *) interrupt_word;
  caml_plat_mutex lock;
  caml_plat_cond cond;

  int running;
  int terminating;
  /* unlike the domain ID, this ID number is not reused */
  uintnat unique_id;

  /* indicates whether there is an interrupt pending */
  atomic_uintnat interrupt_pending;
};

Caml_inline int interruptor_has_pending(struct interruptor *s)
{ return atomic_load_acquire(&s->interrupt_pending) != 0; }
Caml_inline void interruptor_set_handled(struct interruptor *s)
{ atomic_store_release(&s->interrupt_pending, 0); }
Caml_inline void interruptor_set_pending(struct interruptor *s)
{ atomic_store_release(&s->interrupt_pending, 1); }

struct dom_internal {
  /* readonly fields, initialised and never modified */
  int id;
  pthread_t tid;
  caml_domain_state* state;
  struct interruptor interruptor;

  /* backup thread */
  pthread_t backup_thread;
  atomic_uintnat backup_thread_msg;
  caml_plat_mutex domain_lock;
  caml_plat_cond domain_cond;
  bool domain_canceled;

  /* modified only during STW sections */
  uintnat minor_heap_area_start;
  uintnat minor_heap_area_end;
};
typedef struct dom_internal dom_internal;

static struct {
  /* enter barrier for STW sections, participating domains arrive into
     the barrier before executing the STW callback */
  caml_plat_barrier domains_still_running;
  /* the number of domains that have yet to return from the callback */
  atomic_uintnat num_domains_still_processing;
  void (*callback)(caml_domain_state*,
                   void*,
                   int participating_count,
                   caml_domain_state** others_participating);
  void* data;
  int (*enter_spin_callback)(caml_domain_state*, void*);
  void* enter_spin_data;

  /* global_barrier state */
  int num_domains;
  caml_plat_barrier barrier;

  caml_domain_state** participating;
} stw_request = {
  CAML_PLAT_BARRIER_INITIALIZER,
  0,
  NULL,
  NULL,
  NULL,
  NULL,
  0,
  CAML_PLAT_BARRIER_INITIALIZER,
  NULL
};

static caml_plat_mutex all_domains_lock = CAML_PLAT_MUTEX_INITIALIZER;
static caml_plat_cond all_domains_cond = CAML_PLAT_COND_INITIALIZER;
static atomic_uintnat /* dom_internal* */ stw_leader = 0;
static uintnat stw_requests_suspended = 0; /* protected by all_domains_lock */
static caml_plat_cond requests_suspended_cond = CAML_PLAT_COND_INITIALIZER;
static dom_internal* all_domains;
static atomic_intnat domains_exiting = 0;

CAMLexport atomic_uintnat caml_num_domains_running = 0;

/* size of the virtual memory reservation for the minor heap, per domain */
uintnat caml_minor_heap_max_wsz;
/*
  The amount of memory reserved for all minor heaps of all domains is
  caml_params->max_domains * caml_minor_heap_max_wsz. Individual domains can
  allocate smaller minor heaps, but when a domain calls Gc.set to allocate a
  bigger minor heap than this reservation, we perform a new virtual memory
  reservation based on the increased minor heap size.

  New domains are created with a minor heap of size
  caml_params->init_minor_heap_wsz.

  To perform a new virtual memory reservation for the heaps, we stop the world
  and do a minor collection on all domains.
  See [stw_resize_minor_heap_reservation].
*/

CAMLexport uintnat caml_minor_heaps_start;
CAMLexport uintnat caml_minor_heaps_end;
static CAMLthread_local dom_internal* domain_self;

/*
 * This structure is protected by all_domains_lock.
 * [0, participating_domains) are all the domains taking part in STW sections.
 * [participating_domains, caml_params->max_domains) are all those domains free
 * to be used.
 */
static struct {
  int participating_domains;
  dom_internal** domains;
} stw_domains = {
  0,
  NULL
};

static void add_next_to_stw_domains(void)
{
  CAMLassert(stw_domains.participating_domains < caml_params->max_domains);
  stw_domains.participating_domains++;
#ifdef DEBUG
  /* Enforce here the invariant for early-exit in
     [caml_interrupt_all_signal_safe], because the latter must be
     async-signal-safe and one cannot CAMLassert inside it. */
  bool prev_has_interrupt_word = true;
  for (int i = 0; i < caml_params->max_domains; i++) {
    bool has_interrupt_word = all_domains[i].interruptor.interrupt_word != NULL;
    if (i < stw_domains.participating_domains) CAMLassert(has_interrupt_word);
    if (!prev_has_interrupt_word) CAMLassert(!has_interrupt_word);
    prev_has_interrupt_word = has_interrupt_word;
  }
#endif
}

static void remove_from_stw_domains(dom_internal* dom) {
  int i;
  for(i=0; stw_domains.domains[i]!=dom; ++i) {
    CAMLassert(i<caml_params->max_domains);
  }
  CAMLassert(i < stw_domains.participating_domains);

  /* swap passed domain to first free domain */
  stw_domains.participating_domains--;
  stw_domains.domains[i] =
      stw_domains.domains[stw_domains.participating_domains];
  stw_domains.domains[stw_domains.participating_domains] = dom;
}

static dom_internal* next_free_domain(void) {
  if (stw_domains.participating_domains == caml_params->max_domains)
    return NULL;

  CAMLassert(stw_domains.participating_domains < caml_params->max_domains);
  return stw_domains.domains[stw_domains.participating_domains];
}

CAMLexport CAMLthread_local caml_domain_state* caml_state;

#ifndef HAS_FULL_THREAD_VARIABLES
/* Export a getter for caml_state, to be used in DLLs */
CAMLexport caml_domain_state* caml_get_domain_state(void)
{
  return caml_state;
}
#endif

Caml_inline void interrupt_domain(struct interruptor* s)
{
  atomic_uintnat * interrupt_word = atomic_load_relaxed(&s->interrupt_word);
  atomic_store_release(interrupt_word, CAML_UINTNAT_MAX);
}

Caml_inline void interrupt_domain_local(caml_domain_state* dom_st)
{
  atomic_store_relaxed(&dom_st->young_limit, CAML_UINTNAT_MAX);
}

int caml_incoming_interrupts_queued(void)
{
  return interruptor_has_pending(&domain_self->interruptor);
}

static void terminate_backup_thread(dom_internal *di);

static inline bool backup_thread_running(dom_internal *di)
{
    return (atomic_load_acquire(&di->backup_thread_msg) != BT_INIT);
}

/* must NOT be called with s->lock held */
static void stw_handler(caml_domain_state* domain);
static int handle_incoming(struct interruptor* s)
{
  int handled = interruptor_has_pending(s);
  if (handled) {
    CAMLassert (s->running);
    interruptor_set_handled(s);

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
  interruptor_set_pending(target);

  /* Signal the condition variable, in case the target is itself
     waiting for an interrupt to be processed elsewhere, or to wake up
     the backup thread. */
  caml_plat_lock_blocking(&target->lock);
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  interrupt_domain(target);

  return 1;
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
   [caml_minor_heap_max_wsz * caml_params->max_domains]
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
  which is called both at domain-initialization (by [domain_create])
  and if a request comes to change the minor heap size.

  The boundaries of this committed memory area are
     [domain_state->young_start]
   and
     [domain_state->young_end].

  Those [young_{start,end}] variables are never accessed by another
  domain, so they need no synchronization.
*/

Caml_inline void check_minor_heap(void) {
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

static void free_minor_heap(void) {
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
      Bsize_wsize(domain_state->minor_heap_wsz));

  domain_state->young_start   = NULL;
  domain_state->young_end     = NULL;
  domain_state->young_ptr     = NULL;
  domain_state->young_trigger = NULL;
  domain_state->memprof_young_trigger = NULL;
  atomic_store_release(&domain_state->young_limit,
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
  /* Trigger a GC poll when half of the minor heap is filled. At that point, a
   * major slice is scheduled. */
  domain_state->young_trigger = domain_state->young_start
         + (domain_state->young_end - domain_state->young_start) / 2;
  caml_memprof_set_trigger(domain_state);
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
static void domain_create(uintnat initial_minor_heap_wsize,
                          caml_domain_state *parent)
{
  dom_internal* d = 0;
  caml_domain_state* domain_state;
  struct interruptor* s;
  uintnat stack_wsize = caml_get_init_stack_wsize();

  CAMLassert (domain_self == 0);

  /* take the all_domains_lock so that we can alter the STW participant
     set atomically */
  caml_plat_lock_blocking(&all_domains_lock);

  /* How many STW sections we are willing to wait for, any more are
     prevented from happening */
#define Max_stws_before_suspend 2
  int stws_waited = 1;
  /* Wait until any in-progress STW sections end. */
  while (atomic_load_acquire(&stw_leader)) {
    if (stws_waited++ < Max_stws_before_suspend) {
      /* [caml_plat_wait] releases [all_domains_lock] until the current
         STW section ends, and then takes the lock again. */
      caml_plat_wait(&all_domains_cond, &all_domains_lock);
    } else {
      /* Prevent new STW requests to avoid our own starvation */
      stw_requests_suspended++;
      /* Wait for the current STW to end */
      do {
        caml_plat_wait(&all_domains_cond, &all_domains_lock);
      } while (atomic_load_acquire(&stw_leader));
      if (--stw_requests_suspended == 0) {
        /* Notify threads that were trying to run an STW section.
           We still hold the lock, so they won't wake up yet. */
        caml_plat_broadcast(&requests_suspended_cond);
      }
      break;
    }
  }

  d = next_free_domain();

  if (d == NULL)
    goto domain_init_complete;

  s = &d->interruptor;
  CAMLassert(!s->running);
  CAMLassert(!interruptor_has_pending(s));

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

  /* Note: until we take d->domain_lock, the domain_state may still be
   * shared with a domain which is terminating (see
   * caml_domain_terminate). */

  caml_plat_lock_blocking(&d->domain_lock);

  /* Set domain_self if we have successfully allocated the
   * caml_domain_state. Otherwise domain_self will be NULL and it's up
   * to the caller to deal with that. */

  domain_self = d;
  caml_state = domain_state;

  domain_state->young_limit = 0;
  /* Synchronized with [caml_interrupt_all_signal_safe], so that the
     initializing write of young_limit happens before any
     interrupt. */
  atomic_store_explicit(&s->interrupt_word, &domain_state->young_limit,
                        memory_order_release);

  domain_state->id = d->id;

  /* Tell memprof system about the new domain before either (a) new
   * domain can allocate anything or (b) parent domain can go away. */
  CAMLassert(domain_state->memprof == NULL);
  caml_memprof_new_domain(parent, domain_state);
  if (!domain_state->memprof) {
    goto init_memprof_failure;
  }

  CAMLassert(!interruptor_has_pending(s));

  domain_state->extra_heap_resources = 0.0;
  domain_state->extra_heap_resources_minor = 0.0;

  domain_state->dependent_size = 0;
  domain_state->dependent_allocated = 0;

  domain_state->major_work_done_between_slices = 0;

  /* the minor heap will be initialized by
     [caml_reallocate_minor_heap] below. */
  domain_state->young_start = NULL;
  domain_state->young_end = NULL;
  domain_state->young_ptr = NULL;
  domain_state->young_trigger = NULL;

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

  /* No remaining failure cases: domain creation is going to succeed,
   * so we can update globally-visible state without needing to unwind
   * it. */
  s->unique_id = fresh_domain_unique_id();
  domain_state->unique_id = s->unique_id;
  s->running = 1;
  atomic_fetch_add(&caml_num_domains_running, 1);

  domain_state->c_stack = NULL;
  domain_state->exn_handler = NULL;

  domain_state->action_pending = 0;

  domain_state->gc_regs_buckets = NULL;
  domain_state->gc_regs = NULL;

  domain_state->allocated_words = 0;
  domain_state->allocated_words_direct = 0;
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
  domain_state->major_slice_epoch = 0;
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

  add_next_to_stw_domains();
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
  caml_memprof_delete_domain(domain_state);
init_memprof_failure:
  domain_self = NULL;


domain_init_complete:
  caml_gc_log("domain init complete");
  caml_plat_unlock(&all_domains_lock);
}

CAMLexport void caml_reset_domain_lock(void)
{
  dom_internal* self = domain_self;
  // This is only used to reset the domain_lock state on fork.
  /* FIXME: initializing an already-initialized mutex and cond
     variable is UB (especially mutexes that are locked).

     * On systhreads, this is best-effort but at least the error
       conditions should be checked and reported.

     * If there is only one thread, it is sensible to fork but the
       mutex should still not be initialized while locked. On Linux it
       seems that the mutex remains valid and locked
       (https://man7.org/linux/man-pages/man2/fork.2.html). For
       portability on POSIX the lock should be released and destroyed
       prior to calling fork and then init afterwards in both parent
       and child. */
  caml_plat_mutex_reinit(&self->domain_lock);
  caml_plat_cond_init(&self->domain_cond);

  return;
}

/* minor heap initialization and resizing */

static void reserve_minor_heaps_from_stw_single(void) {
  void* heaps_base;
  uintnat minor_heap_reservation_bsize;
  uintnat minor_heap_max_bsz;

  CAMLassert (caml_mem_round_up_pages(Bsize_wsize(caml_minor_heap_max_wsz))
          == Bsize_wsize(caml_minor_heap_max_wsz));

  minor_heap_max_bsz = (uintnat)Bsize_wsize(caml_minor_heap_max_wsz);
  minor_heap_reservation_bsize = minor_heap_max_bsz * caml_params->max_domains;

  /* reserve memory space for minor heaps */
  heaps_base = caml_mem_map(minor_heap_reservation_bsize, 1 /* reserve_only */);
  if (heaps_base == NULL)
    caml_fatal_error("Not enough heap memory to reserve minor heaps");

  caml_minor_heaps_start = (uintnat) heaps_base;
  caml_minor_heaps_end = (uintnat) heaps_base + minor_heap_reservation_bsize;

  caml_gc_log("new minor heap reserved from %p to %p",
              (value*)caml_minor_heaps_start, (value*)caml_minor_heaps_end);

  for (int i = 0; i < caml_params->max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    uintnat domain_minor_heap_area = caml_minor_heaps_start +
      minor_heap_max_bsz * (uintnat)i;

    dom->minor_heap_area_start = domain_minor_heap_area;
    dom->minor_heap_area_end =
         domain_minor_heap_area + minor_heap_max_bsz;

    CAMLassert(dom->minor_heap_area_end <= caml_minor_heaps_end);
  }
}

static void unreserve_minor_heaps_from_stw_single(void) {
  uintnat size;

  caml_gc_log("unreserve_minor_heaps");

  for (int i = 0; i < caml_params->max_domains; i++) {
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
  CAMLassert (Bsize_wsize(caml_minor_heap_max_wsz) * caml_params->max_domains
              == size);
  caml_mem_unmap((void *) caml_minor_heaps_start, size);
}

static
void domain_resize_heap_reservation_from_stw_single(uintnat new_minor_wsz)
{
  CAML_EV_BEGIN(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
  caml_gc_log("stw_resize_minor_heap_reservation: "
              "unreserve_minor_heaps");

  unreserve_minor_heaps_from_stw_single();
  /* new_minor_wsz is page-aligned because caml_norm_minor_heap_size has
     been called to normalize it earlier.
  */
  caml_minor_heap_max_wsz = new_minor_wsz;
  caml_gc_log("stw_resize_minor_heap_reservation: reserve_minor_heaps");
  reserve_minor_heaps_from_stw_single();
  /* The call to [reserve_minor_heaps_from_stw_single] makes a new
     reservation, and it also updates the reservation boundaries of each
     domain by mutating its [minor_heap_area_start{,_end}] variables.

     These variables are synchronized by the fact that we are inside
     a STW section: no other domains are running in parallel, and
     the participating domains will synchronize with this write by
     exiting the barrier, before they read those variables in
     [allocate_minor_heap] below. */
  CAML_EV_END(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
}

static void
stw_resize_minor_heap_reservation(caml_domain_state* domain,
                                  void* minor_wsz_data,
                                  int participating_count,
                                  caml_domain_state** participating) {
  caml_gc_log("stw_resize_minor_heap_reservation: "
              "caml_empty_minor_heap_no_major_slice_from_stw");
  caml_empty_minor_heap_no_major_slice_from_stw(
    domain, NULL, participating_count, participating);

  caml_gc_log("stw_resize_minor_heap_reservation: free_minor_heap");
  free_minor_heap();

  Caml_global_barrier_if_final(participating_count) {
    uintnat new_minor_wsz = (uintnat) minor_wsz_data;
    domain_resize_heap_reservation_from_stw_single(new_minor_wsz);
  }

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

void caml_init_domains(uintnat max_domains, uintnat minor_heap_wsz)
{
  atomic_store_relaxed(&domains_exiting, 0);
  atomic_store_relaxed(&caml_num_domains_running, 0);

  /* Use [caml_stat_calloc_noexc] to zero initialize [all_domains]. */
  all_domains = caml_stat_calloc_noexc(max_domains, sizeof(dom_internal));
  if (all_domains == NULL)
    caml_fatal_error("Failed to allocate all_domains");

  stw_request.participating =
      caml_stat_calloc_noexc(max_domains, sizeof(dom_internal*));
  if (stw_request.participating == NULL)
    caml_fatal_error("Failed to allocate stw_request.participating");

  stw_domains.domains =
      caml_stat_calloc_noexc(max_domains, sizeof(dom_internal*));
  if (stw_domains.domains == NULL)
    caml_fatal_error("Failed to allocate stw_domains.domains");

  reserve_minor_heaps_from_stw_single();
  /* stw_single: mutators and domains have not started yet. */

  for (int i = 0; i < max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    stw_domains.domains[i] = dom;

    dom->id = i;

    dom->interruptor.interrupt_word = NULL;
    caml_plat_mutex_init(&dom->interruptor.lock);
    caml_plat_cond_init(&dom->interruptor.cond);
    dom->interruptor.running = 0;
    dom->interruptor.terminating = 0;
    dom->interruptor.unique_id = 0;
    dom->interruptor.interrupt_pending = 0;

    caml_plat_mutex_init(&dom->domain_lock);
    caml_plat_cond_init(&dom->domain_cond);
    dom->backup_thread_msg = BT_INIT;
    dom->domain_canceled = false;
  }

  domain_create(minor_heap_wsz, NULL);
  if (!domain_self) caml_fatal_error("Failed to create main domain");
  CAMLassert (domain_self->state->unique_id == 0);

  caml_init_signal_handling();
}

void caml_init_domain_self(int domain_id) {
  CAMLassert(0 <= domain_id);
  CAMLassert(domain_id < caml_params->max_domains);
  domain_self = &all_domains[domain_id];
  caml_state = domain_self->state;
}

enum domain_status { Dom_starting, Dom_started, Dom_failed };

struct domain_ml_values {
  value callback;
  value term_sync;
};

/* stdlib/domain.ml */
#define Term_state(sync) (&Field(sync, 0))
#define Term_mutex(sync) (&Field(sync, 1))
#define Term_condition(sync) (&Field(sync, 2))

static void init_domain_ml_values(struct domain_ml_values* ml_values,
                                  value callback, value term_sync)
{
  ml_values->callback = callback;
  ml_values->term_sync = term_sync;
  caml_register_generational_global_root(&ml_values->callback);
  caml_register_generational_global_root(&ml_values->term_sync);
}

static void free_domain_ml_values(struct domain_ml_values* ml_values)
{
  caml_remove_generational_global_root(&ml_values->callback);
  caml_remove_generational_global_root(&ml_values->term_sync);
  caml_stat_free(ml_values);
}

/* This is the structure of the data exchanged between the parent
   domain and child domain during domain_spawn. Some fields are 'in'
   parameters, passed from the parent to the child, others are 'out'
   parameters returned to the parent by the child.
*/
struct domain_startup_params {
  dom_internal *parent; /* in */
  enum domain_status status; /* in+out:
                                parent and child synchronize on this value. */
  struct domain_ml_values* ml_values; /* in */
  dom_internal* newdom; /* out */
  uintnat unique_id; /* out */
};

static void* backup_thread_func(void* v)
{
  dom_internal* di = (dom_internal*)v;
  uintnat msg;
  struct interruptor* s = &di->interruptor;

  domain_self = di;
  caml_state = di->state;

  msg = atomic_load_acquire (&di->backup_thread_msg);
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
        /* Wait safely if there is nothing to do. Will be woken from
         * caml_send_interrupt and caml_domain_terminate.
         */
        caml_plat_lock_blocking(&s->lock);
        msg = atomic_load_acquire (&di->backup_thread_msg);
        if (msg == BT_IN_BLOCKING_SECTION &&
            !caml_incoming_interrupts_queued())
          caml_plat_wait(&s->cond, &s->lock);
        caml_plat_unlock(&s->lock);
        break;
      case BT_ENTERING_OCAML:
        /* Main thread wants to enter OCaml.
         * Will be woken from caml_bt_exit_ocaml
         * or caml_domain_terminate.
         */
        caml_plat_lock_blocking(&di->domain_lock);
        msg = atomic_load_acquire (&di->backup_thread_msg);
        if (msg == BT_ENTERING_OCAML)
          caml_plat_wait(&di->domain_cond, &di->domain_lock);
        caml_plat_unlock(&di->domain_lock);
        break;
      default:
        cpu_relax();
        break;
    };
    msg = atomic_load_acquire (&di->backup_thread_msg);
  }

  /* doing terminate */
  atomic_store_release(&di->backup_thread_msg, BT_INIT);

  return 0;
}

static void install_backup_thread (dom_internal* di)
{
  int err;
#ifndef _WIN32
  sigset_t mask, old_mask;
#endif

  /* If the backup thread is running, but has been instructed to terminate,
     we need to wait for it to stop until we can spawn another. */
  while (backup_thread_running(di)) {
    /* Give a chance for backup thread on this domain to terminate */
    caml_plat_unlock (&di->domain_lock);
    cpu_relax ();
    caml_plat_lock_blocking(&di->domain_lock);
  }

  CAMLassert(!backup_thread_running(di));

#ifndef _WIN32
  /* No signals on the backup thread */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

  atomic_store_release(&di->backup_thread_msg, BT_ENTERING_OCAML);
  err = pthread_create(&di->backup_thread, 0, backup_thread_func, (void*)di);
  caml_check_error(err, "failed to create domain backup thread");

#ifndef _WIN32
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  pthread_detach(di->backup_thread);
}

static void terminate_backup_thread(dom_internal *di)
{
  CAMLassert(!caml_bt_is_self());

  if (backup_thread_running(di)) {
    atomic_store_release(&di->backup_thread_msg, BT_TERMINATE);
    /* Wakeup backup thread if it is sleeping */
    caml_plat_broadcast(&di->interruptor.cond);
    caml_plat_signal(&di->domain_cond);
  }
}

static void caml_domain_initialize_default(void)
{
  return;
}

static void caml_domain_stop_default(void)
{
  return;
}

static void caml_domain_external_interrupt_hook_default(void)
{
  return;
}

CAMLexport void (*caml_domain_initialize_hook)(void) =
   caml_domain_initialize_default;

CAMLexport void (*caml_domain_stop_hook)(void) =
   caml_domain_stop_default;

CAMLexport void (*caml_domain_external_interrupt_hook)(void) =
   caml_domain_external_interrupt_hook_default;

CAMLexport _Atomic caml_timing_hook caml_domain_terminated_hook =
  (caml_timing_hook)NULL;

static value make_finished(caml_result result)
{
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_1(
    (caml_result_is_exception(result) ?
     1 /* Error */ :
     0 /* Ok */),
    result.data);
  /* [Finished res] */
  res = caml_alloc_1(0, res);
  CAMLreturn(res);
}

static void sync_result(value term_sync, value res)
{
  CAMLparam2(term_sync, res);
  /* Synchronize with joining domains. We call [caml_ml_mutex_lock]
     because the systhreads are still running on this domain. We
     assume this does not fail the exception it would raise at this
     point would be bad for us. */
  caml_ml_mutex_lock(*Term_mutex(term_sync));

  /* Store result */
  volatile value *state = Term_state(term_sync);
  CAMLassert(!Is_block(*state));
  caml_modify(state, res);

  /* Signal all the waiting domains to be woken up */
  caml_ml_condition_broadcast(*Term_condition(term_sync));

  /* The mutex is unlocked in the runtime after the cleanup
     functions are finished. */
  CAMLreturn0;
}

static void* domain_thread_func(void* v)
{
  struct domain_startup_params* p = v;
  struct domain_ml_values *ml_values = p->ml_values;

#ifndef _WIN32
  void * signal_stack = caml_init_signal_stack();
  if (signal_stack == NULL) {
    caml_fatal_error("Failed to create domain: signal stack");
  }
#endif

  domain_create(caml_params->init_minor_heap_wsz, p->parent->state);
  if (domain_self)
    domain_self->tid = pthread_self();

  /* this domain is now part of the STW participant set */
  p->newdom = domain_self;

  /* handshake with the parent domain */
  caml_plat_lock_blocking(&p->parent->interruptor.lock);
  if (domain_self) {
    p->status = Dom_started;
    p->unique_id = domain_self->interruptor.unique_id;
  } else {
    p->status = Dom_failed;
  }
  caml_plat_broadcast(&p->parent->interruptor.cond);
  caml_plat_unlock(&p->parent->interruptor.lock);
  /* Cannot access p below here. */

  if (domain_self) {
    install_backup_thread(domain_self);

    caml_gc_log("Domain starting (unique_id = %"ARCH_INTNAT_PRINTF_FORMAT"u)",
                domain_self->interruptor.unique_id);
    CAML_EV_LIFECYCLE(EV_DOMAIN_SPAWN, getpid());
    /* FIXME: ignoring errors during domain initialization is unsafe
       and/or can deadlock. */
    caml_domain_initialize_hook();

    /* release callback early;
       see the [note about callbacks and GC] in callback.c */
    value unrooted_callback = ml_values->callback;
    caml_modify_generational_global_root(&ml_values->callback, Val_unit);
    value res =
      make_finished(caml_callback_res(unrooted_callback, Val_unit));
    sync_result(ml_values->term_sync, res);

    sync_mutex mut = Mutex_val(*Term_mutex(ml_values->term_sync));
    caml_domain_terminate(false);

    /* This domain currently holds [mut], and has signaled all the
       waiting domains to be woken up. We unlock [mut] to release the
       joining domains. The unlock is done after [caml_domain_terminate] to
       ensure that this domain has released all of its runtime state.
       We call [caml_mutex_unlock] directly instead of
       [caml_ml_mutex_unlock] because the domain no longer exists at
       this point. */
    caml_mutex_unlock(mut);

    /* [ml_values] must be freed after unlocking [mut]. This ensures
       that [term_sync] is only removed from the root set after [mut]
       is unlocked. Otherwise, there is a risk of [mut] being
       destroyed by [caml_mutex_finalize] finaliser while it remains
       locked, leading to undefined behaviour. */
    free_domain_ml_values(ml_values);
  } else {
    caml_gc_log("Failed to create domain");
  }
#ifndef _WIN32
  caml_free_signal_stack(signal_stack);
#endif
  return 0;
}

CAMLprim value caml_domain_spawn(value callback, value term_sync)
{
  CAMLparam2 (callback, term_sync);
  struct domain_startup_params p;
  pthread_t th;
  int err;

  if (atomic_load_relaxed(&domains_exiting) != 0) {
    caml_failwith("domain creation not allowed during shutdown");
  }

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support spawning multiple domains");
#endif
  p.parent = domain_self;
  p.status = Dom_starting;

  p.ml_values =
      (struct domain_ml_values*) caml_stat_alloc(
                                    sizeof(struct domain_ml_values));
  init_domain_ml_values(p.ml_values, callback, term_sync);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  caml_check_error(err, "failed to create domain thread: pthread_create");

  /* While waiting for the child thread to start up, we need to service any
     stop-the-world requests as they come in. */
  struct interruptor *interruptor = &domain_self->interruptor;
  caml_plat_lock_blocking(&interruptor->lock);
  while (p.status == Dom_starting) {
    if (caml_incoming_interrupts_queued()) {
      caml_plat_unlock(&interruptor->lock);
      handle_incoming(interruptor);
      caml_plat_lock_blocking(&interruptor->lock);
    } else {
      caml_plat_wait(&interruptor->cond, &interruptor->lock);
    }
  }
  caml_plat_unlock(&interruptor->lock);

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
  domain_self->tid = pthread_self();
  if (!backup_thread_running(domain_self))
    install_backup_thread(domain_self);

  CAMLreturn (Val_long(p.unique_id));
}

CAMLprim value caml_ml_domain_id(value unit)
{
  CAMLnoalloc;
  return Val_long(domain_self->interruptor.unique_id);
}

CAMLprim value caml_ml_domain_index(value unit)
{
  CAMLnoalloc;
  return Val_long(domain_self->id);
}

/* Global barrier implementation */

Caml_inline int global_barrier_is_nth(barrier_status b, int n) {
  return (b & ~BARRIER_SENSE_BIT) == n;
}

static barrier_status global_barrier_begin(void)
{
  return caml_plat_barrier_arrive(&stw_request.barrier);
}

/* last domain into the barrier, flip sense */
static void global_barrier_flip(barrier_status sense)
{
  caml_plat_barrier_flip(&stw_request.barrier, sense);
}

/* wait until another domain flips the sense */
static void global_barrier_wait(barrier_status sense, int num_participating)
{
  /* it's not worth spinning for too long if there's more than one other domain
   */
  unsigned spins = num_participating == 2 ? Max_spins_long : Max_spins_medium;
  SPIN_WAIT_NTIMES(spins) {
    if (caml_plat_barrier_sense_has_flipped(&stw_request.barrier, sense)) {
      return;
    }
  }
  /* just block */
  caml_plat_barrier_wait_sense(&stw_request.barrier, sense);
}

void caml_enter_global_barrier(int num_participating)
{
  CAMLassert(num_participating == stw_request.num_domains);
  barrier_status b = global_barrier_begin();
  barrier_status sense = b & BARRIER_SENSE_BIT;
  if (global_barrier_is_nth(b, num_participating)) {
    global_barrier_flip(sense);
  } else {
    global_barrier_wait(sense, num_participating);
  }
}

barrier_status caml_global_barrier_and_check_final(int num_participating)
{
  CAMLassert(num_participating == stw_request.num_domains);
  barrier_status b = global_barrier_begin();
  if (global_barrier_is_nth(b, num_participating)) {
    CAMLassert(b); /* always nonzero */
    return b;
  } else {
    global_barrier_wait(b & BARRIER_SENSE_BIT, num_participating);
    return 0;
  }
}

void caml_global_barrier_release_as_final(barrier_status b)
{
  global_barrier_flip(b & BARRIER_SENSE_BIT);
}

int caml_global_barrier_num_participating(void)
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
    caml_plat_lock_blocking(&all_domains_lock);
    atomic_store_release(&stw_leader, 0);
    caml_plat_broadcast(&all_domains_cond);
    caml_gc_log("clearing stw leader");
    caml_plat_unlock(&all_domains_lock);
  }
}

/* Wait for other running domains to stop, called by interrupted
   domains before entering the STW section */
static void stw_wait_for_running(caml_domain_state* domain)
{
  /* The STW leader issues interrupts to all domains, then they all
     arrive into this barrier, with the last one releasing it; this
     tends to (and should) be fast, but we likely need to wait a bit
     in any case */

  if (stw_request.enter_spin_callback) {
    /* Spin while there is useful work to do */
    SPIN_WAIT_BOUNDED {
      if (caml_plat_barrier_is_released(&stw_request.domains_still_running)) {
        return;
      }

      if (!stw_request.enter_spin_callback
            (domain, stw_request.enter_spin_data)) {
        break;
      }
    }
  }

  /* Spin a bit for the other domains */
  SPIN_WAIT_NTIMES(Max_spins_long) {
    if (caml_plat_barrier_is_released(&stw_request.domains_still_running)) {
      return;
    }
  }

  /* If we're still waiting, block */
  caml_plat_barrier_wait(&stw_request.domains_still_running);
}

static void stw_api_barrier(caml_domain_state* domain)
{
  CAML_EV_BEGIN(EV_STW_API_BARRIER);
  if (caml_plat_barrier_arrive(&stw_request.domains_still_running)
      == stw_request.num_domains) {
    caml_plat_barrier_release(&stw_request.domains_still_running);
  } else {
    stw_wait_for_running(domain);
  }
  CAML_EV_END(EV_STW_API_BARRIER);
}

static void stw_handler(caml_domain_state* domain)
{
  CAML_EV_BEGIN(EV_STW_HANDLER);
  if (!caml_plat_barrier_is_released(&stw_request.domains_still_running)) {
    stw_api_barrier(domain);
  }

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

   - Domain initialization code from [domain_create] will not run in
     parallel with a STW section, as [domain_create] starts by looping
     until (1) it has the [all_domains_lock] and (2) there is no
     current STW section (using the [stw_leader] variable). To avoid
     starvation, [domain_create] will prevent new STW sections if it
     can't make progress.

   - Domain cleanup code runs after the terminating domain may run in
     parallel to a STW section, but only after that domain has safely
     removed itself from the STW participant set: the
     [caml_domain_terminate] function is careful to only leave the STW
     set when (1) it has the [all_domains_lock] and (2) it hasn't
     received any request to participate in a STW section.

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

   Note: in the case of both [domain_create] and [caml_domain_terminate]
   it is important that the loops (waiting for STW sections to finish)
   regularly release [all_domains_lock], to avoid deadlocks scenario
   with in-progress STW sections.
    - For [caml_domain_terminate] we release the lock and join
      the STW section before resuming.
    - For [domain_create] we wait until the end of the section using
      the condition variable [all_domains_cond] over
      [all_domains_lock], which is broadcasted when a STW section
      finishes.
   The same logic would apply for any other situations in which a domain
   wants to join or leave the set of STW participants.

  The explanation above applies if [sync] = 1. When [sync] = 0, no
  synchronization happens, and we simply run the handler asynchronously on
  all domains. We still hold the stw_leader field until we know that
  every domain has run the handler, so another STW section cannot
  interfere with this one.

*/
int caml_try_run_on_all_domains_with_spin_work(
  int sync,
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*),
  int (*enter_spin_callback)(caml_domain_state*, void*),
  void* enter_spin_data)
{
  int i;
  caml_domain_state* domain_state = domain_self->state;

  caml_gc_log("requesting STW, sync=%d", sync);

  /* Don't touch the lock if there's already a stw leader
     OR we can't get the lock.

     Note: this read on [stw_leader] is an optimization, giving up
     faster (before trying to take the lock) in contended
     situations. Without this read, [stw_leader] would be protected by
     [all_domains_lock] and could be a non-atomic variable.
  */
  if (atomic_load_acquire(&stw_leader) ||
      !caml_plat_try_lock(&all_domains_lock)) {
    caml_handle_incoming_interrupts();
    return 0;
  }

  while (1) {
    /* see if there is a stw_leader already */
    if (atomic_load_acquire(&stw_leader)) {
      caml_plat_unlock(&all_domains_lock);
      caml_handle_incoming_interrupts();
      return 0;
    }

    /* STW requests may be suspended by [domain_create], in which case, instead
       of claiming the stw_leader, we should release the lock and wait for
       requests to be unsuspended before trying again */
    if (CAMLunlikely(stw_requests_suspended)) {
      caml_plat_wait(&requests_suspended_cond, &all_domains_lock);
      /* we hold the lock, but we must check for [stw_leader] again */
      continue;
    }

    break;
  }

  /* we have the lock and can claim the stw_leader */
  atomic_store_release(&stw_leader, (uintnat)domain_self);

  CAML_EV_BEGIN(EV_STW_LEADER);
  caml_gc_log("causing STW");

  /* set up all fields for this stw_request; they must be available
     for domains when they get interrupted */
  stw_request.enter_spin_callback = enter_spin_callback;
  stw_request.enter_spin_data = enter_spin_data;
  stw_request.callback = handler;
  stw_request.data = data;
  stw_request.num_domains = stw_domains.participating_domains;
  /* stw_request.barrier doesn't need resetting */
  atomic_store_release(&stw_request.num_domains_still_processing,
                       stw_domains.participating_domains);

  int is_alone = stw_request.num_domains == 1;
  int should_sync = sync && !is_alone;

  if (should_sync) {
    caml_plat_barrier_reset(&stw_request.domains_still_running);
  }

  if( leader_setup ) {
    leader_setup(domain_state);
  }

#ifdef DEBUG
  {
    int domains_participating = 0;
    for(i=0; i<caml_params->max_domains; i++) {
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
    CAMLassert(!interruptor_has_pending(&d->interruptor));
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

  /* arrive at enter barrier */
  if (should_sync) {
    stw_api_barrier(domain_state);
  }

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
      caml_try_run_on_all_domains_with_spin_work(1,
                                                 handler,
                                                 data,
                                                 leader_setup, 0, 0);
}

int caml_try_run_on_all_domains_async(
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*))
{
  return
      caml_try_run_on_all_domains_with_spin_work(0,
                                                 handler,
                                                 data,
                                                 leader_setup, 0, 0);
}

void caml_interrupt_self(void)
{
  interrupt_domain_local(Caml_state);
}

/*  This function is async-signal-safe as [all_domains] and
    [caml_params->max_domains] are set before signal handlers are installed and
    do not change afterwards. */
void caml_interrupt_all_signal_safe(void)
{
  for (dom_internal *d = all_domains;
       d < &all_domains[caml_params->max_domains];
       d++) {
    /* [all_domains] is an array of values. So we can access
       [interrupt_word] directly without synchronisation other than
       with other people who access the same [interrupt_word].*/
    atomic_uintnat * interrupt_word =
      atomic_load_acquire(&d->interruptor.interrupt_word);
    /* Early exit: if the current domain was never initialized, then
       neither have been any of the remaining ones. */
    if (interrupt_word == NULL) return;
    interrupt_domain(&d->interruptor);
  }
}

/* To avoid any risk of forgetting an action through a race,
   [caml_reset_young_limit] is the only way (apart from setting
   young_limit to -1 for immediate interruption) through which
   [young_limit] can be modified. We take care here of possible
   races. */
void caml_reset_young_limit(caml_domain_state * dom_st)
{
  /* An interrupt might have been queued in the meanwhile; the
     atomic_exchange achieves the proper synchronisation with the
     reads that follow (an atomic_store is not enough). */
  value *trigger = dom_st->young_trigger > dom_st->memprof_young_trigger ?
          dom_st->young_trigger : dom_st->memprof_young_trigger;
  CAMLassert ((uintnat)dom_st->young_ptr >=
              (uintnat)dom_st->memprof_young_trigger);
  CAMLassert ((uintnat)dom_st->young_ptr >=
              (uintnat)dom_st->young_trigger);
  /* An interrupt might have been queued in the meanwhile; this
     achieves the proper synchronisation. */
  atomic_exchange(&dom_st->young_limit, (uintnat)trigger);

  /* For non-delayable asynchronous actions, we immediately interrupt
     the domain again. */
  dom_internal * d = &all_domains[dom_st->id];
  if (interruptor_has_pending(&d->interruptor)
      || dom_st->requested_minor_gc
      || dom_st->requested_major_slice
      || dom_st->major_slice_epoch < atomic_load (&caml_major_slice_epoch)) {
    interrupt_domain_local(dom_st);
  }
  /* We might be here due to a recently-recorded signal or forced
     systhread switching, so we need to remember that we must run
     signal handlers or systhread's yield. In addition, in the case of
     long-running C code (that may regularly poll with
     caml_process_pending_actions), we want to force a query of all
     callbacks at every minor collection or major slice (similarly to
     the OCaml behaviour). */
  caml_set_action_pending(dom_st);
}

void caml_update_young_limit_after_c_call(caml_domain_state * dom_st)
{
  if (CAMLunlikely(dom_st->action_pending)) interrupt_domain_local(dom_st);
}

Caml_inline void advance_global_major_slice_epoch (caml_domain_state* d)
{
  uintnat old_value;

  CAMLassert (atomic_load (&caml_major_slice_epoch) <=
              atomic_load (&caml_minor_collections_count));

  old_value = atomic_exchange (&caml_major_slice_epoch,
                               atomic_load (&caml_minor_collections_count));

  if (old_value != atomic_load (&caml_minor_collections_count)) {
    /* This domain is the first one to use up half of its minor heap arena
        in this minor cycle. Trigger major slice on other domains. */
    caml_interrupt_all_signal_safe();
  }
}

static void stw_global_major_slice(
  caml_domain_state *domain,
  void *unused,
  int participating_count,
  caml_domain_state **participating)
{
  domain->requested_major_slice = 1;
  /* Nothing else to do, as [stw_hander] will call [caml_poll_gc_work]
     right after the callback. */
}

void caml_poll_gc_work(void)
{
  CAMLalloc_point_here;

  caml_domain_state* d = Caml_state;

  if ((uintnat)d->young_ptr - Bhsize_wosize(Max_young_wosize) <
      (uintnat)d->young_trigger) {

    if (d->young_trigger == d->young_start) {
      /* Trigger minor GC */
      d->requested_minor_gc = 1;
    } else {
      CAMLassert (d->young_trigger ==
                  d->young_start + (d->young_end - d->young_start) / 2);
      /* We have used half of our minor heap arena. Request a major slice on
         this domain. */
      advance_global_major_slice_epoch (d);
      /* Advance the [young_trigger] to [young_start] so that the allocation
         fails when the minor heap is full. */
      d->young_trigger = d->young_start;
    }
  } else if (d->requested_minor_gc) {
    /* This domain has _not_ used up half of its minor heap arena, but a minor
       collection has been requested. Schedule a major collection slice so as
       to not lag behind. */
    advance_global_major_slice_epoch (d);
  }

  if (d->major_slice_epoch < atomic_load (&caml_major_slice_epoch)) {
    d->requested_major_slice = 1;
  }

  if (d->requested_minor_gc) {
    /* out of minor heap or collection forced */
    d->requested_minor_gc = 0;
    caml_empty_minor_heaps_once();
  }

  if (d->requested_major_slice || d->requested_global_major_slice) {
    CAML_EV_BEGIN(EV_MAJOR);
    d->requested_major_slice = 0;
    caml_major_collection_slice(AUTO_TRIGGERED_MAJOR_SLICE);
    CAML_EV_END(EV_MAJOR);
  }

  if (d->requested_global_major_slice) {
    if (caml_try_run_on_all_domains_async(
          &stw_global_major_slice, NULL, NULL)){
      d->requested_global_major_slice = 0;
    }
    /* If caml_try_run_on_all_domains_async fails, we'll try again next time
       caml_poll_gc_work is called. */
  }

  caml_reset_young_limit(d);
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

/* Preemptive systhread switching */
void caml_process_external_interrupt(void)
{
  if (atomic_load_acquire(&Caml_state->requested_external_interrupt)) {
    caml_domain_external_interrupt_hook();
  }
}

CAMLexport int caml_bt_is_in_blocking_section(void)
{
  uintnat status = atomic_load_acquire(&domain_self->backup_thread_msg);
  return status == BT_IN_BLOCKING_SECTION;
}

CAMLexport int caml_bt_is_self(void)
{
  return pthread_equal(domain_self->backup_thread, pthread_self());
}

CAMLexport intnat caml_domain_is_multicore (void)
{
  return (!caml_domain_alone()
          || backup_thread_running(domain_self));
}

CAMLexport void caml_acquire_domain_lock(void)
{
  dom_internal* self = domain_self;
  caml_plat_lock_blocking(&self->domain_lock);
  caml_state = self->state;
}

CAMLexport void caml_bt_enter_ocaml(void)
{
  dom_internal* self = domain_self;
  bool bt_running = backup_thread_running(self);
  CAMLassert(caml_domain_alone() || bt_running);

  if (bt_running) {
    atomic_store_release(&self->backup_thread_msg, BT_ENTERING_OCAML);
  }
}

CAMLexport void caml_release_domain_lock(void)
{
  dom_internal* self = domain_self;
  caml_state = NULL;
  caml_plat_unlock(&self->domain_lock);
}

CAMLexport void caml_bt_exit_ocaml(void)
{
  dom_internal* self = domain_self;
  bool bt_running = backup_thread_running(self);

  CAMLassert(caml_domain_alone() || bt_running);

  if (bt_running) {
    atomic_store_release(&self->backup_thread_msg, BT_IN_BLOCKING_SECTION);
    /* Wakeup backup thread if it is sleeping */
    caml_plat_signal(&self->domain_cond);
  }
}

/* default handler for unix_fork, will be called by unix_fork. */
static void caml_atfork_default(void)
{
  caml_reset_domain_lock();
  caml_acquire_domain_lock();
  /* FIXME: For best portability, the IO channel locks should be
     reinitialised as well. (See comment in
     caml_reset_domain_lock.) */
}

CAMLexport void (*caml_atfork_hook)(void) = caml_atfork_default;

static inline int domain_terminating(dom_internal *d) {
  return d->interruptor.terminating;
}

int caml_domain_terminating (caml_domain_state *dom_st)
{
  return domain_terminating(&all_domains[dom_st->id]);
}

int caml_domain_is_terminating (void)
{
  return domain_terminating(domain_self);
}

void caml_domain_terminate(bool last)
{
  caml_domain_state* domain_state = domain_self->state;
  struct interruptor* s = &domain_self->interruptor;
  int finished = 0;

  caml_gc_log("Domain terminating");
  s->terminating = 1;

  /* Join ongoing systhreads, if necessary, and then run user-defined
     termination hooks. No OCaml code can run on this domain after
     this. */
  caml_domain_stop_hook();
  call_timing_hook(&caml_domain_terminated_hook);

  while (!finished) {
    caml_finish_sweeping();

    caml_empty_minor_heaps_once();
    /* Note: [caml_empty_minor_heaps_once] will also join any ongoing
       STW sections that has sent an interrupt to this domain. */

    if (last)
      caml_finish_major_cycle(0);

    caml_finish_marking();

    caml_orphan_ephemerons(domain_state);
    caml_orphan_finalisers(domain_state);

    /* No need to check for interrupts if we are the last domain running. */
    if (last) {
      CAML_EV_LIFECYCLE(EV_DOMAIN_TERMINATE, getpid());
      break;
    }

    /* take the all_domains_lock to try and exit the STW participant set
       without racing with a STW section being triggered */
    caml_plat_lock_blocking(&all_domains_lock);

    /* The interaction of termination and major GC is quite subtle.

       At the end of the major GC, we decide the number of domains to mark and
       sweep for the next cycle. If a STW section has been started, it will
       require this domain to participate, which in turn could involve a major
       GC cycle. This would then require finish marking and sweeping again in
       order to decrement the globals [num_domains_to_mark] and
       [num_domains_to_sweep] (see major_gc.c).
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
      caml_plat_lock_blocking(&s->lock);
      caml_plat_broadcast(&s->cond);
      caml_plat_unlock(&s->lock);

      /* We must signal domain termination before releasing [all_domains_lock]:
         after that, this domain will no longer take part in STWs and emitting
         an event could race with runtime events teardown. */
      CAML_EV_LIFECYCLE(EV_DOMAIN_TERMINATE, getpid());
    }
    caml_plat_unlock(&all_domains_lock);
  }

  /* domain_state may be re-used by a fresh domain here (now that we
   * have done remove_from_stw_domains and released the
   * all_domains_lock). However, domain_create() won't touch it until
   * it has claimed the domain_lock, so we hang onto that while we are
   * tearing down the state. */

  /* Delete the domain state from statmemprof after any promotion
   * (etc) done by this domain: any remaining memprof state will be
   * handed over to surviving domains. */
  caml_memprof_delete_domain(domain_state);

  /* We can not touch domain_self->interruptor after here
     because it may be reused */
  caml_remove_generational_global_root(&domain_state->dls_root);
  caml_remove_generational_global_root(&domain_state->backtrace_last_exn);
  caml_stat_free(domain_state->final_info);
  caml_stat_free(domain_state->ephe_info);
  caml_free_intern_state();
  caml_free_extern_state();
  caml_teardown_major_gc();

  if (last)
    caml_finalise_heap();

  caml_teardown_shared_heap(domain_state->shared_heap);
  domain_state->shared_heap = 0;
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = 0;

  caml_orphan_alloc_stats(domain_state);
  /* Heap stats were orphaned by [caml_teardown_shared_heap] above.
     At this point, the stats of the domain must be empty.

     The sampled copy was also cleared by the minor collection(s)
     performed above at [caml_empty_minor_heaps_once()], see the
     termination-specific logic in [caml_collect_gc_stats_sample_stw].
  */

  /* TODO: can this ever be NULL? can we remove this check? */
  if(domain_state->current_stack != NULL) {
    caml_free_stack(domain_state->current_stack);
  }
  caml_free_backtrace_buffer(domain_state->backtrace_buffer);
  caml_free_gc_regs_buckets(domain_state->gc_regs_buckets);

  /* signal the domain termination to the backup thread
     NB: for a program with no additional domains, the backup thread
     will not have been started */
  terminate_backup_thread(domain_self);
  caml_plat_unlock(&domain_self->domain_lock);

  caml_plat_assert_all_locks_unlocked();
  /* This is the last thing we do because we need to be able to rely
     on caml_domain_alone (which uses caml_num_domains_running) in at least
     the shared_heap lockfree fast paths. Also, we don't want to decrement
     it back to zero when the last domain exits, for caml_domain_alone()
     to remain accurate. */
  if (!last)
    atomic_fetch_add(&caml_num_domains_running, -1);
}

/* Try and terminate the currently running domain.
   This is only invoked when extra domains are left running while the
   main one is terminating. In this case, we are not in a state where
   we can safely release resources. The best we can do is cancel the
   extra running threads. */
static void stw_terminate_domain(caml_domain_state *domain, void *data,
  int participating_count,
  caml_domain_state **participating)
{
  if (!pthread_equal(domain_self->tid, *(pthread_t *)data)) {
    if (caml_bt_is_self()) {
      /* If this STW request is handled by the backup thread, the
         domain thread is currently running C code. */
      domain_self->domain_canceled = true;
      (void)pthread_cancel(domain_self->tid);
      /* We are intentionally not waiting for the thread to terminate here,
         and not decrementing the number of running domains either, since
         we don't know the state of the various locks and condition
         variables in this state. */
      atomic_store_release(&domain_self->backup_thread_msg, BT_INIT);
    } else {
      /* Domain threads forced to exit here will not have a chance to
         run caml_domain_terminate() on their own, so we need to ask
         the backup thread to terminate here. */
      terminate_backup_thread(domain_self);
      caml_plat_unlock(&domain_self->domain_lock);
      /* No particular memory resource cleanup is attempted here, for we
         have no idea which state each domain is in. */
    }
    pthread_exit(0);
  }
}

void caml_stop_all_domains(void)
{
  atomic_store_relaxed(&domains_exiting, 1);

  pthread_t myself = pthread_self();
  do {} while (!caml_try_run_on_all_domains(
               &stw_terminate_domain, &myself, NULL));

  terminate_backup_thread(domain_self);
  caml_plat_unlock(&domain_self->domain_lock);

  caml_plat_assert_all_locks_unlocked();
}

bool caml_free_domains(void)
{
  bool result = true;

  for (int i = 0; i < caml_params->max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    /* Give the backup thread time to terminate gracefully, if needed */
    while (backup_thread_running(dom)) {
      cpu_relax();
    }

    dom->interruptor.interrupt_word = NULL;
    caml_plat_mutex_free(&dom->interruptor.lock);
    caml_plat_cond_free(&dom->interruptor.cond);

    if (dom->domain_canceled)
      result = false;
    else
      caml_plat_mutex_free(&dom->domain_lock);
    caml_plat_cond_free(&dom->domain_cond);
  }

#ifdef WITH_THREAD_SANITIZER
  /* When running with TSan, there will be reports of races between
     freeing the all_domains synchronization objects and domain threads
     accessing them, even though we wait first for the domain threads to
     have terminated in the above loop. */
  result = false;
#endif

  return result;
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

CAMLprim value caml_domain_dls_compare_and_set(value old, value new)
{
  CAMLnoalloc;
  value current = Caml_state->dls_root;
  if (current == old) {
    caml_modify_generational_global_root(&Caml_state->dls_root, new);
    return Val_true;
  } else {
    return Val_false;
  }
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
  else if (n > caml_params->max_domains)
    n = caml_params->max_domains;

  return (Val_long(n));
}
