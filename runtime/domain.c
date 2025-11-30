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
   (install_backup_thread_exn)                              |
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
  /* Synchronised with [caml_find_index_of_running_domain] */
  atomic_uintnat unique_id;

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
  uintnat minor_heap_reservation_start;
  uintnat minor_heap_reservation_end;
};
typedef struct dom_internal dom_internal;

static CAMLthread_local dom_internal* domain_self;

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

/*
  This structure is protected by all_domains_lock.

  Three regions in the [domains]  array correspond to three states for domains:
   - at indices [0, active_domains) are the 'active' domains,
     which participate in STW sections.
   - [active_domains, parked_domains) are the 'parked' domains,
     in the process of spawning.
   - [parked_domains, caml_params->max_domains) are the 'stopped' domains,
     which are currently unused by the runtime.
 */
static struct {
  int active_domains;
  int parked_domains;
  dom_internal** domains;
} stw_domains = {
  0,
  0,
  NULL
};


static void check_domain_limit(int idx) {
  CAMLassert(0 <= idx && idx <= caml_params->max_domains);
}
static void check_stw_domains(void) {
  check_domain_limit(stw_domains.active_domains);
  check_domain_limit(stw_domains.parked_domains);
  CAMLassert(stw_domains.active_domains
             <= stw_domains.parked_domains);
#ifdef DEBUG
  /* Check here the invariant for early-exit in
     [caml_interrupt_all_signal_safe], because the latter must be
     async-signal-safe and one cannot CAMLassert inside it. */
  bool prev_has_interrupt_word = true;
  for (int i = 0; i < caml_params->max_domains; i++) {
    bool has_interrupt_word = all_domains[i].interruptor.interrupt_word != NULL;
    if (i < stw_domains.active_domains) CAMLassert(has_interrupt_word);
    if (!prev_has_interrupt_word) CAMLassert(!has_interrupt_word);
    prev_has_interrupt_word = has_interrupt_word;
  }
#endif
}

static int find_stw_domain(int start, int end, dom_internal *dom) {
  check_domain_limit(start);
  check_domain_limit(end);
  for (int i = start; i < end; i++)
  {
    if (stw_domains.domains[i] == dom)
      return i;
  }
  caml_fatal_error("find_stw_domain");
}
static int find_active_domain(dom_internal *dom) {
  int start = 0;
  int end = stw_domains.active_domains;
  return find_stw_domain(start, end, dom);
}
static int find_parked_domain(dom_internal *dom) {
  int start = stw_domains.active_domains;
  int end = stw_domains.parked_domains;
  return find_stw_domain(start, end, dom);
}

static void swap_stw_domains(int idx1, int idx2) {
  if (idx1 == idx2) return;
  dom_internal *dom1 = stw_domains.domains[idx1];
  dom_internal *dom2 = stw_domains.domains[idx2];
  stw_domains.domains[idx1] = dom2;
  stw_domains.domains[idx2] = dom1;
}


/* One needs to hold [all_domains_lock] to call any of the
   [stw_domains] transition functions that follow. */

static dom_internal* park_next_stopped_domain(void) {
  if (stw_domains.parked_domains == caml_params->max_domains)
    return NULL;

  dom_internal *dom = stw_domains.domains[stw_domains.parked_domains];
  stw_domains.parked_domains++;
  check_stw_domains();
  return dom;
}

static void stop_parked_domain(dom_internal *dom) {
  int idx = find_parked_domain(dom);
  stw_domains.parked_domains--;
  swap_stw_domains(idx, stw_domains.parked_domains);
  check_stw_domains();
}

static void activate_parked_domain(dom_internal *dom)
{
  int idx = find_parked_domain(dom);
  swap_stw_domains(stw_domains.active_domains, idx);
  stw_domains.active_domains++;
  check_stw_domains();
}

static void stop_active_domain(dom_internal* dom) {
  int idx = find_active_domain(dom);
  stw_domains.active_domains--;
  swap_stw_domains(idx, stw_domains.active_domains);

  idx = stw_domains.active_domains;
  stw_domains.parked_domains--;
  swap_stw_domains(idx, stw_domains.parked_domains);

  check_stw_domains();
}

CAMLexport CAMLthread_local caml_domain_state* caml_state;

#ifndef HAS_FULL_THREAD_VARIABLES
/* Export a getter for caml_state, to be used in DLLs */
CAMLexport caml_domain_state* caml_get_domain_state(void)
{
  return caml_state;
}
#endif

static CAMLthread_local uintnat previous_domain_id = -1;

CAMLexport
bool caml_thread_running_on_expected_domain(uintnat expected_unique_id)
{
  return (Caml_state->unique_id == expected_unique_id &&
          (previous_domain_id == -1 ||
            previous_domain_id == expected_unique_id));
}

CAMLexport void caml_thread_record_domain_id(uintnat domain_id)
{
  previous_domain_id = domain_id;
}

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

/* Note [minor heap layout]:

- The 'minor heaps reservation' is a contiguous address space of size
    [caml_minor_heap_max_wsz * caml_params->max_domains]
  reserved by [caml_init_domains]. Its boundaries are
    [caml_minor_heaps_start]
  and
    [caml_minor_heaps_end].

- Each domain gets a 'minor heap reservation', a segment of the global
  reservation of size [caml_minor_heap_max_wsz], whose boundaries are
    [domain_self->minor_heap_reservation_start]
  and
    [domain_self->minor_heap_reservation_end]

  These variables are accessed in [stw_resize_minor_heaps_reservation],
  synchronized by a global barrier.

- STW-participating domains have a 'minor heap arena', a memory block
  used for the minor heap, which is committed within its minor heap
  reservation. The arena has size [domain_state->minor_heap_wsz], and
  its boundaries are
     [domain_state->young_start]
   and
     [domain_state->young_end].

  Those [young_{start,end}] variables are never accessed by another
  domain, so they need no synchronization.

  New domains are created with a minor heap arena of size
  [caml_params->init_minor_heap_wsz].

  Domains commit their minor heap arena in
    [allocate_minor_heap_arena]
  which is called both at domain-initialization (by [domain_create])
  and if a request comes to change the size of the arena.

  They decommit their minor heap arena by calling
    [free_minor_heap_arena]
  before leaving the set of STW participants.

  If a domain uses [Gc.set] to change the size of its memory area, and
  the requested size is larger than its minor heap reservation, then
  we need to change the global minor heaps reservation. This is done
  by a STW section that first performs a minor collection and
  deallocates the arena of each domain. See
  [stw_resize_minor_heap_reservation].
*/

/* Size of the virtual memory reservation for the minor heap, per domain. */
uintnat caml_minor_heap_max_wsz;

/* The boundaries of the reserved address space for all minor heaps. */
CAMLexport uintnat caml_minor_heaps_start;
CAMLexport uintnat caml_minor_heaps_end;

Caml_inline void check_minor_heap(void) {
  caml_domain_state* domain_state = Caml_state;
  CAMLassert(domain_state->young_ptr == domain_state->young_end);

  caml_gc_log(
      "young_start: %p,"
      " young_end: %p,"
      " minor_heap_reservation_start: %p,"
      " minor_heap_reservation_end: %p,"
      " minor_heap_wsz: %" CAML_PRIuSZT " words",
      domain_state->young_start,
      domain_state->young_end,
      (value*)domain_self->minor_heap_reservation_start,
      (value*)domain_self->minor_heap_reservation_end,
      domain_state->minor_heap_wsz);
  CAMLassert(
    (/* uninitialized minor heap arena */
      domain_state->young_start == NULL
      && domain_state->young_end == NULL)
    ||
    (/* initialized minor heap arena */
      domain_state->young_start
      == (value*)domain_self->minor_heap_reservation_start
      && domain_state->young_end
         <= (value*)domain_self->minor_heap_reservation_end));
}


/* Operation on minor heap arenas */

static void free_minor_heap_arena(void) {
  caml_domain_state* domain_state = Caml_state;

  /* Exit early if the arena is not allocated. */
  if (domain_state->minor_heap_wsz == 0) return;

  caml_gc_log("trying to free old minor heap arena: %" CAML_PRIuSZT "k words",
              domain_state->minor_heap_wsz / 1024);

  check_minor_heap();

  caml_mem_decommit(
      (void*)domain_self->minor_heap_reservation_start,
      Bsize_wsize(domain_state->minor_heap_wsz));

  domain_state->minor_heap_wsz = 0;
  domain_state->young_start   = NULL;
  domain_state->young_end     = NULL;
  domain_state->young_ptr     = NULL;
  domain_state->young_trigger = NULL;
  domain_state->memprof_young_trigger = NULL;
  atomic_store_release(&domain_state->young_limit,
                   (uintnat) domain_state->young_start);
}

static int allocate_minor_heap_arena(asize_t wsize) {
  caml_domain_state* domain_state = Caml_state;

  CAMLassert (domain_state->minor_heap_wsz == 0);
  check_minor_heap();

  wsize = caml_norm_minor_heap_size(wsize);

  CAMLassert (wsize <= caml_minor_heap_max_wsz);

  caml_gc_log("trying to allocate minor heap arena: %" CAML_PRIuSZT "k words",
              wsize / 1024);

  if (!caml_mem_commit(
       (void*)domain_self->minor_heap_reservation_start,
       Bsize_wsize(wsize))) {
    return -1;
  }

#ifdef DEBUG
  {
    uintnat* start = (uintnat*)domain_self->minor_heap_reservation_start;
    uintnat* end = (uintnat*)(domain_self->minor_heap_reservation_start
                              + Bsize_wsize(wsize));
    for (uintnat* p = start; p < end; p++)
    {
      *p = Debug_free_minor;
    }
  }
#endif

  domain_state->minor_heap_wsz = wsize;

  domain_state->young_start = (value*)domain_self->minor_heap_reservation_start;
  domain_state->young_end =
      (value*)(domain_self->minor_heap_reservation_start + Bsize_wsize(wsize));
  domain_state->young_ptr = domain_state->young_end;
  /* Trigger a GC poll when half of the minor heap arena is filled. At
     that point, a major slice is scheduled. */
  domain_state->young_trigger = domain_state->young_start
         + (domain_state->young_end - domain_state->young_start) / 2;
  caml_memprof_set_trigger(domain_state);
  caml_reset_young_limit(domain_state);

  check_minor_heap();
  return 0;
}

int caml_reallocate_minor_heap_arena(asize_t wsize)
{
  free_minor_heap_arena();
  return allocate_minor_heap_arena(wsize);
}

/* Minor heaps reservation: initialization and resizing */

static void reserve_minor_heaps_reservation_from_stw_single(void) {
  void* heaps_base;
  uintnat minor_heaps_reservation_bsize;
  uintnat minor_heap_max_bsz;

  CAMLassert (caml_mem_round_up_pages(Bsize_wsize(caml_minor_heap_max_wsz))
          == Bsize_wsize(caml_minor_heap_max_wsz));

  minor_heap_max_bsz = (uintnat)Bsize_wsize(caml_minor_heap_max_wsz);
  minor_heaps_reservation_bsize = minor_heap_max_bsz * caml_params->max_domains;

  /* reserve memory space for minor heaps */
  heaps_base = caml_mem_map(minor_heaps_reservation_bsize, 1/* reserve_only */);
  if (heaps_base == NULL)
    caml_fatal_error("Not enough heap memory to reserve minor heaps");

  caml_minor_heaps_start = (uintnat) heaps_base;
  caml_minor_heaps_end =
    (uintnat) heaps_base + minor_heaps_reservation_bsize;

  caml_gc_log("new minor heaps reservation from %p to %p",
              (value*)caml_minor_heaps_start,
              (value*)caml_minor_heaps_end);

  for (int i = 0; i < caml_params->max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];

    uintnat domain_minor_heap_reservation =
      caml_minor_heaps_start
      + minor_heap_max_bsz * (uintnat)i;

    dom->minor_heap_reservation_start = domain_minor_heap_reservation;
    dom->minor_heap_reservation_end =
      domain_minor_heap_reservation + minor_heap_max_bsz;

    CAMLassert(dom->minor_heap_reservation_end
               <= caml_minor_heaps_end);
  }
}

static void unreserve_minor_heaps_reservation_from_stw_single(void) {
  uintnat size;

  caml_gc_log("unreserve_minor_heaps_reservation");

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

    /* uninitialize the minor heap reservation. */
    dom->minor_heap_reservation_start = dom->minor_heap_reservation_end = 0;
  }

  size = caml_minor_heaps_end - caml_minor_heaps_start;
  CAMLassert (Bsize_wsize(caml_minor_heap_max_wsz) * caml_params->max_domains
              == size);
  caml_mem_unmap((void *) caml_minor_heaps_start, size);
}

static
void domain_resize_heaps_reservation_from_stw_single(uintnat new_minor_wsz)
{
  CAML_EV_BEGIN(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
  caml_gc_log("stw_resize_minor_heaps_reservation: unreserve");

  unreserve_minor_heaps_reservation_from_stw_single();
  /* new_minor_wsz is page-aligned because caml_norm_minor_heap_size has
     been called to normalize it earlier.
  */
  caml_minor_heap_max_wsz = new_minor_wsz;
  caml_gc_log("stw_resize_minor_heaps_reservation: reserve");
  reserve_minor_heaps_reservation_from_stw_single();
  /* The call to [reserve_minor_heaps_reservation_from_stw_single] makes a new
     reservation, and it also updates the reservation boundaries of each
     domain by mutating its [minor_heap_reservation_start{,_end}] variables.

     These variables are synchronized by the fact that we are inside
     a STW section: no other domains are running in parallel, and
     the participating domains will synchronize with this write by
     exiting the barrier, before they read those variables in
     [allocate_minor_heap_arena] below. */
  CAML_EV_END(EV_DOMAIN_RESIZE_HEAP_RESERVATION);
}

static void
stw_resize_minor_heaps_reservation(caml_domain_state* domain,
                                  void* minor_wsz_data,
                                  int participating_count,
                                  caml_domain_state** participating) {
  caml_gc_log("stw_resize_minor_heaps_reservation: "
              "caml_empty_minor_heap_no_major_slice_from_stw");
  caml_empty_minor_heap_no_major_slice_from_stw(
    domain, NULL, participating_count, participating);

  // We must read this now because [free_minor_heap_arena] will zero it.
  uintnat minor_heap_wsz = Caml_state->minor_heap_wsz;

  caml_gc_log("stw_resize_minor_heaps_reservation: free_minor_heap_arena");
  free_minor_heap_arena();

  Caml_global_barrier_if_final(participating_count) {
    uintnat new_minor_wsz = (uintnat) minor_wsz_data;
    domain_resize_heaps_reservation_from_stw_single(new_minor_wsz);
  }

  caml_gc_log("stw_resize_minor_heaps_reservation: allocate_minor_heap_arena");
  /* Note: each domain allocates its own minor heap arena. This seems
     important to get good NUMA behavior. We don't want a single
     domain to allocate all arenas, which could create locality issues
     we don't understand very well. */
  if (allocate_minor_heap_arena(minor_heap_wsz) < 0) {
    caml_fatal_error("Fatal error: No memory for minor heap arena");
  }
}

void caml_update_minor_heap_max(uintnat requested_wsz) {
  caml_gc_log("Changing heap_max_wsz from %" CAML_PRIuNAT
              " to %" CAML_PRIuNAT ".",
              caml_minor_heap_max_wsz, requested_wsz);
  while (requested_wsz > caml_minor_heap_max_wsz) {
    caml_try_run_on_all_domains(
      &stw_resize_minor_heaps_reservation, (void*)requested_wsz, 0);
  }
  check_minor_heap();
}


/* Domain creation. */

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

  d = park_next_stopped_domain();

  if (d == NULL)
    goto domain_parking_failure;

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
      goto domain_state_init_failure;
    d->state = domain_state;
  } else {
    domain_state = d->state;
  }

  /* Note: until we take d->domain_lock, the domain_state may still be
   * shared with a domain which is terminating (see
   * caml_domain_terminate). */

  caml_plat_lock_blocking(&d->domain_lock);

  /* This is the first thing we do after acquiring the domain lock,
     so that [caml_domain_alone()] returns accurate result even
     during domain initialization. */
  atomic_fetch_add(&caml_num_domains_running, 1);

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

  domain_state->sweep_work_done_between_slices = 0;
  domain_state->mark_work_done_between_slices = 0;

  /* the minor heap arena will be initialized by
     [allocate_minor_heap_arena] below. */
  domain_state->minor_heap_wsz = 0;
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

  if(allocate_minor_heap_arena(initial_minor_heap_wsize) < 0) {
    goto allocate_minor_heap_arena_failure;
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

  domain_state->c_stack = NULL;
  domain_state->exn_handler = NULL;

  domain_state->action_pending = 0;

  domain_state->gc_regs_buckets = NULL;
  domain_state->gc_regs = NULL;

  domain_state->allocated_words = 0;
  domain_state->allocated_words_direct = 0;
  domain_state->allocated_words_suspended = 0;
  domain_state->allocated_words_resumed = 0;
  domain_state->current_ramp_up_allocated_words_diff = 0;
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

  activate_parked_domain(d);
  goto domain_init_complete;

alloc_main_stack_failure:
create_stack_cache_failure:
  caml_remove_generational_global_root(&domain_state->dls_root);
  free_minor_heap_arena();
allocate_minor_heap_arena_failure:
  caml_teardown_major_gc();
init_major_gc_failure:
  caml_orphan_shared_heap(d->state->shared_heap);
  caml_free_shared_heap(d->state->shared_heap);
  domain_state->shared_heap = NULL;
init_shared_heap_failure:
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = NULL;
alloc_minor_tables_failure:
  caml_memprof_delete_domain(domain_state);
init_memprof_failure:
  caml_plat_unlock(&d->domain_lock);
  domain_self = NULL;

  atomic_fetch_add(&caml_num_domains_running, -1);

domain_state_init_failure:
  stop_parked_domain(d);
domain_parking_failure:

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

  reserve_minor_heaps_reservation_from_stw_single();
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
#define Term_mutex(sync) (Mutex_val(Field(sync, 1)))
#define Term_condition(sync) (Condition_val(Field(sync, 2)))

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
  uintnat unique_id; /* out */
  const char *error; /* out: set iff status is Dom_failed */
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

static value install_backup_thread_exn (dom_internal* di)
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

#ifndef _WIN32
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  if (err != 0)
      return caml_check_error_exn(err, "failed to create domain backup thread");
  pthread_detach(di->backup_thread);
  return Val_unit;
}

static void terminate_backup_thread(dom_internal *di)
{
  CAMLassert(!caml_bt_is_self());

  if (backup_thread_running(di)) {
    atomic_store_release(&di->backup_thread_msg, BT_TERMINATE);
    /* Wakeup backup thread if it is sleeping */
    caml_plat_lock_blocking(&di->interruptor.lock);
    caml_plat_broadcast(&di->interruptor.cond);
    caml_plat_unlock(&di->interruptor.lock);
    caml_plat_signal(&di->domain_cond);
  }
}

static value caml_domain_initialize_default_exn(void)
{
  return Val_unit;
}

static void caml_domain_stop_default(void)
{
  return;
}

static void caml_domain_external_interrupt_hook_default(void)
{
  return;
}

CAMLexport value (*caml_domain_initialize_hook_exn)(void) =
   caml_domain_initialize_default_exn;

CAMLexport void (*caml_domain_stop_hook)(void) =
   caml_domain_stop_default;

CAMLexport void (*caml_domain_external_interrupt_hook)(void) =
   caml_domain_external_interrupt_hook_default;

CAMLexport _Atomic caml_timing_hook caml_domain_terminated_hook =
  (caml_timing_hook)NULL;

static value make_finished(caml_result result)
{
  CAMLparam0();
  CAMLlocal2(res, bt);
  if (caml_result_is_exception(result)) {
    res = result.data; /* Ensure that [result.data] is rooted before
                          subsequent allocations */
    /* res = exn */
    bt = caml_get_exception_raw_backtrace(Val_unit);
    res = caml_alloc_2(0, res, bt);
    /* res = (exn, bt) */
    res = caml_alloc_1(1 /* Error */, res);
    /* res = Error (exn, bt) */
    res = caml_alloc_1(0 /* Finished */, res);
    /* res = Finished(Error(exn, bt)) */
  } else {
    res = caml_alloc_1(0 /* Ok */,result.data);
    /* res = Ok v */
    res = caml_alloc_1(0, res);
    /* res = Finished(Ok v) */
  }
  CAMLreturn(res);
}

static void handshake_success(struct domain_startup_params *p)
{
  caml_plat_lock_blocking(&p->parent->interruptor.lock);
  p->status = Dom_started;
  p->unique_id = domain_self->interruptor.unique_id;
  caml_plat_broadcast(&p->parent->interruptor.cond);
  caml_plat_unlock(&p->parent->interruptor.lock);
}

static void handshake_failure(struct domain_startup_params *p, const char *str)
{
  caml_plat_lock_blocking(&p->parent->interruptor.lock);
  p->status = Dom_failed;
  p->error = str;
  caml_plat_broadcast(&p->parent->interruptor.cond);
  caml_plat_unlock(&p->parent->interruptor.lock);
  caml_gc_log("Failed to create domain");
}

/* Synchronize with joining domains. */
static void sync_result(value term_sync, value res)
{
  CAMLparam2(term_sync, res);

  /* We only call functions that do not raise exceptions, because this
     would be bad for us at this point. */

  /* Synchronize with joining domains. To avoid deadlocks, we must not
     block. In particular, systhreads are still running on this
     domain. */
  caml_plat_lock_non_blocking(Term_mutex(term_sync));

  /* Store result */
  volatile value *state = Term_state(term_sync);
  CAMLassert(!Is_block(*state));
  caml_modify(state, res);

  /* Signal all the waiting domains to be woken up */
  caml_plat_broadcast(Term_condition(term_sync));

  /* The mutex is unlocked after the domain is destroyed; we must
     release the local roots before this happens. */
  CAMLreturn0;
}

static void sync_and_terminate(struct domain_ml_values *ml_values,
                               caml_result res)
{
  /* Allocate the result value. */
  value v = make_finished(res);
  sync_result(ml_values->term_sync, v);
  /* This domain currently holds a lock for [mut], which is kept alive
     by a global root inside ml_values. */
  caml_plat_mutex *mut = Term_mutex(ml_values->term_sync);
  /* Join all systhreads on this domain and release the runtime state. */
  caml_domain_terminate(false);
  /* This domain has signaled all the waiting domains to be woken up.
     We unlock [mut] to release the joining domains. The unlock is
     done after [caml_domain_terminate] to ensure that this domain has
     released all of its runtime state. The domain no longer exists at
     this point but we can use [caml_plat_unlock]. */
  caml_plat_unlock(mut);
  caml_plat_assert_all_locks_unlocked();
}

static void* domain_thread_func(void* v)
{
  struct domain_startup_params* p = v;
  struct domain_ml_values *ml_values = p->ml_values;
  /* This thread now owns ml_values */

  /* Create domain and do handshake with parent */
#ifndef _WIN32
  void * signal_stack = caml_init_signal_stack();
  if (signal_stack == NULL) {
    handshake_failure(p, "failed to allocate domain: signal stack");
    goto out1;
  }
#endif

  domain_create(caml_params->init_minor_heap_wsz, p->parent->state);
  if (domain_self == NULL) {
    handshake_failure(p, "failed to allocate domain: domain_create");
    goto out2;
  }
  domain_self->tid = pthread_self();
  /* this domain is now part of the STW participant set */
  handshake_success(p);

  /* v and p must no longer be accessed */
  v = NULL;
  p = NULL;

  value exn = install_backup_thread_exn(domain_self);
  if (Is_exception_result(exn)) {
    sync_and_terminate(ml_values, Result_exception(exn));
    goto out2;
  }

  caml_gc_log("Domain starting (unique_id = %" CAML_PRIuNAT ")",
              domain_self->interruptor.unique_id);
  CAML_EV_LIFECYCLE(EV_DOMAIN_SPAWN, getpid());

  exn = caml_domain_initialize_hook_exn();
  if (Is_exception_result(exn)) {
    sync_and_terminate(ml_values, Result_exception(exn));
    goto out2;
  }

  /* release callback early;
     see the [note about callbacks and GC] in callback.c */
  value unrooted_callback = ml_values->callback;
  caml_modify_generational_global_root(&ml_values->callback, Val_unit);
  caml_result res = caml_callback_res(unrooted_callback, Val_unit);
  sync_and_terminate(ml_values, res);
  /* fall through */

 out2:
#ifndef _WIN32
  caml_free_signal_stack(signal_stack);
 out1:
#endif
  /* [ml_values] must be freed after unlocking its [term_sync] mutex.
     This ensures that the [term_sync] field is only removed from the
     root set after the mutex is unlocked. Otherwise, there is a risk
     of it being destroyed by [caml_mutex_finalize] while it remains
     locked, leading to undefined behaviour. */
  free_domain_ml_values(ml_values);
  return NULL;
}

/* Note: [caml_domain_spawn] and [caml_domain_alone()].

   The use of [caml_domain_alone()] to implement sequential fast-path
   requires that no other domain is operating in parallel. This is
   indeed the case when [caml_domain_alone()] is observed while
   holding the domain lock:

   1. When a domain exits, it is careful to decrement
      [caml_num_domains_running] as the very last step, so that
      [caml_domain_alone()] does not return [true] while its mutator
      or domain-termination cleanup logic are still in progress.

   2. When a domain starts, it increments [caml_num_domains_running]
      immediately after taking the domain lock, and its parent domain
      blocks waiting for the child set the [Dom_started] flag, which
      happens after this increment. Neither the parent nor the child
      can wrongly observe [caml_domain_alone()] while the other may be
      running code with its domain lock held.
*/


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

  /* Domain 0 does not need a backup thread when it is the sole
     domain. We create its backup thread before spawning domain 1. */
  domain_self->tid = pthread_self();
  if (!backup_thread_running(domain_self)) {
    value res = install_backup_thread_exn(domain_self);
    if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  }

  p.parent = domain_self;
  p.status = Dom_starting;

  p.ml_values =
      (struct domain_ml_values*) caml_stat_alloc(
                                    sizeof(struct domain_ml_values));
  init_domain_ml_values(p.ml_values, callback, term_sync);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  if (err) {
    free_domain_ml_values(p.ml_values);
    caml_check_error(err, "failed to create domain thread: pthread_create");
  }

  /* p.ml_values is now owned by the new domain */

  /* Handshake with the new domain. While waiting for the child thread
     to start up, we need to service any stop-the-world requests as
     they come in. */
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
    caml_failwith(p.error);
  }

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

  Bearing all that in mind, the spec of this function, which drives
  all STW sections, is as follows:

  `caml_try_run_on_all_domains_with_spin_work
    (sync, handler, data, leader_setup, enter_spin_callback, enter_spin_data)`

  Try to run an STW section in which all domains will run
  `handler(domain, data, N, p)` (where `N` and `p` define the
  participants in the STW section).

  To create the STW section, we must first become the STW leader as
  detailed above. If this fails, return 0.

  Once we are STW leader - so outside any other STW section - but
  before the other domains have been notified of this new STW section,
  we call `leader_setup(domain, data)`.

  If `sync` is non-zero, all these calls are synchronous - none are
  called until after all domains have arrived at the "entry barrier".
  If `enter_spin_callback` is non-NULL, then while any domain is
  waiting for other domains to reach the entry barrier, it may run
  `enter_spin_callback(domain, enter_spin_data)` repeatedly (stopping
  if it ever returns zero).

  If `sync` is zero, there is no entry barrier, so no synchronization
  happens. We still become the STW leader, so no other STW can
  interfere with this one.
*/
int caml_try_run_on_all_domains_with_spin_work(
  int sync,
  void (*handler)(caml_domain_state*, void*, int, caml_domain_state**),
  void* data,
  void (*leader_setup)(caml_domain_state*, void*),
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
  stw_request.num_domains = stw_domains.active_domains;
  /* stw_request.barrier doesn't need resetting */
  atomic_store_release(&stw_request.num_domains_still_processing,
                       stw_domains.active_domains);

  int is_alone = stw_request.num_domains == 1;
  int should_sync = sync && !is_alone;

  if (should_sync) {
    caml_plat_barrier_reset(&stw_request.domains_still_running);
  }

  if( leader_setup ) {
    leader_setup(domain_state, data);
  }

#ifdef DEBUG
  {
    int domains_participating = 0;
    for(i=0; i<caml_params->max_domains; i++) {
      if(all_domains[i].interruptor.running)
        domains_participating++;
    }
    CAMLassert(domains_participating == stw_domains.active_domains);
    CAMLassert(domains_participating > 0);
  }
#endif

  /* Next, interrupt all domains */
  for(i = 0; i < stw_domains.active_domains; i++) {
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
  void (*leader_setup)(caml_domain_state*, void *))
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
  void (*leader_setup)(caml_domain_state*, void *))
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

/*  This function can be called from arbitrary code, possibly running
    concurrently with the OCaml runtime, as long as it synchronized
    with the runtime startup which initialized [all_domains] and
    [caml_params].
    Returns [-1] on failure -- if the provided unique id is
    not assigned to a currently-running domain.
*/
intnat caml_find_index_of_running_domain(uintnat dom_unique_id)
{
  for (int i = 0; i < caml_params->max_domains; i++) {
    dom_internal *d = &all_domains[i];

    /* See [caml_interrupt_all_signal_safe] above for synchronization
       and early-exit comments. */
    atomic_uintnat * interrupt_word =
      atomic_load_acquire(&d->interruptor.interrupt_word);
    if (interrupt_word == NULL) return -1;

    if (d->interruptor.unique_id == dom_unique_id) return i;
  }
  return -1;
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
         fails when the minor heap arena is full. */
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

static bool marking_and_sweeping_done(caml_domain_state *domain_state)
{
  return (domain_state->marking_done
          && domain_state->sweeping_done);
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

    /* Orphaning does not happen in [Phase_sweep_main]. */
    CAMLassert (caml_gc_phase != Phase_sweep_main);
    caml_orphan_ephemerons(domain_state);
    caml_orphan_finalisers(domain_state);

    /* Orphaning ephemerons and finalizers may create new marking or
       sweeping work, so we may need to mark and/or sweep again. */

    /* No need to check for interrupts if we are the last domain running. */
    if (last) {
      CAML_EV_LIFECYCLE(EV_DOMAIN_TERMINATE, getpid());
      break;
    }

    /* If new marking or sweeping work appeared during orphaning,
       run a new loop iteration. */
    if (!marking_and_sweeping_done(domain_state))
      continue;

    /* Orphan the local shared heap.
       This is only valid when [sweeping_done], and does
       not create any new major GC work. */
    caml_orphan_shared_heap(domain_state->shared_heap);
    CAMLassert(marking_and_sweeping_done(domain_state));

    /* Take the all_domains_lock to try and exit the STW participant set
       without racing with a STW section being triggered. */
    caml_plat_lock_blocking(&all_domains_lock);

    /* The interaction of termination and major GC is quite subtle.

       At the end of the major GC, we decide the number of domains to mark and
       sweep for the next cycle. If a STW section has been started, it will
       require this domain to participate, which in turn could involve a major
       GC cycle. This would then require finish marking and sweeping again in
       order to decrement the globals [num_domains_to_mark] and
       [num_domains_to_sweep] (see major_gc.c). We do this by running a new
       loop iteration.
     */
    if (!caml_incoming_interrupts_queued()) {
      finished = 1;
      s->terminating = 0;
      s->running = 0;

      /* Remove this domain from stw_domains.
         (This will only be observed after [all_domains_lock] is released.) */
      stop_active_domain(domain_self);

      /* The minor heap arena is only valid for STW-participating domains,
         so we free it when we stop STW participation. */
      free_minor_heap_arena();

      /* Signal the interruptor condition variable
         because the backup thread may be waiting on it. */
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

  if (!last) caml_assert_shared_heap_is_empty(domain_state->shared_heap);

  /* [domain_state] may be reused by a fresh domain here, now that we
     have done [stop_active_domain] and released the
     [all_domains_lock]. In particular, we cannot touch
     [domain_self->interruptor] after here because it may be reused.

     However, [domain_create()] won't touch the domain state until
     it has claimed the [domain_lock], so we hang onto that while we are
     tearing down the state. */

  /* Delete the domain state from statmemprof after any promotion
   * (etc) done by this domain: any remaining memprof state will be
   * handed over to surviving domains. */
  caml_memprof_delete_domain(domain_state);

  caml_remove_generational_global_root(&domain_state->dls_root);
  caml_remove_generational_global_root(&domain_state->backtrace_last_exn);
  caml_stat_free(domain_state->final_info);
  caml_stat_free(domain_state->ephe_info);
  caml_free_intern_state();
  caml_free_extern_state();
  caml_teardown_major_gc();

  /* At this point, we know that the shared heap has been orphaned,
     except if [last], if we are the last domain. In that case we
     finalise all unswept objects and orphan the shared heap now. */
  if (last) {
    /* First adopt all orphan pools, to avoid missing unswept objects. */
    caml_adopt_all_orphan_heaps(domain_state->shared_heap);

    /* Call all custom finalisers of unswept objects. */
    caml_finalise_heap();

    /* Then orphan all pools again. */
    caml_orphan_shared_heap(domain_state->shared_heap);
  }
  caml_assert_shared_heap_is_empty(domain_state->shared_heap);

  caml_free_shared_heap(domain_state->shared_heap);
  domain_state->shared_heap = NULL;
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = NULL;

  /* At this point, the stats of the domain must be empty.
     - heap stats were orphaned by [caml_orphan_shared_heap]
     - alloc stats were orphaned by [caml_orphan_alloc_stats]
     - the sampled copy in [sampled_gc_stats] was cleared by the minor
       collection performed by [caml_empty_minor_heaps_once()], see
       the termination-specific logic in
       [caml_collect_gc_stats_sample_stw].
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
#ifdef HAVE_PTHREAD_CANCEL
      (void)pthread_cancel(domain_self->tid);
#endif
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

CAMLprim value caml_domain_count(value unused)
{
  return Val_long(atomic_load_relaxed(&caml_num_domains_running));
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
