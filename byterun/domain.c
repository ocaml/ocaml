#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "domain.h"
#include "platform.h"
#include "custom.h"
#include "major_gc.h"
#include "shared_heap.h"
#include "memory.h"
#include "fail.h"
#include "globroots.h"
#include "signals.h"
#include "alloc.h"
#include "startup.h"
#include "fiber.h"
#include "callback.h"
#include "minor_gc.h"

/* Since we support both heavyweight OS threads and lightweight
   userspace threads, the word "thread" is ambiguous. This file deals
   with OS-level threads, called "domains".

   A domain is "alive" from when it is created until it terminates,
   and is "active" during that time except when blocked in a
   long-running system call / C call. Such long-running calls are
   wrapped by caml_domain_deactivate and caml_domain_activate.
*/


struct dom_internal {
  /* readonly fields, initialised and never modified */
  atomic_uintnat* interrupt_word_address;
  struct domain state;

  /* fields accessed concurrently by several threads */
  atomic_uintnat is_active; /* domain is not in a blocking section */

  /* fields accessed by the domain itself and the domain requesting an RPC */
  atomic_uintnat rpc_request;
  domain_rpc_handler rpc_handler;
  void* rpc_data;
  atomic_uintnat* rpc_completion_signal;
  

  /* fields protected by roots_lock */
  caml_plat_mutex roots_lock;
  int sampled_roots_dirty;


  /* fields only modified during STW */
  struct dom_internal* next;

  /* fields protected by all_domains_lock */
  int running;

  /* readonly */
  uintnat minor_heap_base;
};
typedef struct dom_internal dom_internal;


/* possible values of domain->rpc_request */
enum { RPC_IDLE = 0, 
       RPC_REQUEST_INITIALISING = 1, 
       RPC_REQUEST_SENT = 2 };

#define Max_domains (1 << Minor_heap_sel_bits)


static caml_plat_mutex all_domains_lock;
static struct dom_internal all_domains[Max_domains];
static uintnat minor_heaps_base;
static __thread dom_internal* domain_self;


CAMLexport __thread char *caml_young_start = NULL, *caml_young_end = NULL;



asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = caml_mem_round_up_pages(1);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  Assert(page_size * 2 < (1 << Minor_heap_align_bits));
  max = (1 << Minor_heap_align_bits) - page_size * 2;

  if (bs > max) bs = max;

  return bs;
}

void caml_reallocate_minor_heap(asize_t size)
{
  uintnat mem = domain_self->minor_heap_base;
  Assert(caml_young_ptr == caml_young_end);

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit((void*)mem, (1 << Minor_heap_align_bits));

  size = caml_norm_minor_heap_size(size);

  /* leave a guard page at the start */
  mem = caml_mem_round_up_pages(mem + 1);
  caml_mem_commit((void*)mem, size);

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)mem;
    for (; p < (uintnat*)(mem + size); p++) *p = Debug_uninit_align;
  }
#endif

  caml_minor_heap_size = size;
  caml_young_start = (char*)mem;
  caml_young_end = (char*)(mem + size);
  caml_young_ptr = caml_young_end;
  caml_update_young_limit((uintnat)caml_young_start);
}

/* must be run on the domain's thread */
static void activate_domain();

static dom_internal* create_domain(uintnat initial_minor_heap_size, int is_main) {
  int i;
  dom_internal* d = 0;
  Assert (domain_self == 0);

  caml_plat_lock(&all_domains_lock);

  for (i = 0; i < Max_domains; i++) {
    if (!all_domains[i].running) {
      d = &all_domains[i];
      break;
    }
  }

  if (d) {
    d->running = 1;
    atomic_store_rel(&d->is_active, 0);
    d->state.is_main = 0;
    d->state.internals = d;
    /* FIXME: shutdown RPC? */
    atomic_store_rel(&d->rpc_request, RPC_IDLE);

    domain_self = d;
    caml_young_start = caml_young_end = caml_young_ptr = 0;
    d->interrupt_word_address = &caml_young_limit;
    caml_plat_mutex_init(&d->roots_lock);

    d->state.shared_heap = caml_init_shared_heap();
    caml_init_major_gc();
    caml_reallocate_minor_heap(initial_minor_heap_size);

    d->state.runqueue = caml_init_runqueue();

    d->state.remembered_set = &caml_remembered_set;
    d->state.local_roots = &caml_local_roots;
#ifdef NATIVE_CODE
    /* FIXME */
#else
    d->state.current_stack = &caml_current_stack;
#endif
    d->state.young_ptr = &caml_young_ptr;
    d->state.young_end = &caml_young_end;
    d->state.mark_stack = &caml_mark_stack;
    d->state.mark_stack_count = &caml_mark_stack_count;
  }
  caml_plat_unlock(&all_domains_lock);

  return d;
}


void caml_init_domains(uintnat minor_size) {
  int i;
  uintnat size;
  void* heaps_base;

  /* sanity check configuration */
  if (caml_mem_round_up_pages(1 << Minor_heap_align_bits) != (1 << Minor_heap_align_bits))
    caml_fatal_error("Minor_heap_align_bits misconfigured for this platform");

  /* reserve memory space for minor heaps */
  size = (uintnat)1 << (Minor_heap_sel_bits + Minor_heap_align_bits);

  /* To ensure Is_foreign gives no false positives, we reserve twice
     the address space needed and only use the first half */
  heaps_base = caml_mem_map(size*2, size*2, 1 /* reserve_only */);
  if (!heaps_base) caml_raise_out_of_memory();

  minor_heaps_base = (uintnat) heaps_base;

  caml_plat_mutex_init(&all_domains_lock);

  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];
    caml_plat_mutex_init(&dom->roots_lock);
    dom->running = 0;
    dom->state.id = i;
    dom->minor_heap_base =
      minor_heaps_base +
      (uintnat)(1 << Minor_heap_align_bits) * (uintnat)i;
  }


  dom_internal* dom = create_domain(minor_size, 1);
  if (!dom) caml_fatal_error("Failed to create main domain");

  caml_init_global_roots();
  caml_init_signal_handling();

  activate_domain();
}


static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();

  /* FIXME: proper domain termination and reuse */
  /* interrupt_word_address must not go away */
  pause();
}


static void domain_filter_live() {
  int i;
  caml_plat_lock(&all_domains_lock);
  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if (!atomic_load_acq(&d->is_active)) {
      d->sampled_roots_dirty = 1;
    }
  }
  caml_plat_unlock(&all_domains_lock);
}

struct domain_startup_params {
  caml_plat_event ev;
  value callback;
  dom_internal* newdom;
};

static void* domain_thread_func(void* v) {
  CAMLparam0();
  CAMLlocal1(callback);
  struct domain_startup_params* p = v;

  callback = p->callback;
  p->newdom = create_domain(caml_startup_params.minor_heap_init, 0);
  caml_plat_event_trigger(&p->ev);

  if (p->newdom) {
    caml_gc_log("Domain starting");
    activate_domain();
    /* FIXME exceptions */
    caml_callback_exn(callback, Val_unit);
    domain_terminate();
  } else {
    caml_gc_log("Failed to create domain");
  }
  CAMLreturnT(void*, 0);
}


CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  struct domain_startup_params p;
  pthread_t th;
  int err;

  caml_plat_event_init(&p.ev);
  p.callback = caml_promote(&domain_self->state, callback);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  if (err) {
    caml_failwith("failed to create domain thread");
  }

  caml_enter_blocking_section();
  caml_plat_event_wait(&p.ev);
  caml_leave_blocking_section();

  if (p.newdom) {
    /* successfully created a domain */
    pthread_detach(th);
  } else {
    /* failed */
    void* r;
    pthread_join(th, &r);
    caml_failwith("failed to allocate domain");
  }

  CAMLreturn (Val_unit);
}

struct domain* caml_domain_self()
{
  return domain_self ? &domain_self->state : 0;
}

struct domain* caml_random_domain()
{
  dom_internal* d;
  int i, r = rand(), len = 0;

  caml_plat_lock(&all_domains_lock);
  for (i = 0; i < Max_domains; i++)
    if (all_domains[i].running) len++;
  r %= len;
  for (i = 0; r > 0; i++)
    if (all_domains[i].running) r--;
  d = &all_domains[i];
  Assert( 0 <= i && i < Max_domains && d->running);
  caml_plat_unlock(&all_domains_lock);

  return &d->state;
}

struct domain* caml_owner_of_young_block(value v) {
  Assert(Is_minor(v));
  int heap_id = ((uintnat)v - minor_heaps_base) /
    (1 << Minor_heap_align_bits);
  return &all_domains[heap_id].state;
}


CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(domain_self->state.id);
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);
__thread atomic_uintnat caml_young_limit = {0};
__thread uintnat desired_caml_young_limit = 0;
static __thread int volatile caml_force_major_slice = 0;

static atomic_uintnat stw_requested;

/* update caml_young_limit, being careful not to lose interrupts */
void caml_update_young_limit(uintnat new_val) {
  Assert(new_val < INTERRUPT_MAGIC);

  /* Either the CAS succeeds, and we have updated caml_young_limit,
     or else the CAS fails because there's an interrupt pending,
     so we leave the interrupt pending */
  atomic_cas(&caml_young_limit, desired_caml_young_limit, new_val);
  desired_caml_young_limit = new_val;
}



static void interrupt_domain(dom_internal* d) {
  atomic_store_rel(d->interrupt_word_address, INTERRUPT_MAGIC);
}

static void request_stw() {
  int i;
  atomic_store_rel(&stw_requested, 1);
  /* interrupt all running domains */
  caml_plat_lock(&all_domains_lock);
  for (i = 0; i < Max_domains; i++) {
    if (all_domains[i].running && all_domains[i].interrupt_word_address)
      interrupt_domain(&all_domains[i]);
  }
  caml_plat_unlock(&all_domains_lock);
}


void caml_trigger_stw_gc() {
  request_stw();
}

void caml_interrupt_self() {
  interrupt_domain(domain_self);
}

/* Arrange for a garbage collection to be performed on the current domain
   as soon as possible */
void caml_urge_major_slice (void)
{
  caml_force_major_slice = 1;
  caml_interrupt_self();
}


static void stw_phase(void);
static void check_rpc(void);

void caml_handle_gc_interrupt(int required_words) {
  if (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    while (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
      atomic_cas(&caml_young_limit, INTERRUPT_MAGIC, desired_caml_young_limit);
    }
    check_rpc();
    if (atomic_load_acq(&stw_requested)) {
      stw_phase();
    }
  }

  if (((uintnat)caml_young_ptr - Bhsize_wosize(required_words) <
       desired_caml_young_limit) ||
      caml_force_major_slice) {
    /* out of minor heap or collection forced */
    caml_force_major_slice = 0;
    caml_minor_collection();
  }
}

void caml_domain_spin() {
  check_rpc();
  cpu_relax();
}




/* Once per GC cycle, the world stops and all active domains (those
   not engaged in a long-running system call) synchronise. Below is
   the implementation of the synchronisation barrier. It's more or
   less a standard linear barrier, except it needs to handle the
   number of participating domains varying (as domains become active
   or inactive). */

/* 3 fields are packed into domstat, so they may be modified atomically
    phase   (1 bit)   - current phase of barrier executing
    waiters (15 bits) - number of domains currently waiting on barrier
    active  (15 bits) - number of domains currently active */
    
static atomic_uintnat domstat;
#define DOMSTAT_PHASE(stat)   ((stat) & 1)
#define DOMSTAT_WAITERS(stat) (((stat) & 0xffff) >> 1)
#define DOMSTAT_ACTIVE(stat)  ((stat) >> 16)

/* Activating a domain blocks until the current stop-the-world phase
   is complete */
static void activate_domain() 
{
  /* add 1 to DOMSTAT_ACTIVE when DOMSTAT_PHASE and DOMSTAT_WAITERS are zero */
  while (1) {
    uintnat stat = atomic_load_acq(&domstat);
    if (DOMSTAT_PHASE(stat) == 0 && DOMSTAT_WAITERS(stat) == 0) {
      /* DOMSTAT_ACTIVE += 1 */
      if (atomic_cas(&domstat, stat, stat + 0x10000)) break;
    }
    cpu_relax();
  }

  atomic_store_rel(&domain_self->is_active, 1);
  caml_plat_lock(&domain_self->roots_lock);
}

CAMLexport void caml_leave_blocking_section() {
  activate_domain();
  caml_restore_stack_gc();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_save_stack_gc();
  domain_self->sampled_roots_dirty = 1;
  caml_plat_unlock(&domain_self->roots_lock);
  atomic_store_rel(&domain_self->is_active, 0);
  /* subtract 1 from DOMSTAT_ACTIVE */
  atomic_fetch_add(&domstat, -0x10000);
}

struct rpc_domain_takeover {
  dom_internal* target;
  struct rpc_domain_takeover* next;
};
static int try_lock_domain(struct rpc_domain_takeover* takeover, 
                           dom_internal* target);
static void unlock_domain(struct rpc_domain_takeover* takeover);

static void gc_inactive_domains()
{
  while (1) {
    /* find a running, inactive, dirty domain
       and try to take its roots_lock */
    int i;
    struct dom_internal* found = 0;
    struct rpc_domain_takeover takeover;
    caml_plat_lock(&all_domains_lock);
    for (i = 0; i < Max_domains; i++) {
      struct dom_internal* d = &all_domains[i];
      if (d->running && try_lock_domain(&takeover, d)) {
        if (d->sampled_roots_dirty) {
          d->sampled_roots_dirty = 0;
          found = d;
          break;
        } else {
          unlock_domain(&takeover);
        }
      }
    }
    caml_plat_unlock(&all_domains_lock);

    if (!found) break;

    /* If we found one, GC it */
    caml_gc_log("GCing inactive domain [%02d]", found->state.id);

    while (caml_sweep(found->state.shared_heap, 10) <= 0);

    caml_do_sampled_roots(&caml_darken, &found->state);
    caml_empty_mark_stack();

    caml_cycle_heap(found->state.shared_heap);

    unlock_domain(&takeover);
  }
}

/* synchronise all domains. returns 1 if this was the last domain to enter the barrier */
static int barrier_enter(int phase) {
  uintnat oldstat = atomic_fetch_add(&domstat, 2); /* DOMSTAT_WAITERS += 1 */
  uintnat ticket = DOMSTAT_WAITERS(oldstat) + 1; /* we just set DOMSTAT_WAITERS to ticket */
  Assert (DOMSTAT_WAITERS(oldstat) < DOMSTAT_ACTIVE(oldstat));
  Assert (DOMSTAT_PHASE(oldstat) == phase);
  
  while (1) {
    uintnat newstat = atomic_load_acq(&domstat);
    if (DOMSTAT_WAITERS(newstat) != ticket || DOMSTAT_PHASE(newstat) != phase) {
      /* we are no longer the most recent domain to enter the barrier */
      /* wait for the barrier to complete */
      while (1) {
        uintnat stat = atomic_load_acq(&domstat);
        if (DOMSTAT_PHASE(stat) != phase) break;
        check_rpc();
        cpu_relax();
      }
      return 0;
    } else if (DOMSTAT_ACTIVE(newstat) == DOMSTAT_WAITERS(newstat)) {
      /* we are the most recent domain to enter the barrier, and there
         are no more active domains to enter the barrier, either because
         we were last or the remaining domains deactivated. */
      Assert (DOMSTAT_PHASE(newstat) == phase);
      Assert (DOMSTAT_WAITERS(newstat) == ticket);
      return 1;
    }
    check_rpc();
    cpu_relax();
  }
}

/* the domain for which barrier_enter returns 1 must call barrier_release */
static void barrier_release(int phase) {
  /* nobody else can modify dstat, since all of the other domains
     are waiting */
  uintnat stat = atomic_load_acq(&domstat);
  /* DOMSTAT_WAITERS = 0; DOMSTAT_PHASE = !phase */
  atomic_store_rel(&domstat, (stat & ~0xffff) | (!phase));
}



static void stw_phase() {
  Assert(atomic_load_acq(&domain_self->is_active));
  while (caml_sweep(domain_self->state.shared_heap, 10) <= 0);

  caml_empty_minor_heap();
  caml_finish_marking();

  if (!domain_self->state.is_main) usleep(10000);
  if (barrier_enter(0)) {
    gc_inactive_domains();
    domain_filter_live();
    caml_cleanup_deleted_roots();
    barrier_release(0);
  }

  caml_cycle_heap(domain_self->state.shared_heap);

  /* filter_remembered_sets(); */
  
  if (barrier_enter(1)) {
    /* nothing to do here, just verify filter_remembered_sets is globally done */
    caml_cycle_heap_stw();
    atomic_store_rel(&stw_requested, 0);
    caml_gc_log("GC cycle completed");
    barrier_release(1);
  }
}

static void handle_rpc(dom_internal* target)
{
  if (atomic_load_acq(&target->rpc_request) != RPC_IDLE) {

    /* wait until we know what the request is */
    while (atomic_load_acq(&target->rpc_request) == RPC_REQUEST_INITIALISING) {
      cpu_relax();
    }

    Assert(atomic_load_acq(&target->rpc_request) == RPC_REQUEST_SENT);

    {     
      domain_rpc_handler rpc_handler = target->rpc_handler;
      void* rpc_data = target->rpc_data;
      atomic_uintnat* rpc_completion_signal = target->rpc_completion_signal;
      
      /* we have a copy of the request, it is now safe for other domains to overwrite it */
      atomic_store_rel(&target->rpc_request, RPC_IDLE);
      /* handle the request */
      rpc_handler(&target->state, rpc_data);
      /* signal completion */
      atomic_store_rel(rpc_completion_signal, 1);
    }
  }
}

static __thread struct rpc_domain_takeover* domains_taken_over = 0;

static int rpc_already_taken_over(dom_internal* d) {
  struct rpc_domain_takeover* i;
  if (d == domain_self) return 1;
  for (i = domains_taken_over; i; i = i->next) {
    if (i->target == d) return 1;
  }
  return 0;
}

/* If you lock a domain, you are responsible for handling
   its incoming RPC requests */
static int try_lock_domain(struct rpc_domain_takeover* takeover, 
                    dom_internal* target) {
  if (atomic_load_acq(&target->is_active))
    return 0;

  Assert (!rpc_already_taken_over(target));
  if (caml_plat_try_lock(&target->roots_lock)) {
    /* take over this domain as it is deactivated */
    takeover->target = target;
    takeover->next = domains_taken_over;
    domains_taken_over = takeover;
    return 1;
  } else {
    return 0;
  }
}

static void unlock_domain(struct rpc_domain_takeover* takeover)
{
  domains_taken_over = takeover->next;
  caml_plat_unlock(&takeover->target->roots_lock);
}

/* Handle incoming RPC requests for the current domain,
   and any domains we have temporarily taken over */
static void check_rpc()
{
  struct rpc_domain_takeover* i;
  handle_rpc(domain_self);
  for (i = domains_taken_over; i; i = i->next) {
    handle_rpc(i->target);
  }
}

static void attempt_rpc_takeover(dom_internal* target) {
  struct rpc_domain_takeover takeover;
  if (try_lock_domain(&takeover, target)) {
    /* process pending RPC for this domain */
    check_rpc();

    unlock_domain(&takeover);
  }
}


/* may be called from a non-safepoint. must not GC.
   message will be processed during an RPC safepoint in the target
   domain (or while holding the target domain's roots_lock).
   target domain must not be current domain */
CAMLexport void caml_domain_rpc(struct domain* domain,
                                domain_rpc_handler handler, void* data)
{
  atomic_uintnat completed = ATOMIC_UINTNAT_INIT(0);
  dom_internal* target = domain->internals;

  if (rpc_already_taken_over(target)) {
    /* well that was easy */
    handler(&target->state, data);
    return;
  }

  /* Wait until we can send an RPC to the target.
     Need to keep handling incoming RPCs while waiting.
     The target may have deactivated, so try taking it over */
  while (1) {
    if (atomic_load_acq(&target->rpc_request) == RPC_IDLE &&
        atomic_cas(&target->rpc_request, RPC_IDLE, RPC_REQUEST_INITIALISING)) {
      break;
    }
    attempt_rpc_takeover(target);
    check_rpc();
    cpu_relax();
  }

  /* Initialise and send the request */
  target->rpc_handler = handler;
  target->rpc_data = data;
  target->rpc_completion_signal = &completed;
  atomic_store_rel(&target->rpc_request, RPC_REQUEST_SENT);
  interrupt_domain(target);

  /* Wait for a response */
  while (atomic_load_acq(&completed) == 0) {
    attempt_rpc_takeover(target);
    check_rpc(); /* must keep handling RPCs while waiting for this one */
    cpu_relax();
  }
}
