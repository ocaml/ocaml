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


struct domain {
  /* readonly fields, initialised and never modified */
  int id;
  int is_main;
  uintnat initial_minor_heap_size;
  atomic_uintnat* interrupt_word_address;
  struct caml_runqueue* runqueue;

  /* fields only accessed by the domain itself (after initialisation) */
  caml_root initial_data;

  /* fields accessed concurrently by several threads */
  atomic_uintnat is_active; /* domain is not in a blocking section */
  atomic_uintnat is_alive;  /* domain has not terminated */

  /* fields accessed by the domain itself and the domain requesting an RPC */
  atomic_uintnat rpc_request;
  domain_rpc_handler rpc_handler;
  void* rpc_data;
  atomic_uintnat* rpc_completion_signal;
  

  /* fields protected by roots_lock */
  caml_plat_mutex roots_lock;
  struct caml_sampled_roots sampled_roots;
  int sampled_roots_dirty;


  /* fields only modified during STW */
  struct domain* next;
};

/* possible values of domain->rpc_request */
enum { RPC_IDLE = 0, 
       RPC_REQUEST_INITIALISING = 1, 
       RPC_REQUEST_SENT = 2 };

__thread struct domain* domain_self;


static caml_plat_mutex live_domains_lock;
static struct domain* live_domains_list = 0;
static int next_domain_id;


static struct domain* domain_alloc(uintnat initial_minor_heap_size, value initial) {
  struct domain* d = caml_stat_alloc(sizeof (struct domain));
  atomic_store_rel(&d->is_active, 0);
  atomic_store_rel(&d->is_alive, 1);
  d->is_main = 0;
  d->id = -1;
  d->initial_minor_heap_size = initial_minor_heap_size;
  d->initial_data = initial == Val_unit ? 0 : caml_create_root(initial);
  atomic_store_rel(&d->rpc_request, RPC_IDLE);

  return d;
}


/* must be run on the domain's thread */
static void domain_init(struct domain* d) {
  Assert (domain_self == 0);
  domain_self = d;
  caml_init_young_ptrs();
  d->interrupt_word_address = &caml_young_limit;
  caml_plat_mutex_init(&d->roots_lock);

  /* ORD: is_alive = 1 visible before being added to live_domains_list */
  
  /* FIXME: what are the ordering constraints of caml_plat_lock, exactly? */
  
  /* add to live_domains_list */
  caml_plat_lock(&live_domains_lock);
  d->id = next_domain_id++;
  d->next = live_domains_list;
  live_domains_list = d;
  caml_plat_unlock(&live_domains_lock);

  caml_init_shared_heap();
  caml_init_major_gc();
  caml_leave_blocking_section();

  caml_set_minor_heap_size(caml_norm_minor_heap_size(d->initial_minor_heap_size));
}

static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();
  atomic_store_rel(&domain_self->is_alive, 0);
  /* FIXME: block until swept */
}


static void domain_filter_live() {
  struct domain** p = &live_domains_list;
  caml_plat_lock(&live_domains_lock);
  while (*p) {
    struct domain* d = *p;
    if (atomic_load_acq(&d->is_alive)) {
      p = &d->next;
    } else {
      *p = d->next;
      // FIXME: free d
    }
  }
  caml_plat_unlock(&live_domains_lock);
}


static void poll_interrupts();

static value domain_run(value v) {
  CAMLparam1 (v);
  caml_delete_root(domain_self->initial_data);

  /* FIXME */
  caml_sample_local_roots(&domain_self->sampled_roots);

  /* FIXME exceptions */
  caml_callback_exn(v, Val_unit);

  CAMLreturn (Val_unit);
}

static void* domain_thread_func(void* v) {
  domain_init(v);
  caml_gc_log("Domain starting");
  caml_init_domain_fiber();
  domain_self->runqueue = caml_runqueue;
  domain_run(caml_read_root(domain_self->initial_data));
  domain_terminate();
  return 0;
}


CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  value promoted_callback = caml_promote(domain_self, callback);
  struct domain* newdom = domain_alloc(caml_startup_params.minor_heap_init, promoted_callback);
  pthread_t th;
  int err = pthread_create(&th, 0, domain_thread_func, (void*)newdom);
  if (err) {
    caml_failwith("failed to create domain");
  }
  pthread_detach(th);
  CAMLreturn (Val_unit);
}

void caml_domain_register_main(uintnat minor_size) {

  caml_plat_mutex_init(&live_domains_lock);
  struct domain* dom = domain_alloc(minor_size, Val_unit);
  dom->is_main = 1;

  domain_init(dom);

  caml_init_global_roots();
  caml_init_signal_handling();

  caml_init_domain_fiber();
  domain_self->runqueue = caml_runqueue;
}

int caml_domain_id(struct domain* d) {
  /* This function is called from caml_gc_log very early during initialisation,
     so the domain may be NULL */
  if (!d) return -1;
  return d->id;
}

int caml_domain_is_main(struct domain* d) {
  return d->is_main;
}

struct domain* caml_domain_self()
{
  return domain_self;
}

struct caml_runqueue* caml_domain_runqueue(struct domain* d) {
  return d->runqueue;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(caml_domain_id(domain_self));
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);
__thread atomic_uintnat caml_young_limit = {0};
__thread uintnat desired_caml_young_limit = 0;

static atomic_uintnat stw_requested;

static void poll_interrupts() {
  if (Caml_check_gc_interrupt(desired_caml_young_limit)) {
    caml_handle_gc_interrupt();
  }
}

/* update caml_young_limit, being careful not to lose interrupts */
void caml_update_young_limit(uintnat new_val) {
  Assert(new_val < INTERRUPT_MAGIC);
  while (1) {
    if (atomic_cas(&caml_young_limit, desired_caml_young_limit, new_val)) {
      break;
    }
    poll_interrupts();
    cpu_relax();
  }
  desired_caml_young_limit = new_val;
}



void caml_interrupt_domain(struct domain* d) {
  atomic_store_rel(d->interrupt_word_address, INTERRUPT_MAGIC);
}

static void request_stw() {
  struct domain* d;
  atomic_store_rel(&stw_requested, 1);
  /* interrupt all domains */
  caml_plat_lock(&live_domains_lock);
  for (d = live_domains_list; d; d = d->next) {
    caml_interrupt_domain(d);
  }
  caml_plat_unlock(&live_domains_lock);
}


void caml_trigger_stw_gc() {
  request_stw();
}

static void stw_phase(void);
static void check_rpc(void);

void caml_handle_gc_interrupt() {
  if (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    while (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
      atomic_cas(&caml_young_limit, INTERRUPT_MAGIC, desired_caml_young_limit);
    }
    check_rpc();
    if (atomic_load_acq(&stw_requested)) {
      stw_phase();
    }
  } else {
    /* out of minor heap */
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
CAMLexport void caml_leave_blocking_section() {
  /* add 1 to DOMSTAT_ACTIVE when DOMSTAT_PHASE and DOMSTAT_WAITERS are zero */
  while (1) {
    uintnat stat = atomic_load_acq(&domstat);
    if (DOMSTAT_PHASE(stat) == 0 && DOMSTAT_WAITERS(stat) == 0) {
      /* DOMSTAT_ACTIVE += 1 */
      if (atomic_cas(&domstat, stat, stat + 0x10000)) break;
    }
    cpu_relax();
  }
  // FIXME: are these in the wrong order?
  atomic_store_rel(&domain_self->is_active, 1);
  caml_plat_lock(&domain_self->roots_lock);
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_sample_local_roots(&domain_self->sampled_roots);
  domain_self->sampled_roots_dirty = 1;
  caml_plat_unlock(&domain_self->roots_lock);
  atomic_store_rel(&domain_self->is_active, 0);
  /* subtract 1 from DOMSTAT_ACTIVE */
  atomic_fetch_add(&domstat, -0x10000);
}

static void do_foreign_roots(scanning_action f, int mark_dirty)
{
  // FIXME: d->next validity as new domain created
  struct domain* d;
  caml_plat_lock(&live_domains_lock);
  d = live_domains_list;
  caml_plat_unlock(&live_domains_lock);

  for (; d; d = d->next) {
    if (!atomic_load_acq(&d->is_active) &&
        caml_plat_try_lock(&d->roots_lock)) {
      if (d->sampled_roots_dirty) {
        caml_gc_log("Marking roots of domain [%02d]", d->id);
        caml_do_sampled_roots(f, &d->sampled_roots);
        d->sampled_roots_dirty = 0;
      }
      if (mark_dirty) d->sampled_roots_dirty = 1;
      caml_plat_unlock(&d->roots_lock);
    }
  }
}

void caml_do_foreign_roots(scanning_action f)
{
  do_foreign_roots(f, 0);
}

struct caml_sampled_roots* caml_get_sampled_roots(struct domain* d) {
  return &d->sampled_roots;
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
  while (caml_sweep(domain_self->state.shared_heap, 10) <= 0);

  caml_empty_minor_heap();
  caml_finish_marking();

  if (!domain_self->is_main) usleep(10000);
  if (barrier_enter(0)) {
    do_foreign_roots(caml_darken, 1);
    caml_empty_mark_stack();
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

static void handle_rpc(struct domain* target)
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
      rpc_handler(target, rpc_data);
      /* signal completion */
      atomic_store_rel(rpc_completion_signal, 1);
    }
  }
}

struct rpc_domain_takeover {
  struct domain* target;
  struct rpc_domain_takeover* next;
};
static __thread struct rpc_domain_takeover* domains_taken_over = 0;

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

static int rpc_already_taken_over(struct domain* d) {
  struct rpc_domain_takeover* i;
  if (d == domain_self) return 1;
  for (i = domains_taken_over; i; i = i->next) {
    if (i->target == d) return 1;
  }
  return 0;
}

static void attempt_rpc_takeover(struct domain* target) {
  Assert (!rpc_already_taken_over(target));
  if (!atomic_load_acq(&target->is_active) && 
      caml_plat_try_lock(&target->roots_lock)) {
    /* take over this domain as it is deactivated */
    struct rpc_domain_takeover takeover = { target, domains_taken_over };
    domains_taken_over = &takeover;

    /* process pending RPC for this domain */
    check_rpc();
    
    domains_taken_over = takeover.next;
    caml_plat_unlock(&target->roots_lock);
  }
}


/* may be called from a non-safepoint. must not GC.
   message will be processed during an RPC safepoint in the target
   domain (or while holding the target domain's roots_lock).
   target domain must not be current domain */
CAMLexport void caml_domain_rpc(struct domain* target,
                                domain_rpc_handler handler, void* data)
{
  atomic_uintnat completed = ATOMIC_UINTNAT_INIT(0);

  if (rpc_already_taken_over(target)) {
    /* well that was easy */
    handler(target, data);
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

  /* Wait for a response */
  while (atomic_load_acq(&completed) == 0) {
    attempt_rpc_takeover(target);
    check_rpc(); /* must keep handling RPCs while waiting for this one */
    cpu_relax();
  }
}
