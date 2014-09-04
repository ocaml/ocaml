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
  int sampled_roots_dirty;


  /* fields only modified during STW */
  struct dom_internal* next;
};
typedef struct dom_internal dom_internal;


/* possible values of domain->rpc_request */
enum { RPC_IDLE = 0, 
       RPC_REQUEST_INITIALISING = 1, 
       RPC_REQUEST_SENT = 2 };

__thread dom_internal* domain_self;


static caml_plat_mutex live_domains_lock;
static dom_internal* live_domains_list = 0;
static int next_domain_id;


static dom_internal* domain_alloc(uintnat initial_minor_heap_size, value initial) {
  dom_internal* d = caml_stat_alloc(sizeof (dom_internal));
  atomic_store_rel(&d->is_active, 0);
  atomic_store_rel(&d->is_alive, 1);
  d->state.is_main = 0;
  d->state.id = -1;
  d->state.initial_minor_heap_size = initial_minor_heap_size;
  d->state.internals = d;
  d->initial_data = initial == Val_unit ? 0 : caml_create_root(initial);
  atomic_store_rel(&d->rpc_request, RPC_IDLE);

  return d;
}


/* must be run on the domain's thread */
static void activate_domain();
static void domain_init(dom_internal* d) {
  Assert (domain_self == 0);
  domain_self = d;
  caml_init_young_ptrs();
  d->interrupt_word_address = &caml_young_limit;
  caml_plat_mutex_init(&d->roots_lock);

  /* ORD: is_alive = 1 visible before being added to live_domains_list */
  
  /* FIXME: what are the ordering constraints of caml_plat_lock, exactly? */
  
  /* get an ID */
  caml_plat_lock(&live_domains_lock);
  d->state.id = next_domain_id++;
  caml_plat_unlock(&live_domains_lock);

  d->state.shared_heap = caml_init_shared_heap();
  caml_init_major_gc();
  activate_domain();

  caml_set_minor_heap_size(caml_norm_minor_heap_size(d->state.initial_minor_heap_size));
  if (d->state.is_main) {
    caml_init_global_roots();
    caml_init_signal_handling();
  }
  d->state.remembered_set = &caml_remembered_set;
  d->state.runqueue = caml_init_runqueue();

  d->state.local_roots = &caml_local_roots;
  d->state.young_ptr = &caml_young_ptr;
  d->state.young_end = &caml_young_end;
  d->state.mark_stack = &caml_mark_stack;
  d->state.mark_stack_count = &caml_mark_stack_count;

  /* add to live_domains_list */
  caml_plat_lock(&live_domains_lock);
  d->next = live_domains_list;
  live_domains_list = d;
  caml_plat_unlock(&live_domains_lock);

}

static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();
  atomic_store_rel(&domain_self->is_alive, 0);
  /* FIXME: block until swept */
}


static void domain_filter_live() {
  dom_internal** p = &live_domains_list;
  caml_plat_lock(&live_domains_lock);
  while (*p) {
    dom_internal* d = *p;
    if (atomic_load_acq(&d->is_alive)) {
      p = &d->next;
    } else {
      *p = d->next;
      // FIXME: free d
    }
    if (!atomic_load_acq(&d->is_active)) {
      d->sampled_roots_dirty = 1;
    }
  }
  caml_plat_unlock(&live_domains_lock);
}


static void poll_interrupts();

static value domain_run(value v) {
  CAMLparam1 (v);
  caml_delete_root(domain_self->initial_data);

  /* FIXME exceptions */
  caml_callback_exn(v, Val_unit);

  CAMLreturn (Val_unit);
}

static void* domain_thread_func(void* v) {
  domain_init(v);
  caml_gc_log("Domain starting");
  domain_run(caml_read_root(domain_self->initial_data));
  domain_terminate();
  return 0;
}


CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  value promoted_callback = caml_promote(&domain_self->state, callback);
  dom_internal* newdom = domain_alloc(caml_startup_params.minor_heap_init, promoted_callback);
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
  dom_internal* dom = domain_alloc(minor_size, Val_unit);
  dom->state.is_main = 1;

  domain_init(dom);
}

struct domain* caml_domain_self()
{
  return domain_self ? &domain_self->state : 0;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(domain_self->state.id);
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);
__thread atomic_uintnat caml_young_limit = {0};
__thread uintnat desired_caml_young_limit = 0;

static atomic_uintnat stw_requested;

static void poll_interrupts() {
  if (Caml_check_gc_interrupt(desired_caml_young_limit)) {
    caml_handle_gc_interrupt(0);
  }
}

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
  dom_internal* d;
  atomic_store_rel(&stw_requested, 1);
  /* interrupt all domains */
  caml_plat_lock(&live_domains_lock);
  for (d = live_domains_list; d; d = d->next) {
    interrupt_domain(d);
  }
  caml_plat_unlock(&live_domains_lock);
}


void caml_trigger_stw_gc() {
  request_stw();
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

  if ((uintnat)caml_young_ptr - Bhsize_wosize(required_words) < desired_caml_young_limit) {
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
  // FIXME: are these in the wrong order?
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

static void gc_inactive_domains()
{
  while (1) {
    /* find an inactive, dirty domain */
    dom_internal* d;
    caml_plat_lock(&live_domains_lock);
    d = live_domains_list;

    for (; d; d = d->next) {
      if (!atomic_load_acq(&d->is_active) &&
          caml_plat_try_lock(&d->roots_lock)) {
        if (d->sampled_roots_dirty) {
          d->sampled_roots_dirty = 0;
          break;
        } else {
          caml_plat_unlock(&d->roots_lock);
        }
      }
    }

    caml_plat_unlock(&live_domains_lock);

    if (!d) break;

    /* If we found one, GC it */
    caml_gc_log("GCing inactive domain [%02d]", d->state.id);

    while (caml_sweep(d->state.shared_heap, 10) <= 0);

    caml_do_sampled_roots(&caml_darken, &d->state);
    caml_empty_mark_stack();

    caml_cycle_heap(d->state.shared_heap);

    caml_plat_unlock(&d->roots_lock);
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

struct rpc_domain_takeover {
  dom_internal* target;
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

static int rpc_already_taken_over(dom_internal* d) {
  struct rpc_domain_takeover* i;
  if (d == domain_self) return 1;
  for (i = domains_taken_over; i; i = i->next) {
    if (i->target == d) return 1;
  }
  return 0;
}

static void attempt_rpc_takeover(dom_internal* target) {
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
