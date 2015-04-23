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
*/


struct dom_internal {
  /* readonly fields, initialised and never modified */
  atomic_uintnat* interrupt_word_address;
  struct domain state;

  /* fields accessed by the domain itself and the domain requesting an RPC */
  atomic_uintnat rpc_request;
  domain_rpc_handler rpc_handler;
  void* rpc_data;
  atomic_uintnat* rpc_completion_signal;


  caml_plat_mutex roots_lock;

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

static __thread char domains_locked[Max_domains];

/* If you lock a domain, you are responsible for handling
   its incoming RPC requests */
static int try_lock_domain(dom_internal* target) {
  Assert(domains_locked[target->state.id] == 0);
  if (caml_plat_try_lock(&target->roots_lock)) {
    domains_locked[target->state.id] = 1;
    return 1;
  } else {
    return 0;
  }
}

static int domain_is_locked(dom_internal* target) {
  return domains_locked[target->state.id];
}

static void unlock_domain(dom_internal* target) {
  Assert(domains_locked[target->state.id] == 1);
  domains_locked[target->state.id] = 0;
  caml_plat_unlock(&target->roots_lock);
}


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
static void create_domain(uintnat initial_minor_heap_size, int is_main) {
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
    d->state.is_main = 0;
    d->state.internals = d;
    /* FIXME: shutdown RPC? */
    atomic_store_rel(&d->rpc_request, RPC_IDLE);

    domain_self = d;
    caml_plat_lock(&d->roots_lock);

    caml_young_start = caml_young_end = caml_young_ptr = 0;
    d->interrupt_word_address = &caml_young_limit;

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
    d->state.parent_stack = &caml_parent_stack;
#endif
    d->state.young_ptr = &caml_young_ptr;
    d->state.young_end = &caml_young_end;
    d->state.mark_stack = &caml_mark_stack;
    d->state.mark_stack_count = &caml_mark_stack_count;
  }
  caml_plat_unlock(&all_domains_lock);
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


  create_domain(minor_size, 1);
  if (!domain_self) caml_fatal_error("Failed to create main domain");

  caml_init_global_roots();
  caml_init_signal_handling();
}


static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();

  /* FIXME: proper domain termination and reuse */
  /* interrupt_word_address must not go away */
  pause();
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
  create_domain(caml_startup_params.minor_heap_init, 0);
  p->newdom = domain_self;
  caml_plat_event_trigger(&p->ev);

  if (domain_self) {
    caml_gc_log("Domain starting");
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

CAMLexport void caml_leave_blocking_section() {
  caml_plat_lock(&domain_self->roots_lock);
  caml_restore_stack_gc();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_save_stack_gc();
  caml_plat_unlock(&domain_self->roots_lock);
}


static atomic_uintnat heaps_marked;
static atomic_uintnat domain_accounted_for[Max_domains];

static void stw_phase () {
  int i;
  int my_heaps = 0;
  char inactive_domains_locked[Max_domains] = {0};

  /* First, make sure all domains are accounted for. */
  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    while (!atomic_load_acq(&domain_accounted_for[i])) {
      /* not accounted for yet */
      int mine = (d == domain_self) || domain_is_locked(d);
      if (!mine && try_lock_domain(d)) {
        /* mine now! */
        inactive_domains_locked[i] = 1;
        mine = 1;
      }
      if (mine) {
        /* accounted for by current thread */
        atomic_store_rel(&domain_accounted_for[i], 1);
        my_heaps++;
      } else {
        /* locked by some other thread,
           but not yet accounted for, need to wait */
        check_rpc();
        cpu_relax();
      }
    }
  }

  caml_gc_log("Contributing %d heaps to GC", my_heaps);

  /* Finish GC on the domains we've locked */

  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if (!d->state.shared_heap)
      continue; /* skip non-running domains */

    if (d == domain_self) {
      /* finish GC */
      while (caml_sweep(d->state.shared_heap, 10) <= 0);
      caml_empty_minor_heap();
      caml_finish_marking();
    } else if (domain_is_locked(d)) {
      /* GC some inactive domain that we locked */
      caml_gc_log("GCing inactive domain [%02d]", d->state.id);
      while (caml_sweep(d->state.shared_heap, 10) <= 0);
      caml_do_sampled_roots(&caml_darken, &d->state);
      caml_empty_mark_stack();
    }
  }

  /* Wait until all threads finish GC */

  if (atomic_fetch_add(&heaps_marked, (uintnat)my_heaps)
      + (uintnat)my_heaps == Max_domains) {
    /* we marked the last heap, so all other threads are waiting */
    //caml_cleanup_deleted_roots();

    caml_cycle_heap_stw();
    caml_gc_log("GC cycle completed (heap cycled)");

    /* reset for next GC */
    for (i = 0; i < Max_domains; i++) {
      atomic_store_rel(&domain_accounted_for[i], 0);
    }
    atomic_store_rel(&stw_requested, 0);

    /* allow other threads to proceed */
    atomic_store_rel(&heaps_marked, 0);
  } else {
    /* we didn't mark the last heap, so wait */
    while (atomic_load_acq(&heaps_marked)) {
      Assert(atomic_load_acq(&heaps_marked) <= Max_domains);
      check_rpc();
      cpu_relax();
    }
    caml_gc_log("GC cycle completed");
  }

  /* Finally, start the next sweeping cycle and
     unlock any inactive domains we locked for GC */

  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if ((d == domain_self || domain_is_locked(d)) && d->state.shared_heap)
      caml_cycle_heap(d->state.shared_heap);

    if (inactive_domains_locked[i])
      unlock_domain(d);
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

/* Handle incoming RPC requests for the current domain,
   and any domains we have temporarily taken over */
static void check_rpc()
{
  int i;
  handle_rpc(domain_self);
  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if (domain_is_locked(d))
      handle_rpc(d);
  }
}

static void attempt_rpc_takeover(dom_internal* target) {
  if (try_lock_domain(target)) {
    /* process pending RPC for this domain */
    check_rpc();

    unlock_domain(target);
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

  if (target == domain_self || domain_is_locked(target)) {
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
