#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "domain.h"
#include "plat_threads.h"
#include "custom.h"
#include "major_gc.h"
#include "shared_heap.h"
#include "memory.h"
#include "fail.h"
#include "globroots.h"
#include "signals.h"
#include "alloc.h"
#include "startup.h"
#include "stacks.h"
#include "callback.h"

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

  /* fields only accessed by the domain itself (after initialisation) */
  caml_root initial_data;

  /* fields accessed concurrently by several threads */
  atomic_uintnat is_active; /* domain is not in a blocking section */
  atomic_uintnat is_alive;  /* domain has not terminated */

  /* fields protected by roots_lock */
  plat_mutex roots_lock;
  struct caml_sampled_roots sampled_roots;
  int sampled_roots_dirty;
};

__thread struct domain* domain_self;

struct custom_operations domain_custom_ops = {
  "_domain",
  custom_finalize_default,
  custom_compare_default, /* FIXME: comparing domains is actually meaningful */
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

static struct domain* Domain_val(value v) {
  Assert(Is_block(v) && Tag_hd(Hd_val(v)) == Custom_tag && Custom_ops_val(v) == &domain_custom_ops);
  return Data_custom_val(v);
}



/* A normal OCaml list (cons cells) containing at least the alive domains */
static caml_root live_domains_list;
static int next_domain_id;
static plat_mutex live_domains_lock;

static value domain_alloc(uintnat initial_minor_heap_size, value initial) {
  CAMLparam0();
  CAMLlocal2(result, cons);
  /* these really shouldn't be on the minor heap, so we allocate manually */
  /* FIXME: this should caml_alloc_custom and then promote immediately */
  result = caml_alloc_shr(1 + (sizeof (struct domain) + sizeof(value) - 1) / sizeof(value), Custom_tag);
  Custom_ops_val(result) = &domain_custom_ops;
  struct domain* d = Domain_val(result);
  atomic_store_rel(&d->is_active, 0);
  atomic_store_rel(&d->is_alive, 1);
  d->is_main = 0;
  d->id = -1;
  d->initial_minor_heap_size = initial_minor_heap_size;
  d->initial_data = initial == Val_unit ? 0 : caml_create_root(initial);

  /* this object will eventually be placed into live_domains_list. We
     allocate the cons cell now, because domain_init is a bad time to 
     handle allocation failure */
  cons = caml_alloc_shr(2, 0);
  caml_initialize_field(cons, 0, result);
  caml_initialize_field(cons, 1, Val_unit);

  CAMLreturn (cons);
}


/* must be run on the domain's thread */
static void domain_init(value cons) {
  struct domain* d = Domain_val(Field(cons, 0));
  Assert (domain_self == 0);
  domain_self = d;
  d->interrupt_word_address = &caml_young_limit;
  plat_mutex_init(&d->roots_lock);
}

/* must be run on the domain's thread */
static void domain_register(value cons) {
  struct domain* d = Domain_val(Field(cons, 0));
  /* ORD: is_alive = 1 visible before being added to live_domains_list */
  
  /* FIXME: what are the ordering constraints of plat_mutex_lock, exactly? */
  
  /* add to live_domains_list */
  plat_mutex_lock(&live_domains_lock);
  caml_modify_field(cons, 1, caml_read_root(live_domains_list));
  caml_modify_root(live_domains_list, cons);
  d->id = next_domain_id++;
  plat_mutex_unlock(&live_domains_lock);
  
  caml_leave_blocking_section();
}

static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();
  atomic_store_rel(&domain_self->is_alive, 0);
  /* FIXME: block until swept */
}


static void domain_mark_live() {
  value last_live, l;
  int first = 1;
  plat_mutex_lock(&live_domains_lock);
  l = caml_read_root(live_domains_list);
  while (Is_block(l)) {
    value domain = Field(l, 0);
    value tail = Field(l, 1);
    if (atomic_load_acq(&(Domain_val(domain)->is_alive))) {
      caml_mark_object(domain);
      caml_mark_object(l);
      if (first) {
        caml_modify_root(live_domains_list, l);
      } else {
        caml_modify_field(last_live, 1, l);
      }
      first = 0;
      last_live = l;
    }
    l = tail;
  }
  plat_mutex_unlock(&live_domains_lock);
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
  caml_init_young_ptrs();
  caml_init_shared_heap();
  caml_init_major_gc();

  domain_init((value)v);
  domain_register((value)v);
  caml_set_minor_heap_size (caml_norm_minor_heap_size (domain_self->initial_minor_heap_size));
  caml_gc_log("Domain starting");

  caml_init_stack();

  value init = caml_read_root(domain_self->initial_data);
  domain_run(init);

  domain_terminate();
  return 0;
}


CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  caml_empty_minor_heap(); /* FIXME */
  value newdom = domain_alloc(caml_startup_params.minor_heap_init, callback);
  pthread_t th;
  int err = pthread_create(&th, 0, domain_thread_func, (void*)newdom);
  if (err) {
    caml_failwith("failed to create domain");
  }
  pthread_detach(th);
  CAMLreturn (Val_unit);
}

void caml_domain_register_main(uintnat minor_size) {
  caml_init_young_ptrs();
  caml_init_shared_heap();
  caml_init_major_gc();

  value dom = domain_alloc(minor_size, Val_unit);
  domain_init(dom);
  domain_self->is_main = 1;

  caml_init_global_roots();
  live_domains_list = caml_create_root();
  plat_mutex_init(&live_domains_lock);

  domain_register(dom);

  caml_set_minor_heap_size (caml_norm_minor_heap_size (minor_size));

  caml_init_signal_handling();


  //  caml_domain_create(minor_size); /* for testing */
}

int caml_domain_id() {
  if (!domain_self) return -1;
  return domain_self->id;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(caml_domain_id());
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



void caml_interrupt_domain(value domain) {
  atomic_store_rel(Domain_val(domain)->interrupt_word_address, INTERRUPT_MAGIC);
}

static void request_stw() {
  value d;
  atomic_store_rel(&stw_requested, 1);
  /* interrupt all domains */
  plat_mutex_lock(&live_domains_lock);
  for (d = caml_read_root(live_domains_list); Is_block(d); d = Field(d, 1)) {
    caml_interrupt_domain(Field(d, 0));
  }
  plat_mutex_unlock(&live_domains_lock);
}


void caml_trigger_stw_gc() {
  request_stw();
}

static void stw_phase(void);

void caml_handle_gc_interrupt() {
  if (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    while (atomic_load_acq(&caml_young_limit) == INTERRUPT_MAGIC) {
      atomic_cas(&caml_young_limit, INTERRUPT_MAGIC, desired_caml_young_limit);
    }
    if (atomic_load_acq(&stw_requested)) {
      stw_phase();
    }
  } else {
    /* out of minor heap */
    caml_minor_collection();
  }
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
  atomic_store_rel(&domain_self->is_active, 1);
  plat_mutex_lock(&domain_self->roots_lock);
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_sample_local_roots(&domain_self->sampled_roots);
  domain_self->sampled_roots_dirty = 1;
  plat_mutex_unlock(&domain_self->roots_lock);
  atomic_store_rel(&domain_self->is_active, 0);
  /* subtract 1 from DOMSTAT_ACTIVE */
  atomic_fetch_add(&domstat, -0x10000);
}

static void do_foreign_roots(scanning_action f, int mark_dirty)
{
  value domlist;
  plat_mutex_lock(&live_domains_lock);
  domlist = caml_read_root(live_domains_list);
  plat_mutex_unlock(&live_domains_lock);

  for (; Is_block(domlist); domlist = Field(domlist, 1)) {
    struct domain* dom = Domain_val(Field(domlist, 0));
    if (!atomic_load_acq(&dom->is_active) &&
        plat_mutex_try_lock(&dom->roots_lock)) {
      if (dom->sampled_roots_dirty) {
        caml_gc_log("Marking roots of domain [%02d]", dom->id);
        caml_do_sampled_roots(f, &dom->sampled_roots);
        dom->sampled_roots_dirty = 0;
      }
      if (mark_dirty) dom->sampled_roots_dirty = 1;
      plat_mutex_unlock(&dom->roots_lock);
    }
  }
}

void caml_do_foreign_roots(scanning_action f)
{
  do_foreign_roots(f, 0);
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
  if (domain_self->is_main) {
    while (caml_sweep(42) <= 0);


  }

  caml_empty_minor_heap();
  caml_finish_marking();

  if (!domain_self->is_main) usleep(10000);
  if (barrier_enter(0)) {
    do_foreign_roots(caml_darken, 1);
    caml_empty_mark_stack();
    domain_mark_live();
    caml_cleanup_deleted_roots();
    caml_cycle_heap();
    atomic_store_rel(&stw_requested, 0);
    barrier_release(0);
  }

  /* filter_remembered_sets(); */
  
  if (barrier_enter(1)) {
    /* nothing to do here, just verify filter_remembered_sets is globally done */
    caml_gc_log("GC cycle completed");
    barrier_release(1);
  }
}
