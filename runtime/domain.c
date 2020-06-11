/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/domain.h"
#include "caml/domain_state.h"
#include "caml/platform.h"
#include "caml/custom.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/globroots.h"
#include "caml/signals.h"
#include "caml/alloc.h"
#include "caml/startup.h"
#include "caml/fiber.h"
#include "caml/callback.h"
#include "caml/minor_gc.h"
#include "caml/eventlog.h"
#include "caml/gc_ctrl.h"
#include "caml/osdeps.h"
#include "caml/weak.h"
#include "caml/finalise.h"
#include "caml/gc_ctrl.h"

#define MSG_IN_BLOCKING_SECTION 0
#define MSG_ENTERING_OCAML 1
#define MSG_TERMINATE 2

/* Since we support both heavyweight OS threads and lightweight
   userspace threads, the word "thread" is ambiguous. This file deals
   with OS-level threads, called "domains".
*/


/* control of interrupts */
struct interruptor {
  atomic_uintnat* interrupt_word;
  caml_plat_mutex lock;
  caml_plat_cond cond;

  int running;
  int terminating;
  /* unlike the domain ID, this ID number is not reused */
  uintnat unique_id;

  /* Queue of domains trying to send interrupts here */
  struct interrupt* qhead;
  struct interrupt* qtail;      /* defined only when qhead != NULL */

  /* Next pointer for wait queues.
     Touched only when the queue is locked */
  struct interruptor* next;
};
/* returns 0 on failure, if the target has terminated. */
CAMLcheckresult
int caml_send_interrupt(struct interruptor* self,
                        struct interruptor* target,
                        domain_rpc_handler handler,
                        void* data);
void caml_handle_incoming_interrupts(void);


struct dom_internal {
  /* readonly fields, initialised and never modified */
  atomic_uintnat* interrupt_word_address;
  int id;
  struct domain state;
  struct interruptor interruptor;

  /* backup thread */
  int backup_thread_running;
  pthread_t backup_thread;
  atomic_uintnat backup_thread_msg;
  caml_plat_mutex domain_lock;
  caml_plat_cond domain_cond;

  /* readonly */
  uintnat tls_area;
  uintnat tls_area_end;
  uintnat minor_heap_area;
  uintnat minor_heap_area_end;
};
typedef struct dom_internal dom_internal;

static uintnat handle_incoming(struct interruptor* s);

static caml_plat_mutex all_domains_lock = CAML_PLAT_MUTEX_INITIALIZER;
static caml_plat_cond all_domains_cond = CAML_PLAT_COND_INITIALIZER(&all_domains_lock);
static atomic_uintnat /* dom_internal* */ stw_leader = 0;
static struct dom_internal all_domains[Max_domains];
static atomic_uintnat num_domains_running;

static uintnat minor_heaps_base;
static __thread dom_internal* domain_self;

static int64_t startup_timestamp;

struct interrupt {
  /* immutable fields */
  domain_rpc_handler handler;
  void* data;
  struct interruptor* sender;

  atomic_uintnat completed;

  /* accessed only when target's lock held */
  struct interrupt* next;
};

#ifdef __APPLE__
/* OSX has issues with dynamic loading + exported TLS.
    This is slower but works */
CAMLexport pthread_key_t caml_domain_state_key;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;

static void caml_make_domain_state_key ()
{
  (void) pthread_key_create (&caml_domain_state_key, NULL);
}

void caml_init_domain_state_key ()
{
  pthread_once(&key_once, caml_make_domain_state_key);
}

#else
CAMLexport __thread caml_domain_state* Caml_state;
#endif

asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = caml_mem_round_up_pages(1);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  Assert(page_size * 2 < (1 << Minor_heap_align_bits));
  max = (1 << Minor_heap_align_bits) - page_size * 2;

  if (bs > max) bs = max;

  return Wsize_bsize(bs);
}

int caml_reallocate_minor_heap(asize_t wsize)
{
  caml_domain_state* domain_state = Caml_state;
  Assert(domain_state->young_ptr == domain_state->young_end);

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit((void*)domain_self->minor_heap_area,
                    domain_self->minor_heap_area_end - domain_self->minor_heap_area);

  /* we allocate a double buffer to allow early release in minor_gc */
  wsize = caml_norm_minor_heap_size(wsize);

  if (!caml_mem_commit((void*)domain_self->minor_heap_area, Bsize_wsize(wsize))) {
    return -1;
  }

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)domain_self->minor_heap_area;
    for (; p < (uintnat*)(domain_self->minor_heap_area + Bsize_wsize(wsize)); p++)
      *p = Debug_uninit_align;
  }
#endif

  domain_state->minor_heap_wsz = wsize;

  domain_state->young_start = (char*)domain_self->minor_heap_area;
  domain_state->young_end = (char*)(domain_self->minor_heap_area + Bsize_wsize(wsize));
  domain_state->young_limit = (uintnat) domain_state->young_start;
  domain_state->young_ptr = domain_state->young_end;
  return 0;
}

/* must be run on the domain's thread */
static void create_domain(uintnat initial_minor_heap_wsize) {
  int i;
  dom_internal* d = 0;
  Assert (domain_self == 0);

  caml_plat_lock(&all_domains_lock);

  /* wait until any in-progress STW sections end */
  while (atomic_load_acq(&stw_leader)) caml_plat_wait(&all_domains_cond);

  for (i = 0; i < Max_domains && !d; i++) {
    struct interruptor* s = &all_domains[i].interruptor;
    caml_plat_lock(&s->lock);
    if (!s->running) {
      d = &all_domains[i];
      if (!d->interrupt_word_address) {
        /* never been started before, so set up minor heap */
        if (!caml_mem_commit((void*)d->tls_area, (d->tls_area_end - d->tls_area))) {
          /* give up now: if we couldn't get memory for this domain, we're
             unlikely to have better luck with any other */
          d = 0;
          caml_plat_unlock(&s->lock);
          break;
        }
        caml_domain_state* domain_state =
          (caml_domain_state*)(d->tls_area);
        atomic_uintnat* young_limit = (atomic_uintnat*)&domain_state->young_limit;
        d->interrupt_word_address = young_limit;
        atomic_store_rel(young_limit, (uintnat)domain_state->young_start);
        s->interrupt_word = young_limit;
      }
      Assert(s->qhead == NULL);
      s->running = 1;
      atomic_fetch_add(&num_domains_running, 1);
    }
    caml_plat_unlock(&s->lock);
  }
  if (d) {
    d->state.internals = d;
    domain_self = d;
    SET_Caml_state((void*)(d->tls_area));
    caml_domain_state* domain_state =
      (caml_domain_state*)(d->tls_area);
    caml_plat_lock(&d->domain_lock);

    domain_state->id = d->id;
    domain_state->unique_id = d->interruptor.unique_id;
    d->state.state = domain_state;
    domain_state->critical_section_nesting = 0;

    if (caml_init_signal_stack() < 0) {
      goto init_signal_stack_failure;
    }

    domain_state->young_start = domain_state->young_end =
      domain_state->young_ptr = 0;
    domain_state->minor_tables = caml_alloc_minor_tables();
    if(domain_state->minor_tables == NULL) {
      goto alloc_minor_tables_failure;
    }

    d->state.state->shared_heap = caml_init_shared_heap();
    if(d->state.state->shared_heap == NULL) {
      goto init_shared_heap_failure;
    }

    if (caml_init_major_gc(domain_state) < 0) {
      goto init_major_gc_failure;
    }

    if(caml_reallocate_minor_heap(initial_minor_heap_wsize) < 0) {
      goto reallocate_minor_heap_failure;
    }

    Caml_state->current_stack =
        caml_alloc_main_stack(Stack_size / sizeof(value));
    if(Caml_state->current_stack == NULL) {
      goto alloc_main_stack_failure;
    }

    Caml_state->read_fault_ret_val = caml_create_root_noexc(Val_unit);
    if(Caml_state->read_fault_ret_val == NULL) {
      goto create_root_failure;
    }

    atomic_store_rel(&d->backup_thread_msg, MSG_ENTERING_OCAML);

    domain_state->backtrace_buffer = NULL;
#ifndef NATIVE_CODE
    domain_state->external_raise = NULL;
    domain_state->trap_sp_off = 1;
#endif
    goto domain_init_complete;

create_root_failure:
  if(Caml_state->current_stack != NULL)
    caml_free_stack(Caml_state->current_stack);
alloc_main_stack_failure:
reallocate_minor_heap_failure:
  caml_teardown_major_gc();
init_major_gc_failure:
  caml_teardown_shared_heap(d->state.state->shared_heap);
init_shared_heap_failure:
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = NULL;
alloc_minor_tables_failure:
  caml_free_signal_stack();
init_signal_stack_failure:
  domain_self = NULL;

domain_init_complete:
  caml_ev_resume();
  }
  caml_plat_unlock(&all_domains_lock);
}

void caml_init_domains(uintnat minor_heap_wsz) {
  int i;
  uintnat size;
  void* heaps_base;

  /* sanity check configuration */
  if (caml_mem_round_up_pages(1 << Minor_heap_align_bits) != (1 << Minor_heap_align_bits))
    caml_fatal_error("Minor_heap_align_bits misconfigured for this platform");

  /* reserve memory space for minor heaps */
  size = (uintnat)1 << (Minor_heap_sel_bits + Minor_heap_align_bits);

  heaps_base = caml_mem_map(size*2, size*2, 1 /* reserve_only */);
  if (!heaps_base) caml_raise_out_of_memory();

  minor_heaps_base = (uintnat) heaps_base;

  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];
    uintnat domain_minor_heap_base;

    caml_plat_mutex_init(&dom->domain_lock);
    caml_plat_cond_init(&dom->domain_cond, &dom->domain_lock);

    caml_plat_mutex_init(&dom->interruptor.lock);
    caml_plat_cond_init(&dom->interruptor.cond,
                        &dom->interruptor.lock);
    dom->interruptor.qhead = dom->interruptor.qtail = NULL;
    dom->interruptor.running = 0;
    dom->interruptor.terminating = 0;
    dom->interruptor.unique_id = i;
    dom->id = i;

    domain_minor_heap_base = minor_heaps_base +
      (uintnat)(1 << Minor_heap_align_bits) * (uintnat)i;
    dom->tls_area = domain_minor_heap_base;
    dom->tls_area_end =
      caml_mem_round_up_pages(dom->tls_area +
                              sizeof(caml_domain_state));
    dom->minor_heap_area = /* skip guard page */
      caml_mem_round_up_pages(dom->tls_area_end + 1);
    dom->minor_heap_area_end =
      domain_minor_heap_base + (1 << Minor_heap_align_bits);
  }


  create_domain(minor_heap_wsz);
  if (!domain_self) caml_fatal_error("Failed to create main domain");

  caml_init_signal_handling();
  startup_timestamp = caml_time_counter();
}

void caml_init_domain_self(int domain_id) {
  Assert (domain_id >= 0 && domain_id < Max_domains);
  domain_self = &all_domains[domain_id];
  SET_Caml_state(domain_self->state.state);
}

enum domain_status { Dom_starting, Dom_started, Dom_failed };
struct domain_startup_params {
  struct interruptor* parent;
  enum domain_status status;
  caml_root callback;
  dom_internal* newdom;
  uintnat unique_id;
};

static void* backup_thread_func(void* v)
{
  dom_internal* di = (dom_internal*)v;
  uintnat msg;
  struct interruptor* s = &di->interruptor;

  domain_self = di;
  SET_Caml_state((void*)(di->tls_area));

  caml_plat_lock (&di->domain_lock);
  while (1) { /* loop1 */
    msg = atomic_load_acq (&di->backup_thread_msg);
    Assert (msg <= MSG_TERMINATE);
    if (msg == MSG_ENTERING_OCAML) {
      /* Main thread wants to enter OCaml */
      caml_plat_wait(&di->domain_cond);
    } else if (msg == MSG_IN_BLOCKING_SECTION) {
      /* Handle interrupts on behalf of the main thread */
      caml_plat_lock(&s->lock);
      /* Both [s->lock] and [di->domain_lock] held here */
      while (1) { /* loop2 */
        msg = atomic_load_acq (&di->backup_thread_msg);
        if (msg == MSG_ENTERING_OCAML) {
          /* Main thread is leaving blocking section and entering OCaml */
          caml_plat_unlock (&s->lock);
          break; /* break loop2 and goto loop1 */
        } else if (handle_incoming(s) == 0) {
          caml_plat_wait(&s->cond);
        }
      }
    } else if (msg == MSG_TERMINATE) {
      caml_plat_unlock (&di->domain_lock);
      break;
    }
  }
  return 0;
}

static void install_backup_thread (dom_internal* di)
{
  int err;

  if (di->backup_thread_running == 0) {
    err = pthread_create (&di->backup_thread, 0, backup_thread_func, (void*)di);
    if (err)
      caml_failwith("failed to create domain backup thread");
    di->backup_thread_running = 1;
    pthread_detach(di->backup_thread);
  }
}

static void domain_terminate();

static void* domain_thread_func(void* v)
{
  struct domain_startup_params* p = v;
  caml_root callback = p->callback;

  create_domain(caml_params->init_minor_heap_wsz);
  p->newdom = domain_self;

  caml_plat_lock(&p->parent->lock);
  if (domain_self) {
    p->status = Dom_started;
    p->unique_id = domain_self->interruptor.unique_id;
  } else {
    p->status = Dom_failed;
  }
  caml_plat_broadcast(&p->parent->cond);
  caml_plat_unlock(&p->parent->lock);
  /* cannot access p below here */

  if (domain_self) {
    install_backup_thread(domain_self);
    caml_gc_log("Domain starting (unique_id = %"ARCH_INTNAT_PRINTF_FORMAT"u)",
                domain_self->interruptor.unique_id);
    caml_callback(caml_read_root(callback), Val_unit);
    caml_delete_root(callback);
    domain_terminate();
  } else {
    caml_gc_log("Failed to create domain");
  }
  return 0;
}

#define Domainthreadptr_val(val) ((struct domain_thread**)Data_custom_val(val))

CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  struct domain_startup_params p;
  pthread_t th;
  int err;

  caml_ev_begin("domain/spawn");
  p.parent = &domain_self->interruptor;
  p.status = Dom_starting;

  p.callback = caml_create_root(callback);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  if (err) {
    caml_failwith("failed to create domain thread");
  }

  caml_plat_lock(&domain_self->interruptor.lock);
  while (p.status == Dom_starting) {
    if (handle_incoming(&domain_self->interruptor) == 0)
      caml_plat_wait(&domain_self->interruptor.cond);
  }
  caml_plat_unlock(&domain_self->interruptor.lock);

  if (p.status == Dom_started) {
    /* successfully created a domain.
       p.callback is now owned by that domain */
    pthread_detach(th);
  } else {
    Assert (p.status == Dom_failed);
    /* failed */
    pthread_join(th, 0);
    caml_delete_root(p.callback);
    caml_failwith("failed to allocate domain");
  }
  install_backup_thread(domain_self);
  caml_ev_end("domain/spawn");
  CAMLreturn (Val_long(p.unique_id));
}

CAMLprim value caml_ml_domain_join(value domain)
{
    caml_failwith("domain.join unimplemented");
}

struct domain* caml_domain_self()
{
  return domain_self ? &domain_self->state : 0;
}

int caml_domain_alone()
{
  return atomic_load_acq(&num_domains_running) == 1;
}

struct domain* caml_owner_of_young_block(value v) {
  Assert(Is_minor(v));
  int heap_id = ((uintnat)v - minor_heaps_base) /
    (1 << Minor_heap_align_bits);
  return &all_domains[heap_id].state;
}

struct domain* caml_domain_of_id(int id)
{
  return &all_domains[id].state;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(domain_self->interruptor.unique_id);
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);

static void interrupt_domain(dom_internal* d) {
  atomic_store_rel(d->interrupt_word_address, INTERRUPT_MAGIC);
}

static struct {
  atomic_uintnat domains_still_running;
  atomic_uintnat num_domains_still_processing;
  void (*callback)(struct domain*, void*, int participating_count, struct domain** others_participating);
  void* data;
  int leave_when_done;
  int num_domains;
  atomic_uintnat barrier;
  void (*enter_spin_callback)(struct domain*, void*);
  void* enter_spin_data;
  void (*leave_spin_callback)(struct domain*, void*);
  void* leave_spin_data;

  struct interrupt reqs[Max_domains];
  struct domain* participating[Max_domains];
} stw_request = {
  ATOMIC_UINTNAT_INIT(0),
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  0,
  0,
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  NULL,
  NULL,
  { { 0 } },
  { 0 }
};

/* sense-reversing barrier */
#define BARRIER_SENSE_BIT 0x100000

barrier_status caml_global_barrier_begin()
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

void caml_global_barrier()
{
  barrier_status b = caml_global_barrier_begin();
  caml_global_barrier_end(b);
}

int caml_global_barrier_num_domains()
{
  return stw_request.num_domains;
}

static void decrement_stw_domains_still_processing()
{
  /* we check if we are the last to leave a stw section
     if so, clear the stw_leader to allow the new stw sections to start.
   */
  intnat am_last = atomic_fetch_add(&stw_request.num_domains_still_processing, -1) == 1;

  if( am_last ) {
    /* release the STW lock to allow new STW sections */
    caml_plat_lock(&all_domains_lock);
    atomic_store_rel(&stw_leader, 0);
    caml_plat_broadcast(&all_domains_cond);
    caml_gc_log("clearing stw leader");
    caml_plat_unlock(&all_domains_lock);
  }
}

static void stw_handler(struct domain* domain, void* unused2, interrupt* done)
{
#ifdef DEBUG
  caml_domain_state* domain_state = Caml_state;
#endif

  caml_ev_begin("stw/handler");
  caml_acknowledge_interrupt(done);
  caml_ev_begin("stw/api_barrier");
  SPIN_WAIT {
    if (atomic_load_acq(&stw_request.domains_still_running) == 0)
      break;
    caml_handle_incoming_interrupts();

    if (stw_request.enter_spin_callback)
      stw_request.enter_spin_callback(domain, stw_request.enter_spin_data);
  }
  caml_ev_end("stw/api_barrier");

  #ifdef DEBUG
  domain_state->inside_stw_handler = 1;
  #endif
  stw_request.callback(domain, stw_request.data, stw_request.num_domains, stw_request.participating);
  #ifdef DEBUG
  domain_state->inside_stw_handler = 0;
  #endif

  decrement_stw_domains_still_processing();

  if( !stw_request.leave_when_done ) {
    SPIN_WAIT {
      if (atomic_load_acq(&stw_request.num_domains_still_processing) == 0)
        break;

      if (stw_request.leave_spin_callback)
        stw_request.leave_spin_callback(domain, stw_request.leave_spin_data);
    }
  }

  caml_ev_end("stw/handler");
}

/* This runs the passed handler on all running domains but must only be run on *one* domain
   inside of a global barrier during a stop-the-world phase. */
void caml_run_on_all_running_domains_during_stw(void (*handler)(struct domain*, void*), void* data) {
  int i;

  for (i = 0; i < Max_domains; i++) {
    struct interruptor* interruptor = &all_domains[i].interruptor;

    if( interruptor->running ) {
      handler(&all_domains[i].state, data);
    }
  }
}

#ifdef DEBUG
int caml_domain_is_in_stw() {
  caml_domain_state* domain_state = Caml_state;

  return domain_state->inside_stw_handler;
}
#endif

static int caml_send_partial_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data,
                         struct interrupt* req);
static void caml_wait_interrupt_completed(struct interruptor* self, struct interrupt* req);

int caml_try_run_on_all_domains_with_spin_work(
  void (*handler)(struct domain*, void*, int, struct domain**), void* data,
  void (*leader_setup)(struct domain*),
  void (*enter_spin_callback)(struct domain*, void*), void* enter_spin_data,
  void (*leave_spin_callback)(struct domain*, void*), void* leave_spin_data,
  int leave_when_done
  )
{
#ifdef DEBUG
  caml_domain_state* domain_state = Caml_state;
#endif
  int i;
  uintnat domains_participating = 0;

  // Don't take the lock if there's already a stw leader
  if( atomic_load_acq(&stw_leader) ) {
    caml_handle_incoming_interrupts();
    return 0;
  }

  caml_gc_log("requesting STW");

  /* Try to take the lock by setting ourselves as the stw_leader.
     If it fails, handle interrupts (probably participating in
     an STW section) and return. */
  caml_plat_lock(&all_domains_lock);
  if (atomic_load_acq(&stw_leader)) {
    caml_plat_unlock(&all_domains_lock);
    caml_handle_incoming_interrupts();
    return 0;
  } else {
    atomic_store_rel(&stw_leader, (uintnat)domain_self);
  }
  caml_plat_unlock(&all_domains_lock);

  caml_ev_begin("stw/leader");
  caml_gc_log("causing STW");

  atomic_store_rel(&stw_request.domains_still_running, 1);

  if( leader_setup ) {
    leader_setup(&domain_self->state);
  }

  /* Next, interrupt all domains, counting how many domains received
     the interrupt (i.e. are not terminated and are participating in
     this STW section). */
  {
    struct interrupt* reqs = stw_request.reqs;
    struct domain** participating = stw_request.participating;

    for (i = 0; i < Max_domains; i++) {
      if (&all_domains[i] == domain_self) {
        participating[domains_participating] = &domain_self->state;
        domains_participating++;
        continue;
      }
      if (caml_send_partial_interrupt(&domain_self->interruptor,
                              &all_domains[i].interruptor,
                              stw_handler,
                              0,
                              &reqs[domains_participating])) {
        participating[domains_participating] = &all_domains[i].state;
        domains_participating++;
      }
    }

    for(i = 0; i < domains_participating ; i++) {
      if( participating[i] && &domain_self->state != participating[i] ) {
        caml_wait_interrupt_completed(&domain_self->interruptor, &reqs[i]);
      }
    }
  }

  Assert(domains_participating > 0);

  stw_request.num_domains = domains_participating;
  stw_request.leave_when_done = leave_when_done;
  atomic_store_rel(&stw_request.barrier, 0);
  atomic_store_rel(&stw_request.num_domains_still_processing,
                   domains_participating);
  stw_request.callback = handler;
  stw_request.data = data;
  stw_request.enter_spin_callback = enter_spin_callback;
  stw_request.enter_spin_data = enter_spin_data;
  stw_request.leave_spin_callback = leave_spin_callback;
  stw_request.leave_spin_data = leave_spin_data;

  atomic_store_rel(&stw_request.domains_still_running, 0);

  #ifdef DEBUG
  domain_state->inside_stw_handler = 1;
  #endif
  handler(&domain_self->state, data, domains_participating, stw_request.participating);
  #ifdef DEBUG
  domain_state->inside_stw_handler = 0;
  #endif

  decrement_stw_domains_still_processing();

  if( !leave_when_done ) {
    SPIN_WAIT {
      if (atomic_load_acq(&stw_request.num_domains_still_processing) == 0)
        break;
    }
  }

  caml_ev_end("stw/leader");
  /* other domains might not have finished stw_handler yet, but they
     will finish as soon as they notice num_domains_still_processing
     == 0, which will remain the case until they have responded to
     another interrupt from caml_run_on_all_domains */
  return 1;
}

int caml_try_run_on_all_domains(void (*handler)(struct domain*, void*, int, struct domain**), void* data, void (*leader_setup)(struct domain*), int leave_when_done)
{
  return caml_try_run_on_all_domains_with_spin_work(handler, data, leader_setup, 0, 0, 0, 0, leave_when_done);
}

void caml_interrupt_self() {
  interrupt_domain(domain_self);
}

/* Arrange for a garbage collection to be performed on the current domain
   as soon as possible */
void caml_urge_major_slice (void)
{
  Caml_state->force_major_slice = 1;
  caml_interrupt_self();
}

void caml_handle_gc_interrupt() {
  atomic_uintnat* young_limit = domain_self->interrupt_word_address;
  CAMLalloc_point_here;

  if (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    caml_ev_begin("handle_interrupt");
    while (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
      uintnat i = INTERRUPT_MAGIC;
      atomic_compare_exchange_strong(young_limit, &i, (uintnat)Caml_state->young_start);
    }
    caml_ev_pause(EV_PAUSE_YIELD);
    caml_handle_incoming_interrupts();
    caml_ev_resume();
    caml_ev_end("handle_interrupt");
  }

  if (((uintnat)Caml_state->young_ptr - Bhsize_wosize(Max_young_wosize) <
       (uintnat)Caml_state->young_start) ||
      Caml_state->force_major_slice) {
    caml_ev_begin("dispatch");
    /* out of minor heap or collection forced */
    Caml_state->force_major_slice = 0;
    caml_minor_collection();
    caml_ev_end("dispatch");
  }
}

static void caml_enter_blocking_section_default(void)
{
  return;
}

static void caml_leave_blocking_section_default(void)
{
  return;
}


CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;

CAMLexport void caml_leave_blocking_section() {
  dom_internal* self = domain_self;

  atomic_store_rel(&self->backup_thread_msg, MSG_ENTERING_OCAML);

  if (self->backup_thread_running) {
    /* Lock is necessary here to avoid races with the backup thread
     * [backup_thread_func]. */
    caml_plat_lock(&self->interruptor.lock);
    caml_plat_signal(&self->interruptor.cond);
    caml_plat_unlock(&self->interruptor.lock);
  }

  caml_plat_lock(&self->domain_lock);
  caml_leave_blocking_section_hook();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  dom_internal* self = domain_self;

  caml_process_pending_signals();
  caml_enter_blocking_section_hook();
  atomic_store_rel(&self->backup_thread_msg, MSG_IN_BLOCKING_SECTION);
  /* Wakeup backup thread if it is sleeping */
  caml_plat_signal(&self->domain_cond);
  caml_plat_unlock(&self->domain_lock);
}

void caml_print_stats () {
  struct gc_stats s;
#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  struct detailed_stats ds;
  caml_domain_state* st;
  uint64_t total;
  int i;
#endif

  caml_gc_stat(Val_unit);
  caml_sample_gc_stats(&s);
  fprintf(stderr,"**** GC stats ****\n");
  fprintf(stderr, "Minor words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.minor_words);
  fprintf(stderr, "Promoted words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.promoted_words);
  fprintf(stderr, "Major words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.major_words);
  fprintf(stderr, "Minor collections:\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.minor_collections);
  fprintf(stderr, "Major collections:\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    Caml_state->stat_major_collections);

#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  memset(&ds,0,sizeof(struct detailed_stats));
  for (i=0; i<Max_domains; i++) {
    st = all_domains[i].state.state;
    if (st) {
      ds.allocations += st->allocations;

      ds.mutable_stores += st->mutable_stores;
      ds.immutable_stores += st->immutable_stores;

      ds.mutable_loads += st->mutable_loads;
      ds.immutable_loads += st->immutable_loads;

      ds.extcall_noalloc += st->extcall_noalloc;
      ds.extcall_alloc += st->extcall_alloc;
      ds.extcall_alloc_stackargs += st->extcall_alloc_stackargs;

      ds.tailcall_imm += st->tailcall_imm;
      ds.tailcall_ind += st->tailcall_ind;
      ds.call_imm += st->call_imm;
      ds.call_ind += st->call_ind;

      ds.stackoverflow_checks += st->stackoverflow_checks;
    }
  }
  fprintf(stderr, "\n**** Other stats ****\n");
  fprintf(stderr, "Allocations:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", ds.allocations);

  total = ds.mutable_loads + ds.immutable_loads;
  fprintf(stderr, "\nLoads:\t\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", total);
  fprintf(stderr, "Mutable loads:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.mutable_loads, (double)ds.mutable_loads * 100.0 / total);
  fprintf(stderr, "Immutable loads:\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.immutable_loads, (double)ds.immutable_loads * 100.0 / total);

  total = ds.mutable_stores + ds.immutable_stores;
  fprintf(stderr, "\nStores:\t\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", total);
  fprintf(stderr, "Mutable stores:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.mutable_stores, (double)ds.mutable_stores * 100.0 / total);
  fprintf(stderr, "Immutable stores:\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.immutable_stores, (double)ds.immutable_stores * 100.0 / total);

  total = ds.extcall_noalloc + ds.extcall_alloc + ds.extcall_alloc_stackargs;
  fprintf(stderr, "\nExternal calls:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", total);
  fprintf(stderr, "NoAlloc:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.extcall_noalloc, (double)ds.extcall_noalloc * 100.0 / total);
  fprintf(stderr, "Alloc:\t\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.extcall_alloc, (double)ds.extcall_alloc * 100.0 / total);
  fprintf(stderr, "Alloc + stack args:\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.extcall_alloc_stackargs, (double)ds.extcall_alloc_stackargs * 100.0 / total);

  total = ds.tailcall_imm + ds.tailcall_ind + ds.call_imm + ds.call_ind;
  fprintf(stderr, "\nCalls:\t\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", total);
  fprintf(stderr, "Imm tail:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.tailcall_imm, (double)ds.tailcall_imm * 100.0 / total);
  fprintf(stderr, "Ind tail:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.tailcall_ind, (double)ds.tailcall_ind * 100.0 / total);
  fprintf(stderr, "Imm non-tail:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.call_imm, (double)ds.call_imm * 100.0 / total);
  fprintf(stderr, "Ind non-tail:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.call_ind, (double)ds.call_ind * 100.0 / total);

  fprintf(stderr, "\nStackoverflow checks:\t%"ARCH_INTNAT_PRINTF_FORMAT"u (%.2lf%%)\n", ds.stackoverflow_checks, (double)ds.stackoverflow_checks * 100.0 / total);

#endif
}

CAMLexport int caml_domain_rpc(struct domain* domain,
                                domain_rpc_handler handler, void* data)
{
  return caml_send_interrupt(&domain_self->interruptor, &domain->internals->interruptor,
                      handler, data);
}



/* Generate functions for accessing domain state variables in debug mode */
#ifdef DEBUG
  #define DOMAIN_STATE(type, name) \
    type get_##name() { return Caml_state->name; }
  #include "caml/domain_state.tbl"
  #undef DOMAIN_STATE
#endif

/* Sending interrupts between domains.

   To avoid deadlock, some rules are important:

   - Don't hold interruptor locks for long
   - Don't hold two interruptor locks at the same time
   - Continue to handle incoming interrupts even when waiting for a response */

/* must be called with s->lock held */
static uintnat handle_incoming(struct interruptor* s)
{
  uintnat handled = 0;
  Assert (s->running);
  while (s->qhead != NULL) {
    struct interrupt* req = s->qhead;
    s->qhead = req->next;
    /* Unlock s while the handler runs, to allow other
       domains to send us messages. This is necessary to
       avoid deadlocks, since the handler might send
       interrupts */
    caml_plat_unlock(&s->lock);

    caml_ev_end_flow("interrupt", (uintnat)req);
    req->handler(caml_domain_self(), req->data, req);

    caml_plat_lock(&s->lock);
    handled++;
  }
  return handled;
}

void caml_acknowledge_interrupt(struct interrupt* req)
{
  atomic_store_rel(&req->completed, 1);
}

static void acknowledge_all_pending_interrupts()
{
  Assert(Caml_state->critical_section_nesting == 0);
  while (Caml_state->pending_interrupts) {
    interrupt* curr = Caml_state->pending_interrupts;
    Caml_state->pending_interrupts = curr->next;
    caml_acknowledge_interrupt(curr);
  }
}

static void handover_ephemerons(caml_domain_state* domain_state)
{
  value todo_tail = 0;
  value live_tail = 0;

  if (domain_state->ephe_info->todo == 0 &&
      domain_state->ephe_info->live == 0)
    return;

  todo_tail = caml_bias_ephe_list(domain_state->ephe_info->todo, (struct domain*)NULL);
  live_tail = caml_bias_ephe_list(domain_state->ephe_info->live, (struct domain*)NULL);
  caml_add_orphaned_ephe(domain_state->ephe_info->todo, todo_tail,
                         domain_state->ephe_info->live, live_tail);
  if (domain_state->ephe_info->todo != 0) {
    caml_ephe_todo_list_emptied();
  }
  domain_state->ephe_info->live = 0;
  domain_state->ephe_info->todo = 0;
}

static void handover_finalisers(caml_domain_state* domain_state)
{
  struct caml_final_info* f = domain_state->final_info;

  if (f->todo_head != NULL || f->first.size != 0 || f->last.size != 0) {
    /* have some final structures */
    if (caml_gc_phase != Phase_sweep_and_mark_main) {
      /* Force a major GC to simplify constraints for
      * handing over ephemerons. */
      caml_gc_major(Val_unit);
    }
    caml_add_orphaned_finalisers (f);
    /* Create a dummy final info */
    domain_state->final_info = caml_alloc_final_info();
  }
  caml_final_domain_terminate(domain_state);
}

int caml_domain_is_terminating ()
{
  struct interruptor* s = &domain_self->interruptor;
  return s->terminating;
}

static void domain_terminate()
{
  caml_domain_state* domain_state = domain_self->state.state;
  struct interruptor* s = &domain_self->interruptor;
  int finished = 0;

  caml_gc_log("Domain terminating");
  caml_ev_pause(EV_PAUSE_YIELD);
  s->terminating = 1;
  while (!finished) {
    caml_orphan_allocated_words();
    caml_finish_sweeping();

    caml_empty_minor_heaps_once();

    caml_finish_marking();
    handover_ephemerons(domain_state);
    handover_finalisers(domain_state);

    caml_plat_lock(&s->lock);

    /* The interaction of termination and major GC is quite subtle.
     *
     * At the end of the major GC, we decide the number of domains to mark and
     * sweep for the next cycle. If the following [handle_incoming] participates
     * in a major GC cycle, then we need to finish marking and sweeping again in
     * order to decrement the globals [num_domains_to_mark] and
     * [num_domains_to_sweep] (see major_gc.c). Luckily, if the following
     * [handle_incoming] does participate in a major GC cycle, then
     * [Caml_state->sweeping_done] will be set to 0 making conditional check to
     * fail, which forces this domain to finish marking and sweeping again.
     */

    if (handle_incoming(s) == 0 &&
        Caml_state->marking_done &&
        Caml_state->sweeping_done) {

      finished = 1;
      s->terminating = 0;
      s->running = 0;
      s->unique_id += Max_domains;
    }
    caml_plat_unlock(&s->lock);
  }

  caml_delete_root(domain_state->read_fault_ret_val);
  caml_stat_free(domain_state->final_info);
  caml_stat_free(domain_state->ephe_info);
  caml_teardown_major_gc();
  caml_teardown_shared_heap(domain_state->shared_heap);
  domain_state->shared_heap = 0;
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = 0;
  caml_free_signal_stack();

  if(domain_state->current_stack != NULL) {
    caml_free_stack(domain_state->current_stack);
  }

  if (Caml_state->critical_section_nesting) {
    Caml_state->critical_section_nesting = 0;
    acknowledge_all_pending_interrupts();
  }

  atomic_store_rel(&domain_self->backup_thread_msg, MSG_TERMINATE);
  caml_plat_signal(&domain_self->domain_cond);
  caml_plat_unlock(&domain_self->domain_lock);
  domain_self->backup_thread_running = 0;
  pthread_join(domain_self->backup_thread, 0);

  caml_plat_assert_all_locks_unlocked();
  /* This is the last thing we do because we need to be able to rely
     on caml_domain_alone (which uses num_domains_running) in at least
     the shared_heap lockfree fast paths */
  atomic_fetch_add(&num_domains_running, -1);
}

void caml_handle_incoming_interrupts()
{
  struct interruptor* s = &domain_self->interruptor;
  if (s->qhead == NULL) return;
  caml_plat_lock(&s->lock);
  handle_incoming(s);
  caml_plat_unlock(&s->lock);
}

static void caml_wait_interrupt_completed(struct interruptor* self, struct interrupt* req)
{
  int i;
  /* Often, interrupt handlers are fast, so spin for a bit before waiting */
  for (i=0; i<1000; i++) {
    if (atomic_load_acq(&req->completed)) {
      return;
    }
    cpu_relax();
  }

  while (!atomic_load_acq(&req->completed)) {
    cpu_relax();
    caml_plat_lock(&self->lock);
    handle_incoming(self);
    caml_plat_unlock(&self->lock);
  }
  return;
}

int caml_send_partial_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data,
                         struct interrupt* req)
{
  req->handler = handler;
  req->data = data;
  req->sender = self;
  atomic_store_rel(&req->completed, 0);
  req->next = NULL;

  caml_plat_lock(&target->lock);
  if (!target->running) {
    caml_plat_unlock(&target->lock);
    return 0;
  }

  caml_ev_begin_flow("interrupt", (uintnat)&req);
  /* add to wait queue */
  if (target->qhead) {
    /* queue was nonempty */
    target->qtail->next = req;
    target->qtail = req;
  } else {
    /* queue was empty */
    target->qhead = target->qtail = req;
  }
  /* Signal the condition variable, in case the target is
     itself waiting for an interrupt to be processed elsewhere */
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  atomic_store_rel(target->interrupt_word, INTERRUPT_MAGIC); //FIXME dup

  return 1;
}

int caml_send_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data)
{
  struct interrupt req;
  if (!caml_send_partial_interrupt(self, target, handler, data, &req))
    return 0;
  caml_wait_interrupt_completed(self, &req);
  return 1;
}


CAMLprim value caml_ml_domain_critical_section(value delta)
{
  intnat crit = Caml_state->critical_section_nesting + Long_val(delta);
  Caml_state->critical_section_nesting = crit;
  if (crit < 0) {
    caml_fatal_error("invalid critical section nesting");
  } else if (crit == 0) {
    acknowledge_all_pending_interrupts();
  }
  return Val_unit;
}

#define Chunk_size 0x400

CAMLprim value caml_ml_domain_yield(value unused)
{
  struct interruptor* s = &domain_self->interruptor;
  int found_work = 1;
  intnat left;

  if (Caml_state->critical_section_nesting == 0) {
    caml_failwith("Domain.Sync.wait must be called from within a critical section");
  }

  caml_ev_pause(EV_PAUSE_YIELD);

  caml_plat_lock(&s->lock);
  while (!Caml_state->pending_interrupts) {
    if (handle_incoming(s) == 0 && !found_work) {
      caml_plat_wait(&s->cond);
    } else {
      caml_plat_unlock(&s->lock);
      caml_opportunistic_major_collection_slice(Chunk_size, &left);
      if (left == Chunk_size)
        found_work = 0;
      caml_plat_lock(&s->lock);
    }
  }
  caml_plat_unlock(&s->lock);

  caml_ev_resume();
  return Val_unit;
}

static void handle_ml_interrupt(struct domain* d, void* unique_id_p, interrupt* req)
{
  if (d->internals->interruptor.unique_id != *(uintnat*)unique_id_p) {
    caml_acknowledge_interrupt(req);
    return;
  }
  if (d->state->critical_section_nesting > 0) {
    req->next = d->state->pending_interrupts;
    d->state->pending_interrupts = req;
  } else {
    caml_acknowledge_interrupt(req);
  }
}

CAMLprim value caml_ml_domain_interrupt(value domain)
{
  CAMLparam1 (domain);
  uintnat unique_id = (uintnat)Long_val(domain);
  struct interruptor* target =
    &all_domains[unique_id % Max_domains].interruptor;

  if (!caml_send_interrupt(&domain_self->interruptor, target, &handle_ml_interrupt, &unique_id)) {
    /* the domain might have terminated, but that's fine */
  }
  CAMLreturn (Val_unit);
}

CAMLprim int64_t caml_ml_domain_ticks_unboxed(value unused)
{
  return caml_time_counter() - startup_timestamp;
}

CAMLprim value caml_ml_domain_ticks(value unused)
{
  return caml_copy_int64(caml_ml_domain_ticks_unboxed(unused));
}

CAMLprim value caml_ml_domain_yield_until(value t)
{
  int64_t ts = Int64_val(t) + startup_timestamp;
  struct interruptor* s = &domain_self->interruptor;
  value ret = Val_int(1); /* Domain.Sync.Notify */
  int res;
  intnat left;
  int found_work = 1;

  if (Caml_state->critical_section_nesting == 0){
    caml_failwith("Domain.Sync.wait_until must be called from within a critical section");
  }

  caml_ev_pause(EV_PAUSE_YIELD);
  caml_plat_lock(&s->lock);

  while (!Caml_state->pending_interrupts) {
    if (ts < caml_time_counter ()) {
      ret = Val_int(0); /* Domain.Sync.Timeout */
      break;
    } else if (handle_incoming(s) == 0 && !found_work) {
      res = caml_plat_timedwait(&s->cond, ts);
      if (res) {
        ret = Val_int(0); /* Domain.Sync.Timeout */
        break;
      }
    } else {
      caml_plat_unlock(&s->lock);
      caml_opportunistic_major_collection_slice(Chunk_size, &left);
      if (left == Chunk_size)
        found_work = 0;
      caml_plat_lock(&s->lock);
    }
  }

  caml_plat_unlock(&s->lock);
  caml_ev_resume();

  return ret;
}

CAMLprim value caml_ml_domain_cpu_relax(value t)
{
  uintnat interrupts = 0;
  struct interruptor* s = &domain_self->interruptor;
  if (Caml_check_gc_interrupt(Caml_state)) {
    caml_plat_lock(&s->lock);
    interrupts = handle_incoming(s);
    caml_plat_unlock(&s->lock);
  }
  if (!interrupts) cpu_relax();
  return Val_unit;
}


