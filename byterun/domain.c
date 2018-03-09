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

  caml_plat_mutex roots_lock;

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
static dom_internal* stw_leader = 0;
static struct dom_internal all_domains[Max_domains];

static uintnat minor_heaps_base;
static __thread dom_internal* domain_self;

static int64_t startup_timestamp;

#ifdef __APPLE__
  /* OSX has issues with dynamic loading + exported TLS.
     This is slower but works */
  CAMLexport pthread_key_t caml_domain_state_key;
#else
  CAMLexport __thread caml_domain_state* caml_domain_curr_state;
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

  return bs;
}

void caml_reallocate_minor_heap(asize_t size)
{
  caml_domain_state* domain_state = Caml_state;
  Assert(domain_state->young_ptr == domain_state->young_end);

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit((void*)domain_self->minor_heap_area,
                    domain_self->minor_heap_area_end - domain_self->minor_heap_area);

  size = caml_norm_minor_heap_size(size);

  if (!caml_mem_commit((void*)domain_self->minor_heap_area, size)) {
    /* FIXME: handle this gracefully */
    caml_fatal_error("Fatal error: No memory for minor heap\n");
  }

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)domain_self->minor_heap_area;
    for (; p < (uintnat*)(domain_self->minor_heap_area + size); p++)
      *p = Debug_uninit_align;
  }
#endif

  Caml_state->minor_heap_size = size;
  domain_state->young_start = (char*)domain_self->minor_heap_area;
  domain_state->young_end = (char*)(domain_self->minor_heap_area + size);
  domain_state->young_ptr = domain_state->young_end;
}

/* must be run on the domain's thread */
static void create_domain(uintnat initial_minor_heap_size) {
  int i;
  dom_internal* d = 0;
  Assert (domain_self == 0);

  caml_plat_lock(&all_domains_lock);

  /* wait until any in-progress STW sections end */
  while (stw_leader) caml_plat_wait(&all_domains_cond);

  for (i = 0;
       i < Max_domains &&
         !d;
       i++) {
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
        atomic_store_rel(young_limit, d->minor_heap_area);
        s->interrupt_word = young_limit;
      }
      Assert(s->qhead == NULL);
      s->running = 1;
    }
    caml_plat_unlock(&s->lock);
  }

  if (d) {
    d->state.internals = d;
    domain_self = d;
    SET_Caml_state((void*)(d->tls_area));
    caml_domain_state* domain_state =
      (caml_domain_state*)(d->tls_area);
    caml_plat_lock(&d->roots_lock);

    domain_state->id = d->id;
    d->state.state = domain_state;

    /* FIXME: code below does not handle failure to allocate memory
       early in a domain's lifetime correctly */

    domain_state->young_start = domain_state->young_end =
      domain_state->young_ptr = 0;
    domain_state->remembered_set = caml_alloc_remembered_set();

    d->state.state->shared_heap = caml_init_shared_heap();
    caml_init_major_gc();
    caml_reallocate_minor_heap(initial_minor_heap_size);

    caml_init_main_stack();


    domain_state->backtrace_buffer = NULL;
#ifndef NATIVE_CODE
    domain_state->external_raise = NULL;
    domain_state->trap_sp_off = 1;
#endif
    caml_setup_eventlog();
    caml_ev_resume();
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

  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];
    uintnat domain_minor_heap_base;

    caml_plat_mutex_init(&dom->roots_lock);

    caml_plat_mutex_init(&dom->interruptor.lock);
    caml_plat_cond_init(&dom->interruptor.cond,
                        &dom->interruptor.lock);
    dom->interruptor.qhead = dom->interruptor.qtail = NULL;
    dom->interruptor.running = 0;
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


  create_domain(minor_size);
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

static void domain_terminate();
static void* domain_thread_func(void* v) {
  struct domain_startup_params* p = v;
  caml_root callback = p->callback;

  create_domain(caml_params->minor_heap_init);
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

  p.parent = &domain_self->interruptor;
  p.status = Dom_starting;

  p.callback = caml_create_root(caml_promote(&domain_self->state, callback));

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
  int i, found = 0;
  caml_plat_lock(&all_domains_lock);
  for (i = 0;
       i < Max_domains &&
         !found;
       i++) {
    struct interruptor* s = &all_domains[i].interruptor;
    if (s == &domain_self->interruptor) continue;
    caml_plat_lock(&s->lock);
    if (s->running) found = 1;
    caml_plat_unlock(&s->lock);
  }
  caml_plat_unlock(&all_domains_lock);
  return !found;
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
  void (*callback)(struct domain*, void*);
  void* data;

  int num_domains;
  atomic_uintnat barrier;
} stw_request = {
  ATOMIC_UINTNAT_INIT(0),
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  0,
  ATOMIC_UINTNAT_INIT(0)
};

/* sense-reversing barrier */
#define BARRIER_SENSE_BIT 0x100000

barrier_status caml_global_barrier_begin()
{
  uintnat b = 1 + atomic_fetch_add(&stw_request.barrier, 1);
  caml_gc_log("domain %d to barrier", (int)b);
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

static void stw_handler(struct domain* domain, void* unused2, interrupt* done)
{
  caml_ev_msg("starting STW");
  caml_acknowledge_interrupt(done);
  SPIN_WAIT {
    if (atomic_load_acq(&stw_request.domains_still_running) == 0)
      break;
    caml_handle_incoming_interrupts();
  }
  stw_request.callback(domain, stw_request.data);
  atomic_fetch_add(&stw_request.num_domains_still_processing, -1);
  SPIN_WAIT {
    if (atomic_load_acq(&stw_request.num_domains_still_processing) == 0)
      break;
  }
}

int caml_try_run_on_all_domains(void (*handler)(struct domain*, void*), void* data)
{
  int i;
  uintnat domains_participating = 1;

  caml_gc_log("requesting STW");

  /* Try to take the lock by setting ourselves as the stw_leader.
     If it fails, handle interrupts (probably participating in
     an STW section) and return. */
  caml_plat_lock(&all_domains_lock);
  if (stw_leader) {
    caml_plat_unlock(&all_domains_lock);
    caml_handle_incoming_interrupts();
    return 0;
  } else {
      stw_leader = domain_self;
  }
  caml_plat_unlock(&all_domains_lock);

  caml_ev_request_stw();
  caml_gc_log("causing STW");

  atomic_store_rel(&stw_request.domains_still_running, 1);

  /* Next, interrupt all domains, counting how many domains received
     the interrupt (i.e. are not terminated and are participating in
     this STW section). */
  for (i = 0; i < Max_domains; i++) {
    if (&all_domains[i] == domain_self) continue;
    if (caml_send_interrupt(&domain_self->interruptor,
                            &all_domains[i].interruptor,
                            stw_handler,
                            0)) {
      domains_participating++;
    }
  }

  stw_request.num_domains = domains_participating;
  atomic_store_rel(&stw_request.barrier, 0);
  atomic_store_rel(&stw_request.num_domains_still_processing,
                   domains_participating);
  stw_request.callback = handler;
  stw_request.data = data;

  atomic_store_rel(&stw_request.domains_still_running, 0);

  handler(&domain_self->state, data);

  /* release the STW lock before allowing other domains to continue */
  caml_plat_lock(&all_domains_lock);
  Assert (stw_leader == domain_self);
  stw_leader = 0;
  caml_plat_broadcast(&all_domains_cond);
  caml_plat_unlock(&all_domains_lock);
  atomic_fetch_add(&stw_request.num_domains_still_processing, -1);
  SPIN_WAIT {
    if (atomic_load_acq(&stw_request.num_domains_still_processing) == 0)
      break;
  }
  /* other domains might not have finished stw_handler yet, but they
     will finish as soon as they notice num_domains_still_processing
     == 0, which will remain the case until they have responded to
     another interrupt from caml_run_on_all_domains */
  return 1;
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
  if (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    while (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
      atomic_cas(young_limit, INTERRUPT_MAGIC, domain_self->minor_heap_area);
    }
    caml_ev_pause(EV_PAUSE_YIELD);
    caml_handle_incoming_interrupts();
    caml_ev_resume();
  }

  if (((uintnat)Caml_state->young_ptr - Bhsize_wosize(Max_young_wosize) <
       domain_self->minor_heap_area) ||
      Caml_state->force_major_slice) {
    /* out of minor heap or collection forced */
    Caml_state->force_major_slice = 0;
    caml_minor_collection();
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
  caml_plat_lock(&domain_self->roots_lock);
  caml_leave_blocking_section_hook();
  caml_restore_stack_gc();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_save_stack_gc();
  caml_enter_blocking_section_hook();
  caml_plat_unlock(&domain_self->roots_lock);
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
  fprintf(stderr, "Minor words:\t\t%lu\n", s.minor_words);
  fprintf(stderr, "Promoted words:\t\t%lu\n", s.promoted_words);
  fprintf(stderr, "Major words:\t\t%lu\n", s.major_words);
  fprintf(stderr, "Minor collections:\t%lu\n", s.minor_collections);
  fprintf(stderr, "Major collections:\t%lu\n", (uint64_t) Caml_state->stat_major_collections);

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
  fprintf(stderr, "Allocations:\t\t%llu\n", ds.allocations);

  total = ds.mutable_loads + ds.immutable_loads;
  fprintf(stderr, "\nLoads:\t\t\t%llu\n", total);
  fprintf(stderr, "Mutable loads:\t\t%llu (%.2lf%%)\n", ds.mutable_loads, (double)ds.mutable_loads * 100.0 / total);
  fprintf(stderr, "Immutable loads:\t%llu (%.2lf%%)\n", ds.immutable_loads, (double)ds.immutable_loads * 100.0 / total);

  total = ds.mutable_stores + ds.immutable_stores;
  fprintf(stderr, "\nStores:\t\t\t%llu\n", total);
  fprintf(stderr, "Mutable stores:\t\t%llu (%.2lf%%)\n", ds.mutable_stores, (double)ds.mutable_stores * 100.0 / total);
  fprintf(stderr, "Immutable stores:\t%llu (%.2lf%%)\n", ds.immutable_stores, (double)ds.immutable_stores * 100.0 / total);

  total = ds.extcall_noalloc + ds.extcall_alloc + ds.extcall_alloc_stackargs;
  fprintf(stderr, "\nExternal calls:\t\t%llu\n", total);
  fprintf(stderr, "NoAlloc:\t\t%llu (%.2lf%%)\n", ds.extcall_noalloc, (double)ds.extcall_noalloc * 100.0 / total);
  fprintf(stderr, "Alloc:\t\t\t%llu (%.2lf%%)\n", ds.extcall_alloc, (double)ds.extcall_alloc * 100.0 / total);
  fprintf(stderr, "Alloc + stack args:\t%llu (%.2lf%%)\n", ds.extcall_alloc_stackargs, (double)ds.extcall_alloc_stackargs * 100.0 / total);

  total = ds.tailcall_imm + ds.tailcall_ind + ds.call_imm + ds.call_ind;
  fprintf(stderr, "\nCalls:\t\t\t%llu\n", total);
  fprintf(stderr, "Imm tail:\t\t%llu (%.2lf%%)\n", ds.tailcall_imm, (double)ds.tailcall_imm * 100.0 / total);
  fprintf(stderr, "Ind tail:\t\t%llu (%.2lf%%)\n", ds.tailcall_ind, (double)ds.tailcall_ind * 100.0 / total);
  fprintf(stderr, "Imm non-tail:\t\t%llu (%.2lf%%)\n", ds.call_imm, (double)ds.call_imm * 100.0 / total);
  fprintf(stderr, "Ind non-tail:\t\t%llu (%.2lf%%)\n", ds.call_ind, (double)ds.call_ind * 100.0 / total);

  fprintf(stderr, "\nStackoverflow checks:\t%llu (%.2lf%%)\n", ds.stackoverflow_checks, (double)ds.stackoverflow_checks * 100.0 / total);

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

struct interrupt {
  /* immutable fields */
  domain_rpc_handler handler;
  void* data;
  struct interruptor* sender;

  atomic_uintnat completed;

  /* accessed only when target's lock held */
  struct interrupt* next;
};

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

    req->handler(caml_domain_self(), req->data, req);

    caml_plat_lock(&s->lock);
    handled++;
  }
  return handled;
}

void caml_acknowledge_interrupt(struct interrupt* req)
{
  /* We cannot access req after we signal completion, so save the sender's
     identity now. */
  struct interruptor* sender = req->sender;
  atomic_store_rel(&req->completed, 1);

  /* lock sender->lock so that we don't broadcast between check and wait */
  caml_plat_lock(&sender->lock);
  caml_plat_broadcast(&sender->cond);
  caml_plat_unlock(&sender->lock);
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

static void domain_terminate() {
  struct interruptor* s = &domain_self->interruptor;
  int finished = 0;

  caml_gc_log("Domain terminating");
  caml_ev_msg("Domain terminating");
  caml_ev_pause(EV_PAUSE_YIELD);
  while (!finished) {
    caml_finish_sweeping();
    caml_empty_minor_heap();
    caml_finish_marking();

    caml_plat_lock(&s->lock);
    if (handle_incoming(s) == 0 &&
        Caml_state->marking_done &&
        Caml_state->sweeping_done) {
      finished = 1;
      s->running = 0;
      s->unique_id += Max_domains;
    }
    caml_plat_unlock(&s->lock);
  }

  caml_teardown_major_gc();
  caml_teardown_shared_heap(domain_self->state.state->shared_heap);
  domain_self->state.state->shared_heap = 0;
  caml_free_remembered_set(domain_self->state.state->remembered_set);
  domain_self->state.state->remembered_set = 0;

  if (Caml_state->critical_section_nesting) {
    Caml_state->critical_section_nesting = 0;
    acknowledge_all_pending_interrupts();
  }
  caml_ev_resume();
  caml_enter_blocking_section();
  caml_ev_resume();
  caml_teardown_eventlog();
}

void caml_handle_incoming_interrupts()
{
  struct interruptor* s = &domain_self->interruptor;
  if (s->qhead == NULL) return;
  caml_plat_lock(&s->lock);
  handle_incoming(s);
  caml_plat_unlock(&s->lock);
}

int caml_send_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data)
{
  struct interrupt req;
  int i;

  req.handler = handler;
  req.data = data;
  req.sender = self;
  atomic_store_rel(&req.completed, 0);
  req.next = NULL;

  caml_plat_lock(&target->lock);
  if (!target->running) {
    caml_plat_unlock(&target->lock);
    return 0;
  }
  /* add to wait queue */
  if (target->qhead) {
    /* queue was nonempty */
    target->qtail->next = &req;
    target->qtail = &req;
  } else {
    /* queue was empty */
    target->qhead = target->qtail = &req;
  }
  /* Signal the condition variable, in case the target is
     itself waiting for an interrupt to be processed elsewhere */
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  atomic_store_rel(target->interrupt_word, INTERRUPT_MAGIC); //FIXME dup

  /* Often, interrupt handlers are fast, so spin for a bit before waiting */
  for (i=0; i<1000; i++) {
    if (atomic_load_acq(&req.completed)) {
      return 1;
    }
    cpu_relax();
  }

  caml_plat_lock(&self->lock);
  while (1) {
    handle_incoming(self);
    if (atomic_load_acq(&req.completed)) break;
    caml_plat_wait(&self->cond);
  }
  caml_plat_unlock(&self->lock);
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

#define Chunk_size 0x10000

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
      caml_ev_msg("wait");
      caml_plat_wait(&s->cond);
    } else {
      caml_plat_unlock(&s->lock);
      caml_major_collection_slice(Chunk_size, &left);
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

CAMLprim value caml_ml_domain_ticks(value unused)
{
  return caml_copy_int64(caml_time_counter() - startup_timestamp);
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
      caml_ev_msg("timed wait");
      res = caml_plat_timedwait(&s->cond, ts);
      if (res) {
        ret = Val_int(0); /* Domain.Sync.Timeout */
        break;
      }
    } else {
      caml_plat_unlock(&s->lock);
      caml_major_collection_slice(Chunk_size, &left);
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
  int interrupts;
  struct interruptor* s = &domain_self->interruptor;
  caml_plat_lock(&s->lock);
  interrupts = handle_incoming(s);
  caml_plat_unlock(&s->lock);
  if (!interrupts) cpu_relax();
  return Val_unit;
}
