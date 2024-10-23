/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1995 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#define _GNU_SOURCE /* helps to find pthread_setname_np() */
#include "caml/config.h"

#if defined(_WIN32)
#  include <windows.h>
#  include <processthreadsapi.h>
#  include <caml/osdeps.h>
#elif defined(HAS_PRCTL)
#  include <sys/prctl.h>
#elif defined(HAS_PTHREAD_SETNAME_NP) || defined(HAS_PTHREAD_SET_NAME_NP)
#  include <pthread.h>

#  if defined(HAS_PTHREAD_NP_H)
#    include <pthread_np.h>
#  endif
#endif

#include "caml/misc.h"

#if defined(_WIN32) && !defined(NATIVE_CODE) && !defined(_MSC_VER)
/* Ensure that pthread.h marks symbols __declspec(dllimport) so that they can be
   picked up from the runtime (which will have linked winpthreads statically).
   mingw-w64 11.0.0 introduced WINPTHREADS_USE_DLLIMPORT to do this explicitly;
   prior versions co-opted this on the internal DLL_EXPORT, but this is ignored
   in 11.0 and later unless IN_WINPTHREAD is also defined, so we can safely
   define both to support both versions.
   When compiling with MSVC, we currently link directly the winpthreads objects
   into our runtime, so we do not want to mark its symbols with
   __declspec(dllimport). */
#define WINPTHREADS_USE_DLLIMPORT
#define DLL_EXPORT
#endif

#include <stdbool.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/startup_aux.h"
#include "caml/sync.h"
#include "caml/sys.h"
#include "caml/memprof.h"

/* "caml/threads.h" is *not* included since it contains the _external_
   declarations for the caml_c_thread_register and caml_c_thread_unregister
   functions. */

/* Max computation time before rescheduling, in milliseconds */
#define Thread_timeout 50

typedef int st_retcode;

static void st_bt_lock_acquire(void) {

  /* We do not want to signal the backup thread if it is not "working"
     as it may very well not be, because we could have just resumed
     execution from another thread right away. */
  if (caml_bt_is_in_blocking_section()) {
    caml_bt_enter_ocaml();
  }

  caml_acquire_domain_lock();

  return;
}

static void st_bt_lock_release(bool other_threads_waiting) {

  /* Here we do want to signal the backup thread iff there's
     no thread waiting to be scheduled, and the backup thread is currently
     idle. */
  if (other_threads_waiting && caml_bt_is_in_blocking_section() == 0) {
    caml_bt_exit_ocaml();
  }

  caml_release_domain_lock();

  return;
}

/* OS-specific code */
#ifdef _WIN32
#include "st_win32.h"
#else
#include "st_posix.h"
#endif

/* The ML value describing a thread (heap-allocated) */

#define Ident(v) Field(v, 0)
#define Start_closure(v) Field(v, 1)
#define Terminated(v) Field(v, 2)

/* The infos on threads (allocated via caml_stat_alloc()) */

struct caml_thread_struct {

  value descr;              /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next; /* Doubly-linked list of running threads */
  struct caml_thread_struct * prev;
  int domain_id;      /* The id of the domain to which this thread belongs */
  struct stack_info* current_stack;      /* saved Caml_state->current_stack */
  struct c_stack_link* c_stack;          /* saved Caml_state->c_stack */
  /* Note: we do not save Caml_state->stack_cache, because it can
     safely be shared between all threads on the same domain. */
  struct caml__roots_block *local_roots; /* saved value of local_roots */
  int backtrace_pos;           /* saved value of Caml_state->backtrace_pos */
  backtrace_slot * backtrace_buffer;
    /* saved value of Caml_state->backtrace_buffer */
  value backtrace_last_exn;
    /* saved value of Caml_state->backtrace_last_exn (root) */
  value * gc_regs;           /* saved value of Caml_state->gc_regs */
  value * gc_regs_buckets;   /* saved value of Caml_state->gc_regs_buckets */
  void * exn_handler;        /* saved value of Caml_state->exn_handler */
  memprof_thread_t memprof;  /* memprof's internal thread data structure */
  void * signal_stack;       /* this thread's signal stack */

#ifndef NATIVE_CODE
  intnat trap_sp_off;      /* saved value of Caml_state->trap_sp_off */
  intnat trap_barrier_off; /* saved value of Caml_state->trap_barrier_off */
  struct caml_exception_context* external_raise;
    /* saved value of Caml_state->external_raise */
#endif
};

typedef struct caml_thread_struct* caml_thread_t;

/* Thread-local key for accessing the current thread's [caml_thread_t] */
st_tlskey caml_thread_key;

#define This_thread ((caml_thread_t) st_tls_get(caml_thread_key))

/* overall table for threads across domains */
struct caml_thread_table {
  caml_thread_t active_thread;
  st_masterlock thread_lock;
  int tick_thread_running;
  st_thread_id tick_thread_id;
  atomic_uintnat tick_thread_stop;
};

/* thread_table instance, up to caml_params->max_domains */
static struct caml_thread_table* thread_table;

#define Thread_lock(dom_id) &thread_table[dom_id].thread_lock

static void thread_lock_acquire(int dom_id)
{
  st_masterlock_acquire(Thread_lock(dom_id));
}

static void thread_lock_release(int dom_id)
{
  st_masterlock_release(Thread_lock(dom_id));
}

/* Used to signal that the "tick" thread for this domain should be stopped. */
#define Tick_thread_stop thread_table[Caml_state->id].tick_thread_stop

/* The remaining fields are accessed while holding the domain lock */

/* The descriptor for the currently executing thread for this domain;
   also the head of a circular list of thread descriptors for this
   domain. Invariant: at every safe point, either Active_thread is
   NULL, or Caml_state is setup for Active_thread. */
#define Active_thread thread_table[Caml_state->id].active_thread

/* Whether the "tick" thread is already running for this domain */
#define Tick_thread_running thread_table[Caml_state->id].tick_thread_running

/* The thread identifier of the "tick" thread for this domain */
#define Tick_thread_id thread_table[Caml_state->id].tick_thread_id

/* Identifier for next thread creation */
static atomic_uintnat thread_next_id = 0;

/* Forward declarations */
static value caml_threadstatus_new (void);
static void caml_threadstatus_terminate (value);
static st_retcode caml_threadstatus_wait (value);

/* Hook for scanning the stacks of the other threads */

static scan_roots_hook prev_scan_roots_hook;

static void caml_thread_scan_roots(
  scanning_action action, scanning_action_flags fflags, void *fdata,
  caml_domain_state *domain_state)
{
  const caml_thread_t active = thread_table[domain_state->id].active_thread;
  caml_thread_t th = active;

  /* The GC could be triggered before [active_thread] is initialized,
     or after [caml_thread_domain_stop_hook] has been called; in this
     case do nothing. */
  if (active != NULL) {
    do {
      (*action)(fdata, th->descr, &th->descr);
      (*action)(fdata, th->backtrace_last_exn, &th->backtrace_last_exn);
      /* Don't rescan the stack of the current thread, it was done already */
      if (th != active) {
        if (th->current_stack != NULL)
          caml_do_local_roots(action, fflags, fdata,
                              th->local_roots, th->current_stack, th->gc_regs);
      }
      th = th->next;
    } while (th != active);
  }

  /* Hook */
  if (prev_scan_roots_hook != NULL)
    (*prev_scan_roots_hook)(action, fflags, fdata, domain_state);

  return;
}

/* Saving and restoring runtime state in this_thread */

static void save_runtime_state(void)
{
  CAMLassert(This_thread != NULL);
  caml_thread_t this_thread = This_thread;
  this_thread->current_stack = Caml_state->current_stack;
  this_thread->c_stack = Caml_state->c_stack;
  this_thread->gc_regs = Caml_state->gc_regs;
  this_thread->gc_regs_buckets = Caml_state->gc_regs_buckets;
  this_thread->exn_handler = Caml_state->exn_handler;
  this_thread->local_roots = Caml_state->local_roots;
  this_thread->backtrace_pos = Caml_state->backtrace_pos;
  this_thread->backtrace_buffer = Caml_state->backtrace_buffer;
  this_thread->backtrace_last_exn = Caml_state->backtrace_last_exn;
#ifndef NATIVE_CODE
  this_thread->trap_sp_off = Caml_state->trap_sp_off;
  this_thread->trap_barrier_off = Caml_state->trap_barrier_off;
  this_thread->external_raise = Caml_state->external_raise;
#endif
}

static void restore_runtime_state(caml_thread_t th)
{
  CAMLassert(th != NULL);
  Active_thread = th;
  Caml_state->current_stack = th->current_stack;
  Caml_state->c_stack = th->c_stack;
  Caml_state->gc_regs = th->gc_regs;
  Caml_state->gc_regs_buckets = th->gc_regs_buckets;
  Caml_state->exn_handler = th->exn_handler;
  Caml_state->local_roots = th->local_roots;
  Caml_state->backtrace_pos = th->backtrace_pos;
  Caml_state->backtrace_buffer = th->backtrace_buffer;
  caml_modify_generational_global_root
    (&Caml_state->backtrace_last_exn, th->backtrace_last_exn);
#ifndef NATIVE_CODE
  Caml_state->trap_sp_off = th->trap_sp_off;
  Caml_state->trap_barrier_off = th->trap_barrier_off;
  Caml_state->external_raise = th->external_raise;
#endif
  caml_memprof_enter_thread(th->memprof);
}

CAMLprim value caml_thread_cleanup(value unit);

static void reset_active(void)
{
  Active_thread = NULL;
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  caml_thread_cleanup(Val_unit);
}

/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */

static void caml_thread_enter_blocking_section(void)
{
  /* Save the current runtime state in the thread descriptor
     of the current thread */
  save_runtime_state();
  /* Tell other threads that the runtime is free */
  thread_lock_release(Caml_state->id);
}

static void caml_thread_leave_blocking_section(void)
{
  caml_thread_t th = This_thread;
  /* Wait until the runtime is free */
  thread_lock_acquire(th->domain_id);
  /* Update Active_thread to point to the thread descriptor
     corresponding to the thread currently executing and restore the
     runtime state */
  restore_runtime_state(th);
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */
static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th = NULL;
  caml_domain_state *domain_state = Caml_state;
  uintnat stack_wsize = caml_get_init_stack_wsize();

  th = (caml_thread_t)caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;

  th->descr = Val_unit;
  th->next = NULL;
  th->prev = NULL;

  th->domain_id = domain_state->id;
  th->signal_stack = NULL;

  /* The remaining fields which store the values of the domain state
     must be initialized to a valid value, as in [domain_create]. */

  th->current_stack = caml_alloc_main_stack(stack_wsize);
  if (th->current_stack == NULL) goto out_err1;

  th->memprof = caml_memprof_new_thread(domain_state);
  if (th->memprof == NULL) goto out_err2;

  th->c_stack = NULL;
  th->local_roots = NULL;
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  th->backtrace_last_exn = Val_unit;
  th->gc_regs = NULL;
  th->gc_regs_buckets = NULL;
  th->exn_handler = NULL;

#ifndef NATIVE_CODE
  th->trap_sp_off = 1;
  th->trap_barrier_off = 2; /* TODO: 0? trap_barrier_block? */
  th->external_raise = NULL;
#endif
  return th;

 out_err2:
  caml_free_stack(th->current_stack);
 out_err1:
  caml_stat_free(th);
  return NULL;
}

/* Free the resources held by a thread. */
void caml_thread_free_info(caml_thread_t th)
{
  /* the following fields do not need any specific cleanup:
     descr: heap-allocated
     c_stack: stack-allocated
     local_roots: stack-allocated
     backtrace_last_exn: heap-allocated
     gc_regs:
       must be empty for a terminated thread
       (we assume that the C call stack must be empty at
        thread-termination point, so there are no gc_regs buckets in
        use in this variable nor on the stack)
     exn_handler: stack-allocated
     external_raise: stack-allocated
     init_mask: stack-allocated
  */
  caml_memprof_delete_thread(th->memprof);
  caml_free_stack(th->current_stack);
  caml_free_backtrace_buffer(th->backtrace_buffer);

  /* Remark: we could share gc_regs_buckets between threads on a same
     domain, but this might break the invariant that it is always
     non-empty at the point where we switch from OCaml to C, so we
     would need to do something more complex when activating a thread
     to restore this invariant. */
  caml_free_gc_regs_buckets(th->gc_regs_buckets);

  caml_stat_free(th);
}

/* Allocate a thread descriptor block. */

static value caml_thread_new_descriptor(value clos)
{
  CAMLparam1(clos);
  CAMLlocal1(mu);
  value descr;
  /* Create and initialize the termination semaphore */
  mu = caml_threadstatus_new();
  /* Create a descriptor for the new thread */
  descr = caml_alloc_3(0, Val_long(atomic_fetch_add(&thread_next_id, +1)),
                       clos, mu);
  CAMLreturn(descr);
}

/* Allocate a thread info block and add it to the list of threads */
static caml_thread_t thread_alloc_and_add(void)
{
  caml_thread_t th = caml_thread_new_info();

  if (th == NULL) return NULL;

  CAMLassert(Active_thread != NULL);
  th->next = Active_thread->next;
  th->prev = Active_thread;

  Active_thread->next->prev = th;
  Active_thread->next = th;

  return th;
}

/* Remove a thread info block from the list of threads
   and free its resources. */
static void caml_thread_remove_and_free(caml_thread_t th)
{
  if (th->next == th)
    reset_active(); /* last OCaml thread exiting */
  else if (Active_thread == th)
    restore_runtime_state(th->next); /* PR#5295 */
  th->next->prev = th->prev;
  th->prev->next = th->next;

  caml_thread_free_info(th);
  return;
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */
/* TODO(engil): more work on the multicore fork machinery. */

static void caml_thread_reinitialize(void)
{
  /* caml_thread_reinitialize is called as part of the
     caml_atfork_hook, and therefore this function is executed right
     after a fork.

     Note: "If a multi-threaded process calls fork() [...] the child
     process may only execute async-signal-safe operations until such
     time as one of the exec functions is called." (POSIX fork())

     Keeping OCaml+threads running after a fork is best-effort, and
     relies on having a single domain whose domain lock ensures some
     consistency of state. (If there are other C threads running we
     are hopeless.)
  */

  caml_thread_t th, next;

  th = Active_thread->next;
  while (th != Active_thread) {
    next = th->next;
    caml_thread_free_info(th);
    th = next;
  }
  Active_thread->next = Active_thread;
  Active_thread->prev = Active_thread;

  /* Within the child, the domain_lock needs to be reset and acquired. */
  caml_reset_domain_lock();
  caml_acquire_domain_lock();
  /* The master lock needs to be initialized again. This process will also be
     the effective owner of the lock. So there is no need to run
     st_masterlock_acquire (busy = 1) */
  st_masterlock *m = Thread_lock(Caml_state->id);
  m->init = false; /* force reinitialization */
  /* Note: initializing an already-initialized mutex and cond variable
     is UB (especially mutexes that are locked). This is best
     effort. */
  if (st_masterlock_init(m) != 0)
    caml_fatal_error("Unix.fork: failed to reinitialize master lock");

  /* Reinitialize IO mutexes, in case the fork happened while another thread
     had locked the channel. If so, we're likely in an inconsistent state,
     but we may be able to proceed anyway. */
  for (struct channel *chan = caml_all_opened_channels;
       chan != NULL;
       chan = chan->next) {
    caml_plat_mutex_init(&chan->mutex);
  }
}

CAMLprim value caml_thread_join(value th);

/* This hook is run when a domain shuts down (see domains.c).

   When a domain shuts down, the state must be cleared to allow proper reuse of
   the domain slot the next time a domain is started on this slot. If a program
   is single-domain, we mimic OCaml 4's behavior and do not care about ongoing
   thread: the program will exit. */
static void caml_thread_domain_stop_hook(void) {
  /* If the program runs multiple domains, we should not let systhreads to hang
     around when a domain exits. If the domain is not the last one (and the last
     one will always be domain 0) we force the domain to join on every thread
     on its chain before wrapping up. */
  if (!caml_domain_alone()) {

    while (Active_thread->next != Active_thread) {
      caml_thread_join(Active_thread->next->descr);
    }

    /* another domain thread may be joining on this domain's descriptor */
    caml_threadstatus_terminate(Terminated(Active_thread->descr));
    /* Shut down the tick thread */
    reset_active();
    /* We free the thread info but not its resources: they are owned
       by Caml_state at this point, and will be cleaned-up later. */
    caml_stat_free(This_thread);
  };
}

/* FIXME: this should return an encoded exception for use in
   domain_thread_func, but the latter is not ready to handle it
   yet. */
static void caml_thread_domain_initialize_hook(void)
{
  caml_thread_t new_thread;

  atomic_store_release(&Tick_thread_stop, 0);

  int ret = st_masterlock_init(Thread_lock(Caml_state->id));
  caml_check_error(ret, "caml_thread_domain_initialize_hook");

  new_thread =
    (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));

  new_thread->domain_id = Caml_state->id;
  new_thread->descr = caml_thread_new_descriptor(Val_unit);
  new_thread->next = new_thread;
  new_thread->prev = new_thread;
  new_thread->backtrace_last_exn = Val_unit;
  new_thread->memprof = caml_memprof_main_thread(Caml_state);
  new_thread->signal_stack = NULL;

  st_tls_set(caml_thread_key, new_thread);

  Active_thread = new_thread;
  caml_memprof_enter_thread(new_thread->memprof);
}

static void thread_yield(void);

void caml_thread_interrupt_hook(void)
{
  /* Do not attempt to yield from the backup thread */
  if (caml_bt_is_self()) return;

  uintnat is_on = 1;
  atomic_uintnat* req_external_interrupt =
    &Caml_state->requested_external_interrupt;

  if (atomic_compare_exchange_strong(req_external_interrupt, &is_on, 0)) {
    thread_yield();
  }

  return;
}

static atomic_bool threads_initialized = false;

/* [caml_thread_initialize] initialises the systhreads infrastructure. This
   function first sets up the chain for systhreads on this domain, then setup
   the global variables and hooks for systhreads to cooperate with the runtime
   system. */
CAMLprim value caml_thread_initialize(value unit)
{
  /* Protect against repeated initialization (PR#3532) */
  if (threads_initialized) return Val_unit;

  if (!caml_domain_alone())
    caml_failwith("caml_thread_initialize: cannot initialize Thread "
                  "while several domains are running.");

  thread_table = caml_stat_calloc_noexc(caml_params->max_domains,
                                        sizeof(struct caml_thread_table));
  if (thread_table == NULL)
    caml_fatal_error("caml_thread_initialize: failed to allocate thread"
                     " table");

  /* Initialize the key to the [caml_thread_t] structure */
  st_tls_newkey(&caml_thread_key);

  /* First initialise the systhread chain on this domain */
  caml_thread_domain_initialize_hook();

  prev_scan_roots_hook = atomic_exchange(&caml_scan_roots_hook,
                                         caml_thread_scan_roots);
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
  caml_domain_external_interrupt_hook = caml_thread_interrupt_hook;
  caml_domain_initialize_hook = caml_thread_domain_initialize_hook;
  caml_domain_stop_hook = caml_thread_domain_stop_hook;
  caml_atfork_hook = caml_thread_reinitialize;

  threads_initialized = true;

  return Val_unit;
}

/* Cleanup the thread machinery when the runtime is shut down. Joining the tick
   thread take 25ms on average / 50ms in the worst case, so we don't do it on
   program exit. (FIXME: not implemented in OCaml 5 yet) */

CAMLprim value caml_thread_cleanup(value unit)
{
  if (Tick_thread_running){
    atomic_store_release(&Tick_thread_stop, 1);
    st_thread_join(Tick_thread_id);
    atomic_store_release(&Tick_thread_stop, 0);
    Tick_thread_running = 0;
  }

  return Val_unit;
}

static void thread_detach_from_runtime(void)
{
  caml_thread_t th = This_thread;
  CAMLassert(th == Active_thread);
  /* PR#5188, PR#7220: some of the global runtime state may have
     changed as the thread was running, so we save it in the
     This_thread data to make sure that the cleanup logic
     below uses accurate information. */
  save_runtime_state();
  /* The main domain thread does not go through
     [thread_detach_from_runtime]. There is always one more thread in
     the chain at this point in time. */
  CAMLassert(th->next != th);
  /* Signal that the thread has terminated */
  caml_threadstatus_terminate(Terminated(th->descr));
  /* Remove signal stack */
  CAMLassert(th->signal_stack != NULL);
  caml_free_signal_stack(th->signal_stack);
  /* The following also sets Active_thread to a sane value in case the
     backup thread does a GC before the domain lock is acquired
     again. */
  caml_thread_remove_and_free(th);
  /* Forget the now-freed thread info */
  st_tls_set(caml_thread_key, NULL);
  /* Release domain lock */
  thread_lock_release(Caml_state->id);
}

/* Register current thread */
static void thread_init_current(caml_thread_t th)
{
  st_tls_set(caml_thread_key, th);
  restore_runtime_state(th);
  th->signal_stack = caml_init_signal_stack();
}

/* Create a thread */

/* the thread lock is not held when entering */
static void * caml_thread_start(void * v)
{
  caml_thread_t th = (caml_thread_t) v;
  int dom_id = th->domain_id;
  value clos;

  /* Acquire lock of domain */
  caml_init_domain_self(dom_id);
  thread_lock_acquire(dom_id);

  thread_init_current(th);

  clos = Start_closure(Active_thread->descr);
  caml_modify(&(Start_closure(Active_thread->descr)), Val_unit);
  caml_callback_exn(clos, Val_unit);
  thread_detach_from_runtime();
  return 0;
}

struct caml_thread_tick_args {
  int domain_id;
  atomic_uintnat* stop;
};

/* The tick thread: interrupt the domain periodically to force preemption  */
static void * caml_thread_tick(void * arg)
{
  struct caml_thread_tick_args* tick_thread_args =
    (struct caml_thread_tick_args*) arg;
  int domain_id = tick_thread_args->domain_id;
  atomic_uintnat* stop = tick_thread_args->stop;
  caml_stat_free(tick_thread_args);

  caml_init_domain_self(domain_id);
  caml_domain_state *domain = Caml_state;

  while(! atomic_load_acquire(stop)) {
    st_msleep(Thread_timeout);

    atomic_store_release(&domain->requested_external_interrupt, 1);
    caml_interrupt_self();
  }
  return NULL;
}

static st_retcode create_tick_thread(void)
{
  if (Tick_thread_running) return 0;

#ifdef POSIX_SIGNALS
  sigset_t mask, old_mask;

  /* Block all signals, so that we do not try to execute a C signal
     handler in the new tick thread. */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

  struct caml_thread_tick_args* tick_thread_args =
    caml_stat_alloc_noexc(sizeof(struct caml_thread_tick_args));
  if (tick_thread_args == NULL)
    caml_fatal_error("create_tick_thread: failed to allocate thread args");

  tick_thread_args->domain_id = Caml_state->id;
  tick_thread_args->stop = &Tick_thread_stop;

  st_retcode err = st_thread_create(&Tick_thread_id, caml_thread_tick,
                                    (void *)tick_thread_args);

#ifdef POSIX_SIGNALS
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  if (err != 0) return err;

  Tick_thread_running = 1;
  return 0;
}

CAMLprim value caml_thread_new(value clos)
{
  CAMLparam1(clos);

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support multithreaded programs");
#endif

  /* Create the tick thread if not already done.
     Because of PR#4666, we start the tick thread late, only when we create
     the first additional thread in the current process */
  st_retcode err = create_tick_thread();
  caml_check_error(err, "Thread.create");

  /* Create a thread info block */
  caml_thread_t th = thread_alloc_and_add();
  if (th == NULL) caml_raise_out_of_memory();
  th->descr = caml_thread_new_descriptor(clos);

  err = st_thread_create(NULL, caml_thread_start, (void *) th);

  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_and_free(th);
    caml_check_error(err, "Thread.create");
  }

  CAMLreturn(th->descr);
}

/* Register a thread already created from C */

#define Dom_c_threads 0

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_register(void)
{
  /* Systhreads initialized? */
  if (!threads_initialized) return 0;
  /* Already registered? */
  if (This_thread != NULL) return 0;

  /* At this point we should not hold any domain lock */
  CAMLassert(Caml_state_opt == NULL);

  /* Acquire lock of domain */
  caml_init_domain_self(Dom_c_threads);
  thread_lock_acquire(Dom_c_threads);

  /* Create tick thread if not already done */
  st_retcode err = create_tick_thread();
  if (err != 0) goto out_err;

  /* Set a thread info block */
  caml_thread_t th = thread_alloc_and_add();
  /* If it fails, we release the lock and return an error. */
  if (th == NULL) goto out_err;
  thread_init_current(th);
  /* We can now allocate the thread descriptor on the major heap */
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */

  /* Release the domain lock the regular way. Note: we cannot receive
     an exception here. */
  caml_enter_blocking_section_no_pending();
  return 1;

out_err:
  /* Note: we cannot raise an exception here. */
  thread_lock_release(Dom_c_threads);
  return 0;
}

/* Unregister a thread that was created from C and registered with
   the function above */

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_unregister(void)
{
  /* If [This_thread] is not set, then the thread was not registered */
  if (This_thread == NULL) return 0;
  /* Acquire the domain lock the regular way */
  caml_leave_blocking_section();
  /* Detach thread from the OCaml runtime; note that this resets
     [Caml_state_opt] and [This_thread]. */
  thread_detach_from_runtime();
  return 1;
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)
{
  return Active_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)
{
  char * msg = caml_format_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(Active_thread->descr)), msg);
  caml_stat_free(msg);
  if (Caml_state->backtrace_active) caml_print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Allow re-scheduling */

static void thread_yield(void)
{
  st_masterlock *m = Thread_lock(Caml_state->id);
  if (st_masterlock_waiters(m) == 0)
    return;

  /* Do all the parts of a blocking section enter&leave except lock
     manipulation, which we will do more efficiently in
     st_thread_yield, and executing signal handlers (which is already
     done at this point as part of the asynchronous actions mechanism
     when forcing a systhread yield). (Since our blocking section does
     not contain anything interesting, do not bother saving errno.)
  */

  save_runtime_state();
  st_thread_yield(m);
  restore_runtime_state(This_thread);

  /* Switching threads might have unmasked some signal. */
  if (caml_check_pending_signals())
    caml_set_action_pending(Caml_state);
}

CAMLprim value caml_thread_yield(value unit)
{
  caml_process_pending_actions();
  thread_yield();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)
{
  st_retcode rc = caml_threadstatus_wait(Terminated(th));
  caml_check_error(rc, "Thread.join");
  return Val_unit;
}

/* Thread status blocks */

#define Threadstatus_val(v) (* ((st_event *) Data_custom_val(v)))

static void caml_threadstatus_finalize(value wrapper)
{
  st_event_destroy(Threadstatus_val(wrapper));
}

static int caml_threadstatus_compare(value wrapper1, value wrapper2)
{
  st_event ts1 = Threadstatus_val(wrapper1);
  st_event ts2 = Threadstatus_val(wrapper2);
  return ts1 == ts2 ? 0 : ts1 < ts2 ? -1 : 1;
}

static struct custom_operations caml_threadstatus_ops = {
  "_threadstatus",
  caml_threadstatus_finalize,
  caml_threadstatus_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value caml_threadstatus_new (void)
{
  st_event ts = NULL;           /* suppress warning */
  value wrapper;
  caml_check_error(st_event_create(&ts), "Thread.create");
  wrapper = caml_alloc_custom(&caml_threadstatus_ops,
                              sizeof(st_event *),
                              0, 1);
  Threadstatus_val(wrapper) = ts;
  return wrapper;
}

static void caml_threadstatus_terminate (value wrapper)
{
  st_event_trigger(Threadstatus_val(wrapper));
}

static st_retcode caml_threadstatus_wait (value wrapper)
{
  CAMLparam1(wrapper); /* prevent deallocation of ts */
  st_event ts = Threadstatus_val(wrapper);
  st_retcode retcode;

  caml_enter_blocking_section();
  retcode = st_event_wait(ts);
  caml_leave_blocking_section();

  CAMLreturnT(st_retcode, retcode);
}

/* Set the current thread's name. */
CAMLprim value caml_set_current_thread_name(value name)
{
#if defined(_WIN32)
  wchar_t *thread_name = caml_stat_strdup_to_utf16(String_val(name));
  SetThreadDescription(GetCurrentThread(), thread_name);
  caml_stat_free(thread_name);

  // We are using both methods.
  // See: https://github.com/ocaml/ocaml/pull/13504#discussion_r1786358928
  pthread_setname_np(pthread_self(), String_val(name));
#elif defined(HAS_PRCTL)
  prctl(PR_SET_NAME, String_val(name));
#elif defined(HAS_PTHREAD_SETNAME_NP)
#  if defined(__APPLE__)
  pthread_setname_np(String_val(name));
#  elif defined(__NetBSD__)
  pthread_setname_np(pthread_self(), "%s", String_val(name));
#  else
  pthread_setname_np(pthread_self(), String_val(name));
#  endif
#elif defined(HAS_PTHREAD_SET_NAME_NP)
  pthread_set_name_np(pthread_self(), String_val(name));
#else
  if (caml_runtime_warnings_active()) {
    fprintf(stderr, "set thread name not implemented\n");
    fflush(stderr);
  }
#endif

  return Val_unit;
}
