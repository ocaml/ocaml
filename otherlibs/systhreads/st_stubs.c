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
#include "caml/sync.h"
#include "caml/sys.h"
#include "caml/memprof.h"

#include "../../runtime/sync_posix.h"

/* threads.h is *not* included since it contains the _external_ declarations for
   the caml_c_thread_register and caml_c_thread_unregister functions. */

/* Max computation time before rescheduling, in milliseconds */
#define Thread_timeout 50

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

#ifndef NATIVE_CODE
  intnat trap_sp_off;      /* saved value of Caml_state->trap_sp_off */
  intnat trap_barrier_off; /* saved value of Caml_state->trap_barrier_off */
  struct caml_exception_context* external_raise;
    /* saved value of Caml_state->external_raise */
#endif

#ifdef POSIX_SIGNALS
  sigset_t init_mask;
#endif
};

typedef struct caml_thread_struct* caml_thread_t;

/* Thread-local key for accessing the current thread's [caml_thread_t] */
st_tlskey caml_thread_key;

/* overall table for threads across domains */
struct caml_thread_table {
  caml_thread_t active_thread;
  st_masterlock thread_lock;
  int tick_thread_running;
  st_thread_id tick_thread_id;
};

/* thread_table instance, up to Max_domains */
static struct caml_thread_table thread_table[Max_domains];

#define Thread_lock(dom_id) &thread_table[dom_id].thread_lock

static void thread_lock_acquire(int dom_id)
{
  st_masterlock_acquire(Thread_lock(dom_id));
}

static void thread_lock_release(int dom_id)
{
  st_masterlock_release(Thread_lock(dom_id));
}

/* The remaining fields are accessed while holding the domain lock */

/* The descriptor for the currently executing thread for this domain;
   also the head of a circular list of thread descriptors for this
   domain. */
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
  caml_thread_t th;

  th = Active_thread;

  /* GC could be triggered before [Active_thread] is initialized */
  if (th != NULL) {
    do {
      (*action)(fdata, th->descr, &th->descr);
      (*action)(fdata, th->backtrace_last_exn, &th->backtrace_last_exn);
      if (th != Active_thread) {
        if (th->current_stack != NULL)
          caml_do_local_roots(action, fflags, fdata,
                              th->local_roots, th->current_stack, th->gc_regs);
      }
      th = th->next;
    } while (th != Active_thread);

  };

  if (prev_scan_roots_hook != NULL)
    (*prev_scan_roots_hook)(action, fflags, fdata, domain_state);

  return;
}

void caml_thread_save_runtime_state(void)
{
  Active_thread->current_stack = Caml_state->current_stack;
  Active_thread->c_stack = Caml_state->c_stack;
  Active_thread->gc_regs = Caml_state->gc_regs;
  Active_thread->gc_regs_buckets = Caml_state->gc_regs_buckets;
  Active_thread->exn_handler = Caml_state->exn_handler;
  Active_thread->local_roots = Caml_state->local_roots;
  Active_thread->backtrace_pos = Caml_state->backtrace_pos;
  Active_thread->backtrace_buffer = Caml_state->backtrace_buffer;
  Active_thread->backtrace_last_exn = Caml_state->backtrace_last_exn;
#ifndef NATIVE_CODE
  Active_thread->trap_sp_off = Caml_state->trap_sp_off;
  Active_thread->trap_barrier_off = Caml_state->trap_barrier_off;
  Active_thread->external_raise = Caml_state->external_raise;
#endif
}

void caml_thread_restore_runtime_state(void)
{
  Caml_state->current_stack = Active_thread->current_stack;
  Caml_state->c_stack = Active_thread->c_stack;
  Caml_state->gc_regs = Active_thread->gc_regs;
  Caml_state->gc_regs_buckets = Active_thread->gc_regs_buckets;
  Caml_state->exn_handler = Active_thread->exn_handler;
  Caml_state->local_roots = Active_thread->local_roots;
  Caml_state->backtrace_pos = Active_thread->backtrace_pos;
  Caml_state->backtrace_buffer = Active_thread->backtrace_buffer;
  Caml_state->backtrace_last_exn = Active_thread->backtrace_last_exn;
#ifndef NATIVE_CODE
  Caml_state->trap_sp_off = Active_thread->trap_sp_off;
  Caml_state->trap_barrier_off = Active_thread->trap_barrier_off;
  Caml_state->external_raise = Active_thread->external_raise;
#endif
}

/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */

static void caml_thread_enter_blocking_section(void)
{
  /* Save the current runtime state in the thread descriptor
     of the current thread */
  caml_thread_save_runtime_state();
  /* Tell other threads that the runtime is free */
  thread_lock_release(Caml_state->id);
}

static void caml_thread_leave_blocking_section(void)
{
  caml_thread_t th = st_tls_get(caml_thread_key);
  /* Wait until the runtime is free */
  thread_lock_acquire(th->domain_id);
  /* Update Active_thread to point to the thread descriptor corresponding to
     the thread currently executing */
  Active_thread = th;
  /* Restore the runtime state from the Active_thread descriptor */
  caml_thread_restore_runtime_state();
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
  caml_domain_state *domain_state;
  uintnat stack_wsize = caml_get_init_stack_wsize();

  domain_state = Caml_state;
  th = NULL;
  th = (caml_thread_t)caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;
  th->descr = Val_unit;
  th->next = NULL;
  th->prev = NULL;
  th->domain_id = domain_state->id;
  th->current_stack = caml_alloc_main_stack(stack_wsize);
  if (th->current_stack == NULL) {
    caml_stat_free(th);
    return NULL;
  }
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
  th->trap_barrier_off = 2;
  th->external_raise = NULL;
#endif

  return th;
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

/* Remove a thread info block from the list of threads
   and free its resources. */
static void caml_thread_remove_and_free(caml_thread_t th)
{
  if (th->next == th)
    Active_thread = NULL; /* last OCaml thread exiting */
  else if (Active_thread == th)
    Active_thread = th->next;     /* PR#5295 */
  th->next->prev = th->prev;
  th->prev->next = th->next;

  caml_thread_free_info(th);
  return;
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */
/* TODO(engil): more work on the multicore fork machinery. */

static void caml_thread_reinitialize(void)
{
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
  m->init = 0; /* force initialization */
  st_masterlock_init(m);
}

CAMLprim value caml_thread_join(value th);

/* This hook is run when a domain shuts down (see domains.c).

   When a domain shuts down, the state must be cleared to allow proper reuse of
   the domain slot the next time a domain is started on this slot. If a program
   is single-domain, we mimic OCaml 4's behavior and do not care about ongoing
   thread: the program will exit. */
static void caml_thread_domain_stop_hook(void) {
  /* If the program runs multiple domains, we should not let systhreads to hang
     around when a domain exit. If the domain is not the last one (and the last
     one will always be domain 0) we force the domain to join on every thread
     on its chain before wrapping up. */
  if (!caml_domain_alone()) {

    while (Active_thread->next != Active_thread) {
      caml_thread_join(Active_thread->next->descr);
    }

    /* another domain thread may be joining on this domain's descriptor */
    caml_threadstatus_terminate(Terminated(Active_thread->descr));

    caml_stat_free(Active_thread);
    Active_thread = NULL;
  };
}

CAMLprim value caml_thread_initialize_domain(value v)
{
  CAMLparam0();

  caml_thread_t new_thread;

  /* OS-specific initialization */
  st_initialize();

  st_masterlock_init(Thread_lock(Caml_state->id));

  new_thread =
    (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));

  new_thread->domain_id = Caml_state->id;
  new_thread->descr = caml_thread_new_descriptor(Val_unit);
  new_thread->next = new_thread;
  new_thread->prev = new_thread;
  new_thread->backtrace_last_exn = Val_unit;

  st_tls_set(caml_thread_key, new_thread);

  Active_thread = new_thread;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_thread_yield(value unit);

void caml_thread_interrupt_hook(void)
{
  /* Do not attempt to yield from the backup thread */
  if (caml_bt_is_self()) return;

  uintnat is_on = 1;
  atomic_uintnat* req_external_interrupt =
    &Caml_state->requested_external_interrupt;

  if (atomic_compare_exchange_strong(req_external_interrupt, &is_on, 0)) {
    caml_thread_yield(Val_unit);
  }

  return;
}

/* [caml_thread_initialize] initialises the systhreads infrastructure. This
   function first sets up the chain for systhreads on this domain, then setup
   the global variables and hooks for systhreads to cooperate with the runtime
   system. */
CAMLprim value caml_thread_initialize(value unit)
{
  CAMLparam0();

  if (!caml_domain_alone())
    caml_failwith("caml_thread_initialize: cannot initialize Thread "
                  "while several domains are running.");

  /* Initialize the key to the [caml_thread_t] structure */
  st_tls_newkey(&caml_thread_key);

  /* First initialise the systhread chain on this domain */
  caml_thread_initialize_domain(Val_unit);

  prev_scan_roots_hook = atomic_exchange(&caml_scan_roots_hook,
                                         caml_thread_scan_roots);
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
  caml_domain_external_interrupt_hook = caml_thread_interrupt_hook;
  caml_domain_stop_hook = caml_thread_domain_stop_hook;

  caml_atfork_hook = caml_thread_reinitialize;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_thread_cleanup(value unit)
{
  if (Tick_thread_running){
    atomic_store_rel(&Tick_thread_stop, 1);
    st_thread_join(Tick_thread_id);
    atomic_store_rel(&Tick_thread_stop, 0);
    Tick_thread_running = 0;
  }

  return Val_unit;
}
/* Thread cleanup at termination */

static void caml_thread_stop(void)
{
  /* PR#5188, PR#7220: some of the global runtime state may have
     changed as the thread was running, so we save it in the
     curr_thread data to make sure that the cleanup logic
     below uses accurate information. */
  caml_thread_save_runtime_state();

  /* The main domain thread does not go through [caml_thread_stop]. There is
     always one more thread in the chain at this point in time. */
  CAMLassert(Active_thread->next != Active_thread);

  caml_threadstatus_terminate(Terminated(Active_thread->descr));

  /* The following also sets Active_thread to a sane value in case the
     backup thread does a GC before the domain lock is acquired
     again. */
  caml_thread_remove_and_free(Active_thread);
  caml_thread_restore_runtime_state();

  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (Active_thread == NULL) caml_thread_cleanup(Val_unit);

  thread_lock_release(Caml_state->id);
}

/* Create a thread */

/* the thread lock is not held when entering */
static void * caml_thread_start(void * v)
{
  caml_thread_t th = (caml_thread_t) v;
  int dom_id = th->domain_id;
  value clos;

  caml_init_domain_self(dom_id);

  st_tls_set(caml_thread_key, th);

  thread_lock_acquire(dom_id);
  Active_thread = th;
  caml_thread_restore_runtime_state();

#ifdef POSIX_SIGNALS
  /* restore the signal mask from the spawning thread, now it is safe for the
     signal handler to run (as Caml_state is initialised) */
  pthread_sigmask(SIG_SETMASK, &th->init_mask, NULL);
#endif

  clos = Start_closure(Active_thread->descr);
  caml_modify(&(Start_closure(Active_thread->descr)), Val_unit);
  caml_callback_exn(clos, Val_unit);
  caml_thread_stop();

  return 0;
}

static int create_tick_thread()
{
  int err;
#ifdef POSIX_SIGNALS
  sigset_t mask, old_mask;

  /* Block all signals so that we don't try to execute an OCaml signal
     handler in the new tick thread */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

  err = st_thread_create(&Tick_thread_id, caml_thread_tick,
                         (void *) &Caml_state->id);

#ifdef POSIX_SIGNALS
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  return err;
}

CAMLprim value caml_thread_new(value clos)
{
  CAMLparam1(clos);
  caml_thread_t th;
  st_retcode err;
#ifdef POSIX_SIGNALS
  sigset_t mask, old_mask;

  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support multithreaded programs");
#endif
  /* Create a thread info block */
  th = caml_thread_new_info();

  if (th == NULL)
    caml_raise_out_of_memory();

  th->descr = caml_thread_new_descriptor(clos);

#ifdef POSIX_SIGNALS
  th->init_mask = mask;
#endif

  th->next = Active_thread->next;
  th->prev = Active_thread;

  Active_thread->next->prev = th;
  Active_thread->next = th;

  err = st_thread_create(NULL, caml_thread_start, (void *) th);

#ifdef POSIX_SIGNALS
  /* regardless of error, return our sigmask to the original state */
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_and_free(th);
    sync_check_error(err, "Thread.create");
  }

  if (! Tick_thread_running) {
    err = create_tick_thread();
    sync_check_error(err, "Thread.create");
    Tick_thread_running = 1;
  }
  CAMLreturn(th->descr);
}

/* Register a thread already created from C */

#define Dom_c_threads 0

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_register(void)
{
  /* Already registered? */
  if (st_tls_get(caml_thread_key) != NULL) return 0;

  CAMLassert(Caml_state == NULL);
  caml_init_domain_self(Dom_c_threads);

  /* Take master lock to protect access to the runtime */
  thread_lock_acquire(Dom_c_threads);
  /* Create a thread info block */
  caml_thread_t th = caml_thread_new_info();
  /* If it fails, we release the lock and return an error. */
  if (th == NULL) {
    thread_lock_release(Dom_c_threads);
    return 0;
  }
  /* Add thread info block to the list of threads */
  if (Active_thread == NULL) {
    th->next = th;
    th->prev = th;
    Active_thread = th;
  } else {
    th->next = Active_thread->next;
    th->prev = Active_thread;
    Active_thread->next->prev = th;
    Active_thread->next = th;
  }
  /* Associate the thread descriptor with the thread */
  st_tls_set(caml_thread_key, (void *) th);
  /* Allocate the thread descriptor on the heap */
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */

  if (! Tick_thread_running) {
    st_retcode err = create_tick_thread();
    sync_check_error(err, "caml_register_c_thread");
    Tick_thread_running = 1;
  }

  /* Release the master lock */
  thread_lock_release(Dom_c_threads);
  return 1;
}

/* Unregister a thread that was created from C and registered with
   the function above */

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_unregister(void)
{
  caml_thread_t th = st_tls_get(caml_thread_key);

  /* If this thread is not set, then it was not registered */
  if (th == NULL) return 0;
  /* Wait until the runtime is available */
  thread_lock_acquire(Dom_c_threads);
  /*  Forget the thread descriptor */
  st_tls_set(caml_thread_key, NULL);
  /* Remove thread info block from list of threads, and free it */
  caml_thread_remove_and_free(th);

  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (Active_thread == NULL)
    caml_thread_cleanup(Val_unit);
  else
    caml_thread_restore_runtime_state();

  /* Release the runtime */
  thread_lock_release(Dom_c_threads);
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

CAMLprim value caml_thread_yield(value unit)
{
  st_masterlock *m = Thread_lock(Caml_state->id);
  if (st_masterlock_waiters(m) == 0)
    return Val_unit;

  /* Do all the parts of a blocking section enter/leave except lock
     manipulation, which we'll do more efficiently in st_thread_yield. (Since
     our blocking section doesn't contain anything interesting, don't bother
     with saving errno.)
  */

  caml_raise_if_exception(caml_process_pending_signals_exn());
  caml_thread_save_runtime_state();
  st_thread_yield(m);
  Active_thread = st_tls_get(caml_thread_key);
  caml_thread_restore_runtime_state();
  caml_raise_if_exception(caml_process_pending_signals_exn());

  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)
{
  st_retcode rc = caml_threadstatus_wait(Terminated(th));
  sync_check_error(rc, "Thread.join");
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
  sync_check_error(st_event_create(&ts), "Thread.create");
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
