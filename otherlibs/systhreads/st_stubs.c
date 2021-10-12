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

#include <pthread.h>
#include <signal.h>
#include <stdio.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/domain_state.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/io.h"
#include "caml/platform.h"
#include "caml/printexc.h"
#include "caml/memory.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/weak.h"

#include "caml/sync.h"
#include "st_posix.h"

/* ML value for a thread descriptor */

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  value terminated;             /* Triggered event for thread termination */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Clos(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Terminated(v) (((struct caml_thread_descr *)(v))->terminated)

/* structure holding runtime state */
struct caml_thread_struct {

  value descr;

  struct caml_thread_struct * next;
  struct caml_thread_struct * prev;

  int domain_id;

  struct stack_info* current_stack;
  struct c_stack_link* c_stack;
  struct caml__roots_block *local_roots;
  struct longjmp_buffer *exit_buf;
  int backtrace_pos;
  code_t * backtrace_buffer;
  value backtrace_last_exn;
  value * gc_regs;
  value * gc_regs_buckets;
  value ** gc_regs_slot;
  void * exn_handler;

  #ifndef NATIVE_CODE
  intnat trap_sp_off;
  intnat trap_barrier_off;
  struct caml_exception_context* external_raise;
  #endif

};

typedef struct caml_thread_struct* caml_thread_t;

/* overall table for threads accross domains */
struct caml_thread_table {
  caml_thread_t all_threads;
  caml_thread_t current_thread;
  st_tlskey thread_key;
  st_tlskey last_locked_key;
  st_masterlock thread_lock;
  int tick_thread_running;
  st_thread_id tick_thread_id;
};

/* thread_table instance, up to Max_domains */
static struct caml_thread_table thread_table[Max_domains];

#define Thread_main_lock thread_table[Caml_state->id].thread_lock
#define Thread_key thread_table[Caml_state->id].thread_key
#define Last_channel_locked_key thread_table[Caml_state->id].last_locked_key
#define All_threads thread_table[Caml_state->id].all_threads
#define Current_thread thread_table[Caml_state->id].current_thread
#define Tick_thread_running thread_table[Caml_state->id].tick_thread_running
#define Tick_thread_id thread_table[Caml_state->id].tick_thread_id

/* Identifier for next thread creation */
static atomic_uintnat thread_next_id = 0;

/* Forward declarations */

static value caml_threadstatus_new (value unit);
static void caml_threadstatus_terminate (value wrapper);
static st_retcode caml_threadstatus_wait (value wrapper);

/* Imports from the native-code runtime system */
#ifdef NATIVE_CODE
extern struct longjmp_buffer caml_termination_jmpbuf;
extern void (*caml_termination_hook)(void);
#endif

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action, void *,
                                     caml_domain_state *);

static void caml_thread_scan_roots(scanning_action action,
                                   void *fdata,
                                   caml_domain_state *domain_state)
{
  caml_thread_t th;

  th = Current_thread;

  // a GC could be triggered before current_dec is initialized
  if (th != NULL) {
    do {
      (*action)(fdata, th->descr, &th->descr);

      if (th != Current_thread) {
        if (th->current_stack != NULL)
          caml_do_local_roots(action, fdata, th->local_roots,
                              th->current_stack, th->gc_regs);
      }
      th = th->next;
    } while (th != Current_thread);

  };

  if (prev_scan_roots_hook != NULL)
    (*prev_scan_roots_hook)(action, fdata, domain_state);

  return;
}

void caml_thread_save_runtime_state(void)
{
  Current_thread->current_stack = Caml_state->current_stack;
  Current_thread->c_stack = Caml_state->c_stack;
  Current_thread->gc_regs = Caml_state->gc_regs;
  Current_thread->gc_regs_buckets = Caml_state->gc_regs_buckets;
  Current_thread->gc_regs_slot = Caml_state->gc_regs_slot;
  Current_thread->exn_handler = Caml_state->exn_handler;
  Current_thread->local_roots = Caml_state->local_roots;
  Current_thread->backtrace_pos = Caml_state->backtrace_pos;
  Current_thread->backtrace_buffer = Caml_state->backtrace_buffer;
  caml_modify_generational_global_root(
    &Current_thread->backtrace_last_exn, Caml_state->backtrace_last_exn);
  #ifndef NATIVE_CODE
  Current_thread->trap_sp_off = Caml_state->trap_sp_off;
  Current_thread->trap_barrier_off = Caml_state->trap_barrier_off;
  Current_thread->external_raise = Caml_state->external_raise;
  #endif
}

void caml_thread_restore_runtime_state(void)
{
  Caml_state->current_stack = Current_thread->current_stack;
  Caml_state->c_stack = Current_thread->c_stack;
  Caml_state->gc_regs = Current_thread->gc_regs;
  Caml_state->gc_regs_buckets = Current_thread->gc_regs_buckets;
  Caml_state->gc_regs_slot = Current_thread->gc_regs_slot;
  Caml_state->exn_handler = Current_thread->exn_handler;
  Caml_state->local_roots = Current_thread->local_roots;
  Caml_state->backtrace_pos = Current_thread->backtrace_pos;
  Caml_state->backtrace_buffer = Current_thread->backtrace_buffer;
  caml_modify_generational_global_root(
    &Caml_state->backtrace_last_exn, Current_thread->backtrace_last_exn);
  #ifndef NATIVE_CODE
  Caml_state->trap_sp_off = Current_thread->trap_sp_off;
  Caml_state->trap_barrier_off = Current_thread->trap_barrier_off;
  Caml_state->external_raise = Current_thread->external_raise;
  #endif
}

/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */

static void caml_thread_enter_blocking_section(void)
{
  Current_thread = st_tls_get(Thread_key);
  caml_thread_save_runtime_state();
  st_tls_set(Thread_key, Current_thread);
  st_masterlock_release(&Thread_main_lock);
}

static void caml_thread_leave_blocking_section(void)
{
#ifdef _WIN32
  /* TlsGetValue calls SetLastError which will mask any error which occurred
     prior to the caml_thread_leave_blocking_section call. EnterCriticalSection
     does not do this. */
  DWORD error = GetLastError();
#endif
  st_masterlock_acquire(&Thread_main_lock);
  Current_thread = st_tls_get(Thread_key);
  caml_thread_restore_runtime_state();
#ifdef _WIN32
  SetLastError(error);
#endif
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
  caml_domain_state *domain_state;

  domain_state = Caml_state;
  th = NULL;
  th = (caml_thread_t)caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;

  th->descr = Val_unit;
  th->next = NULL;
  th->prev = NULL;
  th->domain_id = domain_state->id;
  th->current_stack = caml_alloc_main_stack(Stack_size / sizeof(value));;
  th->c_stack = NULL;
  th->local_roots = NULL;
  th->exit_buf = NULL;
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  th->backtrace_last_exn = Val_unit;
  th->gc_regs = NULL;
  th->gc_regs_buckets = NULL;
  th->gc_regs_slot = NULL;
  th->exn_handler = NULL;
  caml_register_generational_global_root(&th->backtrace_last_exn);

  #ifndef NATIVE_CODE
  th->trap_sp_off = 1;
  th->trap_barrier_off = 2;
  th->external_raise = NULL;
  #endif

  return th;
}

/* Allocate a thread descriptor block. */

static value caml_thread_new_descriptor(value clos)
{
  value mu = Val_unit;
  value descr;
  Begin_roots2 (clos, mu)
    mu = caml_threadstatus_new(Val_unit);
    descr = caml_alloc_small(3, 0);
    Ident(descr) = Val_long(atomic_load_acq(&thread_next_id));
    Clos(descr) = clos;
    Terminated(descr) = mu;
    atomic_fetch_add(&thread_next_id, +1);
  End_roots();
  return descr;
}

/* Remove a thread info block from the list of threads.
   Free it and its stack resources. */

static void caml_thread_remove_info(caml_thread_t th)
{
  if (th->next == th)
    All_threads = NULL;
  else if (All_threads == th)
    All_threads = th->next;

  th->next->prev = th->prev;
  th->prev->next = th->next;
  caml_free_stack(th->current_stack);
  caml_remove_generational_global_root(&th->backtrace_last_exn);
  caml_stat_free(th);
  return;
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */
/* TODO(engil): more work on the multicore fork machinery. */

static void caml_thread_reinitialize(void)
{
  caml_thread_t th, next;

  th = Current_thread->next;
  while (th != Current_thread) {
    next = th->next;
    caml_free_stack(th->current_stack);
    caml_remove_generational_global_root(&th->backtrace_last_exn);
    caml_stat_free(th);
    th = next;
  }
  Current_thread->next = Current_thread;
  Current_thread->prev = Current_thread;
  All_threads = Current_thread;

  // within the child, the domain_lock needs to be reset
  // and acquired.
  caml_reset_domain_lock();
  caml_acquire_domain_lock();
  // master_lock needs to be initialized again.
  // this process will also be the effective owner of the lock.
  // so there is no need to run st_masterlock_acquire (busy = 1)
  st_masterlock_init(&Thread_main_lock);
}

CAMLprim value caml_thread_initialize(value unit);
static void caml_thread_initialize_domain();

/* To initialize the thread machinery on a newly created domain. */
/* FIXME(engil): it may be better to just initialize the machinery for every */
/* domain at startup */
static void caml_thread_domain_start_hook(void) {
  caml_thread_initialize_domain();
}

static void caml_thread_domain_stop_hook(void) {
  // This hook will clean up the initial domain's thread descriptor.
  // The assumption is that it is the only remaining link in the threading chain
  // (as every other threads either go through caml_thread_stop or
  // caml_c_thread_unregister.)
  caml_stat_free(Current_thread);
  Current_thread = NULL;
}

static void caml_thread_initialize_domain()
{
  caml_thread_t new_thread;

  /* OS-specific initialization */
  st_initialize();

  st_masterlock_init(&Thread_main_lock);

  new_thread =
    (caml_thread_t) caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));

  new_thread->descr = caml_thread_new_descriptor(Val_unit);
  new_thread->next = new_thread;
  new_thread->prev = new_thread;

  #ifdef NATIVE_CODE
  new_thread->exit_buf = &caml_termination_jmpbuf;
  #endif

  st_tls_newkey(&Thread_key);
  st_tls_newkey(&Last_channel_locked_key);
  st_tls_set(Thread_key, (void *) new_thread);
  st_thread_set_id(Ident(new_thread->descr));

  All_threads = new_thread;
  Current_thread = new_thread;
  Tick_thread_running = 0;

  return;
}

CAMLprim value caml_thread_yield(value unit);

void caml_thread_interrupt_hook(void)
{
  caml_domain_state *domain = Caml_state;
  atomic_uintnat* req_external_interrupt =
    (atomic_uintnat*)&domain->requested_external_interrupt;

  if (atomic_load_acq(req_external_interrupt) == 1) {
    atomic_store_rel(req_external_interrupt, 0);
    caml_thread_yield(Val_unit);
  }

  return;
}

/* Hooks for I/O locking */

static void caml_io_mutex_free(struct channel *chan)
{
  caml_plat_mutex_free(&chan->mutex);
}

static void caml_io_mutex_lock(struct channel *chan)
{
  if( caml_plat_try_lock(&chan->mutex) ) {
    st_tls_set(Last_channel_locked_key, (void *) chan);
    return;
  }

  /* If unsuccessful, block on mutex */
  caml_enter_blocking_section();
  caml_plat_lock(&chan->mutex);
  st_tls_set(Last_channel_locked_key, (void *) chan);
  caml_leave_blocking_section();
}

static void caml_io_mutex_unlock(struct channel *chan)
{
  caml_plat_unlock(&chan->mutex);
  st_tls_set(Last_channel_locked_key, NULL);
}

static void caml_io_mutex_unlock_exn(void)
{
  struct channel * chan = st_tls_get(Last_channel_locked_key);
  if (chan != NULL) caml_io_mutex_unlock(chan);
}

// This setup function is called as an entrypoint to the Thread module.
// This will setup the global variables and hooks for systhreads
// cooperate with the runtime system, after initializing
// the thread chaining.
CAMLprim value caml_thread_initialize(value unit)   /* ML */
{
  CAMLparam0();

  // We first initialize the thread chaining.
  caml_thread_initialize_domain();

  prev_scan_roots_hook = caml_scan_roots_hook;
  caml_scan_roots_hook = caml_thread_scan_roots;
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
  caml_domain_external_interrupt_hook = caml_thread_interrupt_hook;
  caml_domain_start_hook = caml_thread_domain_start_hook;
  caml_domain_stop_hook = caml_thread_domain_stop_hook;
  caml_channel_mutex_free = caml_io_mutex_free;
  caml_channel_mutex_lock = caml_io_mutex_lock;
  caml_channel_mutex_unlock = caml_io_mutex_unlock;
  caml_channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;

  st_atfork(caml_thread_reinitialize);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_thread_cleanup(value unit)   /* ML */
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
  caml_thread_t next = Current_thread->next;

  // The main domain thread does not go through caml_thread_stop.
  // There is always one more thread in the chaining at this point in time.
  Assert(next != Current_thread);

  caml_threadstatus_terminate(Terminated(Current_thread->descr));
  caml_thread_remove_info(Current_thread);
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  // TODO: when we have the tick thread, caution with #9971
  // if (all_threads == NULL) caml_thread_cleanup(Val_unit);

  // FIXME: tricky bit with backup thread
  // Normally we expect the next thread to kick in and resume operation
  // by first setting Current_thread to the right TLS dec data.
  // However it may very well be that there's no runnable dec next
  // (eg: next dec is blocking.), so we set it to next for now to give a
  // valid state to the backup thread.
  Current_thread = next;

  caml_thread_restore_runtime_state();

  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (All_threads == NULL) caml_thread_cleanup(Val_unit);

  st_masterlock_release(&Thread_main_lock);
}

/* Create a thread */

static void * caml_thread_start(void * v)
{
  caml_thread_t th = (caml_thread_t) v;
  value clos;
#ifdef NATIVE_CODE
  struct longjmp_buffer termination_buf;
#endif

  caml_init_domain_self(th->domain_id);

  st_tls_set(Thread_key, th);
  st_thread_set_id(Ident(th->descr));

  st_masterlock_acquire(&Thread_main_lock);
  Current_thread = st_tls_get(Thread_key);
  caml_thread_restore_runtime_state();

#ifdef NATIVE_CODE
  /* Setup termination handler (for caml_thread_exit) */
  if (sigsetjmp(termination_buf.buf, 0) == 0) {
    Current_thread->exit_buf = &termination_buf;
#endif
  clos = Clos(Current_thread->descr);
  caml_modify(&(Clos(Current_thread->descr)), Val_unit);
  caml_callback_exn(clos, Val_unit);
  caml_thread_stop();
#ifdef NATIVE_CODE
  }
#endif
  return 0;
}

CAMLprim value caml_thread_new(value clos)          /* ML */
{
  CAMLparam1(clos);
  caml_thread_t th;
  st_retcode err;

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support multithreaded programs");
#endif
  /* Create a thread info block */
  th = caml_thread_new_info();
  th->descr = caml_thread_new_descriptor(clos);

  th->next = Current_thread->next;
  th->prev = Current_thread;

  Current_thread->next->prev = th;
  Current_thread->next = th;

  err = st_thread_create(NULL, caml_thread_start, (void *) th);
  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_info(th);
    sync_check_error(err, "Thread.create");
  }

  if (! Tick_thread_running) {
    err = st_thread_create(&Tick_thread_id,
                           caml_thread_tick,
                           (void *) &Caml_state->id);
    sync_check_error(err, "Thread.create");
    Tick_thread_running = 1;
  }
  CAMLreturn(th->descr);
}

/* Register a thread already created from C */

CAMLexport int caml_c_thread_register(void)
{
  caml_thread_t th;
  st_retcode err;

  /* Already registered? */
  if (Caml_state == NULL) {
    caml_init_domain_self(0);
  };
  if (st_tls_get(Thread_key) != NULL) return 0;
  /* Take master lock to protect access to the runtime */
  st_masterlock_acquire(&Thread_main_lock);
  /* Create a thread info block */
  th = caml_thread_new_info();
  /* If it fails, we release the lock and return an error. */
  if (th == NULL) {
    st_masterlock_release(&Thread_main_lock);
    return 0;
  }
  /* Add thread info block to the list of threads */
  if (All_threads == NULL) {
    th->next = th;
    th->prev = th;
    All_threads = th;
  } else {
    th->next = All_threads->next;
    th->prev = All_threads;
    All_threads->next->prev = th;
    All_threads->next = th;
  }
  /* Associate the thread descriptor with the thread */
  st_tls_set(Thread_key, (void *) th);
  /* Allocate the thread descriptor on the heap */
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */
  st_thread_set_id(Ident(th->descr));

  if (! Tick_thread_running) {
    err = st_thread_create(&Tick_thread_id,
                           caml_thread_tick,
                           (void *) &Caml_state->id);
    sync_check_error(err, "caml_register_c_thread");
    Tick_thread_running = 1;
  }

  /* Release the master lock */
  st_masterlock_release(&Thread_main_lock);
  return 1;
}

/* Unregister a thread that was created from C and registered with
   the function above */

CAMLexport int caml_c_thread_unregister(void)
{
  caml_thread_t th;

  /* If Caml_state is not set, this thread was likely not registered */
  if (Caml_state == NULL) return 0;

  th = st_tls_get(Thread_key);
  /* Not registered? */
  if (th == NULL) return 0;
  /* Wait until the runtime is available */
  st_masterlock_acquire(&Thread_main_lock);
  /*  Forget the thread descriptor */
  st_tls_set(Thread_key, NULL);
  /* Remove thread info block from list of threads, and free it */
  caml_thread_remove_info(th);
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  // TODO: when we have the tick thread, careful with 9971
  //if (all_threads == NULL) caml_thread_cleanup(Val_unit);
  Current_thread = All_threads;
  caml_thread_restore_runtime_state();

  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (All_threads == NULL) caml_thread_cleanup(Val_unit);

  /* Release the runtime */
  st_masterlock_release(&Thread_main_lock);
  return 1;
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)         /* ML */
{
  return Current_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)          /* ML */
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)  /* ML */
{
  char * msg = caml_format_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(Current_thread->descr)), msg);
  caml_stat_free(msg);
  if (Caml_state->backtrace_active) caml_print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Terminate current thread */

CAMLprim value caml_thread_exit(value unit)   /* ML */
{
  struct longjmp_buffer * exit_buf = NULL;

  if (Current_thread == NULL)
    caml_invalid_argument("Thread.exit: not initialized");

  #ifdef NATIVE_CODE
    exit_buf = Current_thread->exit_buf;
  #endif

    caml_thread_stop();

  if (exit_buf != NULL) {
    /* Native-code and (main thread or thread created by OCaml) */
    siglongjmp(exit_buf->buf, 1);
  } else {
    st_thread_exit();
  };

  return Val_unit;
}

/* Allow re-scheduling */

CAMLprim value caml_thread_yield(value unit)        /* ML */
{
  if (atomic_load_acq(&Thread_main_lock.waiters) == 0) return Val_unit;

  caml_raise_if_exception(caml_process_pending_signals_exn());
  caml_thread_save_runtime_state();
  st_thread_yield(&Thread_main_lock);
  Current_thread = st_tls_get(Thread_key);
  caml_thread_restore_runtime_state();
  caml_raise_if_exception(caml_process_pending_signals_exn());

  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)          /* ML */
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

static value caml_threadstatus_new (value unit)
{
  CAMLparam0();
  CAMLlocal1(wrapper);


  st_event ts = NULL;           /* suppress warning */
  st_event_create(&ts);
  wrapper = caml_alloc_custom(&caml_threadstatus_ops,
                              sizeof(st_event *),
                              0, 1);
  Threadstatus_val(wrapper) = ts;

  CAMLreturn (wrapper);
}

static void caml_threadstatus_terminate (value wrapper)
{
  st_event_trigger(Threadstatus_val(wrapper));
}

static st_retcode caml_threadstatus_wait (value wrapper)
{
  st_event ts = Threadstatus_val(wrapper);
  st_retcode retcode;

  Begin_roots1(wrapper)         /* prevent deallocation of ts */
    caml_enter_blocking_section();
    retcode = st_event_wait(ts);
    caml_leave_blocking_section();
  End_roots();

  return retcode;
}
