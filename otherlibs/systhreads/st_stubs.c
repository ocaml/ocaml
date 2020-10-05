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
#include "caml/domain_state.h"
#include "caml/platform.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/startup.h"
#include "caml/fiber.h"
#include "caml/callback.h"
#include "caml/weak.h"
#include "caml/finalise.h"
#include "caml/domain.h"
#include "caml/printexc.h"
#include "caml/backtrace.h"
#include <pthread.h>
#include <signal.h>
#include <caml/signals.h>

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
  caml_root backtrace_last_exn;
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
  st_masterlock thread_lock;
};

/* thread_table instance, up to Max_domains */
static struct caml_thread_table thread_table[Max_domains];

#define Thread_main_lock thread_table[Caml_state->id].thread_lock
#define Thread_key thread_table[Caml_state->id].thread_key
#define All_threads thread_table[Caml_state->id].all_threads
#define Current_thread thread_table[Caml_state->id].current_thread

/* Identifier for next thread creation */
static atomic_uintnat thread_next_id;

/* Forward declarations */

static value caml_threadstatus_new (value unit);
static void caml_threadstatus_terminate (value wrapper);
static void caml_threadstatus_wait (value wrapper);

/* Imports from the native-code runtime system */
#ifdef NATIVE_CODE
extern struct longjmp_buffer caml_termination_jmpbuf;
extern void (*caml_termination_hook)(void);
#endif

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action, void *, struct domain *);

static void caml_thread_scan_roots(scanning_action action, void *fdata, struct domain *self)
{
  caml_thread_t th;

  th = Current_thread;

  // a GC could be triggered before current_dec is initialized
  if (th != NULL) {
    do {
      (*action)(fdata, th->descr, &th->descr);

      if (th != Current_thread) {
	if (th->current_stack != NULL)
	  caml_do_local_roots(action, fdata, th->local_roots, th->current_stack, 0);
      }
      th = th->next;
    } while (th != Current_thread);

  };

  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action, fdata, self);

  return;
}

void caml_thread_save_runtime_state(void)
{
  Current_thread->current_stack = Caml_state->current_stack;
  Current_thread->c_stack = Caml_state->c_stack;
  Current_thread->gc_regs_buckets = Caml_state->gc_regs_buckets;
  Current_thread->gc_regs_slot = Caml_state->gc_regs_slot;
  Current_thread->exn_handler = Caml_state->exn_handler;
  Current_thread->local_roots = Caml_state->local_roots;
  Current_thread->backtrace_pos = Caml_state->backtrace_pos;
  Current_thread->backtrace_buffer = Caml_state->backtrace_buffer;
  Current_thread->backtrace_last_exn = Caml_state->backtrace_last_exn;
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
  Caml_state->gc_regs_buckets = Current_thread->gc_regs_buckets;
  Caml_state->gc_regs_slot = Current_thread->gc_regs_slot;
  Caml_state->exn_handler = Current_thread->exn_handler;
  Caml_state->local_roots = Current_thread->local_roots;
  Caml_state->backtrace_pos = Current_thread->backtrace_pos;
  Caml_state->backtrace_buffer = Current_thread->backtrace_buffer;
  Caml_state->backtrace_last_exn = Current_thread->backtrace_last_exn;
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
  st_masterlock_acquire(&Thread_main_lock);
  Current_thread = st_tls_get(Thread_key);
  caml_thread_restore_runtime_state();
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
  struct domain *d;

  d = caml_domain_self();
  th = NULL;
  th = (caml_thread_t) caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;

  th->descr = Val_unit;
  th->current_stack = caml_alloc_main_stack(Stack_size / sizeof(value));;
  th->c_stack = NULL;
  th->exn_handler = NULL;
  th->local_roots = NULL;
  th->exit_buf = NULL;
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  th->backtrace_last_exn = caml_create_root(Val_unit);
  th->domain_id = d->state->id;

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
  caml_stat_free(th->current_stack);
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
  // main_lock needs to be initialized and released.
  st_masterlock_init(&Thread_main_lock);
  st_masterlock_release(&Thread_main_lock);
}

CAMLprim value caml_thread_initialize(value unit);

/* To initialize the thread machinery on a newly created domain. */
/* FIXME(engil): it may be better to just initialize the machinery for every */
/* domain at startup */

static void caml_thread_domain_start_hook(void) {
  caml_thread_initialize(Val_unit);
}

CAMLprim value caml_thread_initialize(value unit)   /* ML */
{
  CAMLparam0();

  caml_thread_t new_thread;

  st_masterlock_init(&Thread_main_lock);

  new_thread =
    (caml_thread_t) caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));

  new_thread->descr = caml_thread_new_descriptor(Val_unit);
  new_thread->next = new_thread;
  new_thread->prev = new_thread;

  #ifdef NATIVE_CODE
  new_thread->exit_buf = &caml_termination_jmpbuf;
  #endif

  // Hooks setup, if caml_scan_roots_hook is set, it was done already
  // by another domain.
  if (caml_scan_roots_hook != caml_thread_scan_roots) {
    prev_scan_roots_hook = caml_scan_roots_hook;
    caml_scan_roots_hook = caml_thread_scan_roots;
    caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
    caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
    caml_domain_start_hook = caml_thread_domain_start_hook;
  };

  st_tls_newkey(&Thread_key);
  st_tls_set(Thread_key, (void *) new_thread);

  All_threads = new_thread;
  Current_thread = new_thread;

  st_atfork(caml_thread_reinitialize);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_thread_cleanup(value unit)   /* ML */
{
  return Val_unit;
}
/* Thread cleanup at termination */

static void caml_thread_stop(void)
{
  caml_threadstatus_terminate(Terminated(Current_thread->descr));
  caml_thread_remove_info(Current_thread);
  // FIXME: tricky bit with backup thread
  // Normally we expect the next thread to kick in and resume operation
  // by first setting Current_thread to the right TLS dec data.
  // However it may very well be that there's no runnable dec next
  // (eg: next dec is blocking.), so we set it to next for now to give a
  // valid state to the backup thread.
  Current_thread = Current_thread->next;
  caml_thread_restore_runtime_state();
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

  th = caml_thread_new_info();
  th->descr = caml_thread_new_descriptor(clos);

  th->next = Current_thread->next;
  th->prev = Current_thread;

  Current_thread->next->prev = th;
  Current_thread->next = th;

  st_thread_create(NULL, caml_thread_start, (void *) th);
  // FIXME: check return from create

  CAMLreturn(th->descr);
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)         /* ML */
{
  return Current_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)          /* ML */
{
  return Val_long(Ident(th));
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

  caml_thread_save_runtime_state();
  st_thread_yield(&Thread_main_lock);
  Current_thread = st_tls_get(Thread_key);
  caml_thread_restore_runtime_state();

  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)          /* ML */
{
  caml_threadstatus_wait(Terminated(th));
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (* ((st_mutex *) Data_custom_val(v)))

static void caml_mutex_finalize(value wrapper)
{
  st_mutex_destroy(Mutex_val(wrapper));
}

static int caml_mutex_compare(value wrapper1, value wrapper2)
{
  st_mutex mut1 = Mutex_val(wrapper1);
  st_mutex mut2 = Mutex_val(wrapper2);
  return mut1 == mut2 ? 0 : mut1 < mut2 ? -1 : 1;
}

static intnat caml_mutex_hash(value wrapper)
{
  return (intnat) (Mutex_val(wrapper));
}

static const struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_compare,
  caml_mutex_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_mutex_new(value unit)        /* ML */
{
  st_mutex mut = NULL;
  value wrapper;

  st_mutex_create(&mut);
  wrapper = caml_alloc_custom(&caml_mutex_ops, sizeof(caml_plat_mutex *),
                              0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_mutex_lock(value wrapper)     /* ML */
{
  st_mutex mut = Mutex_val(wrapper);

  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (caml_plat_try_lock(mut)) return Val_unit;
  /* If unsuccessful, block on mutex */
  Begin_root(wrapper)
    caml_enter_blocking_section();
    st_mutex_lock(mut);
    caml_leave_blocking_section();
  End_roots();
  return Val_unit;
}

CAMLprim value caml_mutex_unlock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  /* PR#4351: no need to release and reacquire master lock */
  st_mutex_unlock(mut);
  return Val_unit;
}

CAMLprim value caml_mutex_try_lock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  int retcode = st_mutex_trylock(mut);
  if (retcode == 1) return Val_false;
  return Val_true;
}

/* Conditions operations */

// TODO: refactor the condition part in a way that makes the Condition module
// and platform code cohabit.
// st_cond_broadcast and st_cond_signal are borrowed from platform
// but do not assert on the mutex.
// The main issue is that platform conditions do rely on explicitly binding a
// mutex to the cond, which is not how Condition work.

#define Condition_val(v) (* (st_condvar *) Data_custom_val(v))

static void caml_condition_finalize(value wrapper)
{
  st_condvar_destroy(Condition_val(wrapper));
}

static int caml_condition_compare(value wrapper1, value wrapper2)
{
  st_condvar cond1 = Condition_val(wrapper1);
  st_condvar cond2 = Condition_val(wrapper2);
  return cond1 == cond2 ? 0 : cond1 < cond2 ? -1 : 1;
}

static intnat caml_condition_hash(value wrapper)
{
  return (intnat) (Condition_val(wrapper));
}

static struct custom_operations caml_condition_ops = {
  "_condition",
  caml_condition_finalize,
  caml_condition_compare,
  caml_condition_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_condition_new(value unit)        /* ML */
{
  value wrapper;
  st_condvar cond;

  st_condvar_create(&cond);
  wrapper = caml_alloc_custom(&caml_condition_ops, sizeof(st_condvar *),
                              0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_condition_wait(value wcond, value wmut)           /* ML */
{
  st_condvar cond = Condition_val(wcond);
  st_mutex mut = Mutex_val(wmut);

  Begin_roots2(wcond, wmut)
    caml_enter_blocking_section();
    st_condvar_wait(cond, mut);
    caml_leave_blocking_section();
  End_roots();

  return Val_unit;
}

CAMLprim value caml_condition_signal(value wrapper)           /* ML */
{
  st_condvar_signal(Condition_val(wrapper));
  return Val_unit;
}

CAMLprim value caml_condition_broadcast(value wrapper)           /* ML */
{
  st_condvar_broadcast(Condition_val(wrapper));
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
  wrapper = caml_alloc_custom(&caml_threadstatus_ops, sizeof(st_event *),
                              0, 1);
  Threadstatus_val(wrapper) = ts;

  CAMLreturn (wrapper);
}

static void caml_threadstatus_terminate (value wrapper)
{
  st_event_trigger(Threadstatus_val(wrapper));
}

static void caml_threadstatus_wait (value wrapper)
{
  st_event ts = Threadstatus_val(wrapper);

  Begin_roots1(wrapper)         /* prevent deallocation of ts */
    caml_enter_blocking_section();
    st_event_wait(ts);
    caml_leave_blocking_section();
  End_roots();

  return;
}
