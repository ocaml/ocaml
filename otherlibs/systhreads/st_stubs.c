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
#include "caml/callback.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/roots.h"
#include "caml/signals.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#else
#include "caml/fiber.h"
#endif
#include "caml/sys.h"
#include "threads.h"

#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
#include "../../asmrun/spacetime.h"
#endif

/* Initial size of bytecode stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in milliseconds */
#define Thread_timeout 50

/* OS-specific code */
#ifdef _WIN32
#include "st_win32.h"
#else
#include "st_posix.h"
#endif

/* The ML value describing a thread (heap-allocated) */

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  value terminated;             /* Triggered event for thread termination */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Start_closure(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Start_closure_index ((offsetof(struct caml_thread_descr, start_closure))/sizeof(value))
#define Terminated(v) (((struct caml_thread_descr *)(v))->terminated)

/* The infos on threads (allocated via malloc()) */

struct caml_thread_struct {
  value descr;                  /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;

  value current_stack;         /* Saved value of caml_current_stack */
  struct caml__roots_block *local_roots; /* Saved value of local_roots */
#ifdef NATIVE_CODE
  char *system_sp;
  char *system_stack_high;
  uintnat system_exnptr_offset;
  struct longjmp_buffer *exit_buf; /* For thread exit */
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  value internal_spacetime_trie_root;
  value internal_spacetime_finaliser_trie_root;
  value* spacetime_trie_node_ptr;
  value* spacetime_finaliser_trie_root;
#endif
#else
  intnat trap_sp_off;
  intnat trap_barrier_off;
  struct caml_exception_context *external_raise; /* Saved external_raise */
#endif
  int backtrace_pos;            /* Saved backtrace_pos */
  code_t * backtrace_buffer;    /* Saved backtrace_buffer */
  caml_root backtrace_last_exn; /* Saved backtrace_last_exn (root) */
};

typedef struct caml_thread_struct * caml_thread_t;

/* The "head" of the circular list of thread descriptors */
static caml_thread_t all_threads = NULL;

/* The descriptor for the currently executing thread */
static caml_thread_t curr_thread = NULL;

/* The master lock protecting the OCaml runtime system */
static st_masterlock caml_master_lock;

/* Whether the "tick" thread is already running */
static int caml_tick_thread_running = 0;

/* The thread identifier of the "tick" thread */
static st_thread_id caml_tick_thread_id;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding system thread. */
static st_tlskey thread_descriptor_key;

/* The key used for unlocking I/O channels on exceptions */
static st_tlskey last_channel_locked_key;

/* Identifier for next thread creation */
static intnat thread_next_ident = 0;

/* Forward declarations */
static value caml_threadstatus_new (void);
static void caml_threadstatus_terminate (value);
static st_retcode caml_threadstatus_wait (value);

/* Imports from the native-code runtime system */
#ifdef NATIVE_CODE
extern struct longjmp_buffer caml_termination_jmpbuf;
extern void (*caml_termination_hook)(void);
#endif

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action, void*, struct domain* domain);

static void caml_thread_scan_roots(scanning_action action, void* fdata, struct domain* domain)
{
  caml_thread_t th;
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

  th = curr_thread;
  do {
    (*action)(fdata, th->descr, &th->descr);
    /* Don't rescan the stack of the current thread, it was done already */
    if (th != curr_thread) {
      (*action)(fdata, th->current_stack, &th->current_stack);

      for (lr = th->local_roots; lr != NULL; lr = lr->next) {
        for (i = 0; i < lr->ntables; i++){
          for (j = 0; j < lr->nitems; j++){
            sp = &(lr->tables[i][j]);
            (*action)(fdata, *sp, sp);
          }
        }
      }

    }
    th = th->next;
  } while (th != curr_thread);
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action, fdata, domain);
}

/* Saving and restoring runtime state in curr_thread */

static inline void caml_thread_save_runtime_state(void)
{
  /* Save the stack-related global variables in the thread descriptor
     of the current thread */
  curr_thread->current_stack = Caml_state->current_stack;
  curr_thread->local_roots = CAML_LOCAL_ROOTS;
#ifdef NATIVE_CODE
  curr_thread->system_sp = Caml_state->system_sp;
  curr_thread->system_stack_high = Caml_state->system_stack_high;
  curr_thread->system_exnptr_offset = Caml_state->system_exnptr_offset;
#ifdef WITH_SPACETIME
  curr_thread->spacetime_trie_node_ptr
    = caml_spacetime_trie_node_ptr;
  curr_thread->spacetime_finaliser_trie_root
    = caml_spacetime_finaliser_trie_root;
#endif
#else
  Stack_sp(Caml_state->current_stack) =
    Caml_state->extern_sp - Caml_state->stack_high;
  curr_thread->trap_sp_off = Caml_state->trap_sp_off;
  curr_thread->trap_barrier_off = Caml_state->trap_barrier_off;
  curr_thread->external_raise = Caml_state->external_raise;
#endif
  curr_thread->backtrace_pos = Caml_state->backtrace_pos;
  curr_thread->backtrace_buffer = Caml_state->backtrace_buffer;
  curr_thread->backtrace_last_exn = Caml_state->backtrace_last_exn;
}

static inline void caml_thread_restore_runtime_state(void)
{
  /* Update curr_thread to point to the thread descriptor corresponding
     to the thread currently executing */
  curr_thread = st_tls_get(thread_descriptor_key);
  /* Restore the stack-related global variables */
  Caml_state->current_stack = curr_thread->current_stack;
  caml_restore_stack();
  CAML_LOCAL_ROOTS = curr_thread->local_roots;
#ifdef NATIVE_CODE
  Caml_state->system_sp = curr_thread->system_sp;
  Caml_state->system_stack_high = curr_thread->system_stack_high;
  Caml_state->system_exnptr_offset = curr_thread->system_exnptr_offset;
#ifdef WITH_SPACETIME
  caml_spacetime_trie_node_ptr
    = curr_thread->spacetime_trie_node_ptr;
  caml_spacetime_finaliser_trie_root
    = curr_thread->spacetime_finaliser_trie_root;
#endif
#else
  Caml_state->trap_sp_off = curr_thread->trap_sp_off;
  Caml_state->trap_barrier_off = curr_thread->trap_barrier_off;
  Caml_state->caml_external_raise = curr_thread->external_raise;
#endif
  Caml_state->backtrace_pos = curr_thread->backtrace_pos;
  Caml_state->backtrace_buffer = curr_thread->backtrace_buffer;
  Caml_state->backtrace_last_exn = curr_thread->backtrace_last_exn;
}

/* Hooks for enter_blocking_section and leave_blocking_section */


static void caml_thread_enter_blocking_section(void)
{
  /* Save the current runtime state in the thread descriptor
     of the current thread */
  caml_thread_save_runtime_state();
  /* Tell other threads that the runtime is free */
  st_masterlock_release(&caml_master_lock);
}

static void caml_thread_leave_blocking_section(void)
{
  /* Wait until the runtime is free */
  st_masterlock_acquire(&caml_master_lock);
  /* Update curr_thread to point to the thread descriptor corresponding
     to the thread currently executing */
  curr_thread = st_tls_get(thread_descriptor_key);
  /* Restore the runtime state from the curr_thread descriptor */
  caml_thread_restore_runtime_state();
}

static int caml_thread_try_leave_blocking_section(void)
{
  /* Disable immediate processing of signals (PR#3659).
     try_leave_blocking_section always fails, forcing the signal to be
     recorded and processed at the next leave_blocking_section or
     polling. */
  return 0;
}

/* Hook for estimating stack usage */

static uintnat (*prev_stack_usage_hook)(void);

static uintnat caml_thread_stack_usage(void)
{
  return 0;
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
#ifndef NATIVE_CODE
  value stack;
#endif

  th = (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;
  th->descr = Val_unit;         /* filled later */
  th->local_roots = NULL;
#ifdef NATIVE_CODE
  th->current_stack = caml_alloc_main_stack(Thread_stack_size);
  th->system_sp = NULL;
  th->system_stack_high = NULL;
  th->system_exnptr_offset = 0;
  th->exit_buf = NULL;
#ifdef WITH_SPACETIME
  /* CR-someday mshinwell: The commented-out changes here are for multicore,
     where we think we should have one trie per domain. */
  th->internal_spacetime_trie_root = Val_unit;
  th->spacetime_trie_node_ptr =
    &caml_spacetime_trie_root; /* &th->internal_spacetime_trie_root; */
  th->internal_spacetime_finaliser_trie_root = Val_unit;
  th->spacetime_finaliser_trie_root
    = caml_spacetime_finaliser_trie_root;
    /* &th->internal_spacetime_finaliser_trie_root; */
  caml_spacetime_register_thread(
    th->spacetime_trie_node_ptr,
    th->spacetime_finaliser_trie_root);
#endif
#else
  stack = caml_alloc_shr(Thread_stack_size, Stack_tag);
  Stack_sp(stack) = 0;
  Stack_dirty_domain(stack) = 0;
  Stack_handle_value(stack) = Val_long(0);
  Stack_handle_exception(stack) = Val_long(0);
  Stack_handle_effect(stack) = Val_long(0);
  Stack_parent(stack) = Val_unit;

  th->current_stack = stack;
  th->trap_sp_off = 1;
  th->trap_barrier_off = 2;
  th->external_raise = NULL;
#endif
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  return th;
}

/* Allocate a thread descriptor block. */

static value caml_thread_new_descriptor(value clos)
{
  value mu = Val_unit;
  value descr;
  Begin_roots2 (clos, mu)
    /* Create and initialize the termination semaphore */
    mu = caml_threadstatus_new();
    /* Create a descriptor for the new thread */
    descr = alloc_small(3, 0);
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = clos;
    Terminated(descr) = mu;
    thread_next_ident++;
  End_roots();
  return descr;
}

/* Remove a thread info block from the list of threads.
   Free it and its stack resources. */

static void caml_thread_remove_info(caml_thread_t th)
{
  if (th->next == th)
    all_threads = NULL; /* last OCaml thread exiting */
  else if (all_threads == th)
    all_threads = th->next;     /* PR#5295 */
  th->next->prev = th->prev;
  th->prev->next = th->next;
  if (th->backtrace_buffer != NULL) free(th->backtrace_buffer);
#ifndef WITH_SPACETIME
  stat_free(th);
  /* CR-soon mshinwell: consider what to do about the Spacetime trace.  Could
     perhaps have a hook to save a snapshot on thread termination.
     For the moment we can't even free [th], since it contains the trie
     roots. */
#endif
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */

static void caml_thread_reinitialize(void)
{
  caml_thread_t thr, next;

  /* Remove all other threads (now nonexistent)
     from the doubly-linked list of threads */
  thr = curr_thread->next;
  while (thr != curr_thread) {
    next = thr->next;
    stat_free(thr);
    thr = next;
  }
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  all_threads = curr_thread;
  /* Reinitialize the master lock machinery,
     just in case the fork happened while other threads were doing
     leave_blocking_section */
  st_masterlock_init(&caml_master_lock);
  /* Tick thread is not currently running in child process, will be
     re-created at next Thread.create */
  caml_tick_thread_running = 0;
}

/* Initialize the thread machinery */

CAMLprim value caml_thread_initialize(value unit)   /* ML */
{
  /* Protect against repeated initialization (PR#1325) */
  if (curr_thread != NULL) return Val_unit;
  /* OS-specific initialization */
  st_initialize();
  /* Initialize and acquire the master lock */
  st_masterlock_init(&caml_master_lock);
  /* Initialize the keys */
  st_tls_newkey(&thread_descriptor_key);
  st_tls_newkey(&last_channel_locked_key);
  /* Set up a thread info block for the current thread */
  curr_thread =
    (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));
  curr_thread->descr = caml_thread_new_descriptor(Val_unit);
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  all_threads = curr_thread;
#ifdef NATIVE_CODE
  curr_thread->exit_buf = &caml_termination_jmpbuf;
#endif
  /* The stack-related fields will be filled in at the next
     enter_blocking_section */
  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) curr_thread);
  /* Set up the hooks */
  prev_scan_roots_hook = caml_scan_roots_hook;
  caml_scan_roots_hook = caml_thread_scan_roots;
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
#ifdef NATIVE_CODE
  caml_termination_hook = st_thread_exit;
#endif
  /* Set up fork() to reinitialize the thread machinery in the child
     (PR#4577) */
  st_atfork(caml_thread_reinitialize);
  return Val_unit;
}

/* Cleanup the thread machinery on program exit or DLL unload. */

CAMLprim value caml_thread_cleanup(value unit)   /* ML */
{
  if (caml_tick_thread_running){
    caml_tick_thread_stop = 1;
    st_thread_join(caml_tick_thread_id);
    caml_tick_thread_stop = 0;
    caml_tick_thread_running = 0;
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
  /* Signal that the thread has terminated */
  caml_threadstatus_terminate(Terminated(curr_thread->descr));
  /* Remove th from the doubly-linked list of threads and free its info block */
  caml_thread_remove_info(curr_thread);
  /* OS-specific cleanups */
  st_thread_cleanup();
  /* Release the runtime system */
  st_masterlock_release(&caml_master_lock);
}

/* Create a thread */

struct st_thread_info {
  caml_thread_t thread_info;
  int domain_id;
};

static ST_THREAD_FUNCTION caml_thread_start(void * arg)
{
  struct st_thread_info* info;
  caml_thread_t th;
  value clos;
#ifdef NATIVE_CODE
  struct longjmp_buffer termination_buf;
  char tos;
#endif

  info = (struct st_thread_info*) arg;
  th = info->thread_info;
  caml_init_domain_self(info->domain_id);
  free(info);
  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) th);
  /* Acquire the global mutex */
  leave_blocking_section();
#ifdef NATIVE_CODE
  /* Record top of stack (approximative) */
  Caml_state->system_stack_high = &tos;
  /* Setup termination handler (for caml_thread_exit) */
  if (sigsetjmp(termination_buf.buf, 0) == 0) {
    th->exit_buf = &termination_buf;
#endif
    /* Callback the closure */
    clos = Start_closure(th->descr);
    caml_modify_field(th->descr, Start_closure_index, Val_unit);
    callback_exn(clos, Val_unit);
    /* Enter blocking section to release domain locks */
    enter_blocking_section();
    caml_thread_stop();
#ifdef NATIVE_CODE
  }
#endif
  /* The thread now stops running */
  return 0;
}

CAMLprim value caml_thread_new(value clos)          /* ML */
{
  CAMLparam1(clos);
  CAMLlocal1(stack);
  caml_thread_t th;
  st_retcode err;
  uintnat domain_id;

  /* Create a thread info block */
  th = caml_thread_new_info();
  if (th == NULL) caml_raise_out_of_memory();
  stack = th->current_stack;
  /* Equip it with a thread descriptor */
  th->descr = caml_thread_new_descriptor(clos);
  /* Add thread info block to the list of threads */
  th->next = curr_thread->next;
  th->prev = curr_thread;
  curr_thread->next->prev = th;
  curr_thread->next = th;
  /* Create the new thread */
  struct st_thread_info *info =
    (struct st_thread_info*)malloc(sizeof(struct st_thread_info));
  info->thread_info = th;
  info->domain_id = Caml_state->id;
  domain_id = info->domain_id;
  err = st_thread_create(NULL, caml_thread_start, (void *) info);
  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_info(th);
    st_check_error(err, "Thread.create");
  }
  /* Create the tick thread if not already done.
     Because of PR#4666, we start the tick thread late, only when we create
     the first additional thread in the current process*/
  if (! caml_tick_thread_running) {
    err = st_thread_create(&caml_tick_thread_id, caml_thread_tick, (void*)domain_id);
    st_check_error(err, "Thread.create");
    caml_tick_thread_running = 1;
  }
  CAMLreturn(th->descr);
}

/* Register a thread already created from C */

CAMLexport int caml_c_thread_register(void)
{
  caml_thread_t th;
  st_retcode err;

  /* Already registered? */
  if (st_tls_get(thread_descriptor_key) != NULL) return 0;
  /* Create a thread info block */
  th = caml_thread_new_info();
  if (th == NULL) return 0;
#ifdef NATIVE_CODE
  th->system_stack_high = (char *) &err;
#endif
  /* Take master lock to protect access to the chaining of threads */
  st_masterlock_acquire(&caml_master_lock);
  /* Add thread info block to the list of threads */
  if (all_threads == NULL) {
    th->next = th;
    th->prev = th;
    all_threads = th;
  } else {
    th->next = all_threads->next;
    th->prev = all_threads;
    all_threads->next->prev = th;
    all_threads->next = th;
  }
  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) th);
  /* Release the master lock */
  st_masterlock_release(&caml_master_lock);
  /* Now we can re-enter the run-time system and heap-allocate the descriptor */
  leave_blocking_section();
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */
  /* Create the tick thread if not already done.  */
  if (! caml_tick_thread_running) {
    err = st_thread_create(&caml_tick_thread_id, caml_thread_tick, NULL);
    if (err == 0) caml_tick_thread_running = 1;
  }
  /* Exit the run-time system */
  enter_blocking_section();
  return 1;
}

/* Unregister a thread that was created from C and registered with
   the function above */

CAMLexport int caml_c_thread_unregister(void)
{
  caml_thread_t th = st_tls_get(thread_descriptor_key);
  /* Not registered? */
  if (th == NULL) return 0;
  /* Wait until the runtime is available */
  st_masterlock_acquire(&caml_master_lock);
  /* Forget the thread descriptor */
  st_tls_set(thread_descriptor_key, NULL);
  /* Remove thread info block from list of threads, and free it */
  caml_thread_remove_info(th);
  /* Release the runtime */
  st_masterlock_release(&caml_master_lock);
  return 1;
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)         /* ML */
{
  if (curr_thread == NULL) invalid_argument("Thread.self: not initialized");
  return curr_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)          /* ML */
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)  /* ML */
{
  char * msg = format_caml_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(curr_thread->descr)), msg);
  free(msg);
  if (Caml_state->backtrace_active) print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Terminate current thread */

CAMLprim value caml_thread_exit(value unit)   /* ML */
{
  struct longjmp_buffer * exit_buf = NULL;

  if (curr_thread == NULL) invalid_argument("Thread.exit: not initialized");

  /* In native code, we cannot call pthread_exit here because on some
     systems this raises a C++ exception, and ocamlopt-generated stack
     frames cannot be unwound.  Instead, we longjmp to the thread
     creation point (in caml_thread_start) or to the point in
     caml_main where caml_termination_hook will be called.
     Note that threads created in C then registered do not have
     a creation point (exit_buf == NULL).
 */
#ifdef NATIVE_CODE
  exit_buf = curr_thread->exit_buf;
#endif
  caml_thread_stop();
  if (exit_buf != NULL) {
    /* Native-code and (main thread or thread created by OCaml) */
    siglongjmp(exit_buf->buf, 1);
  } else {
    /* Bytecode, or thread created from C */
    st_thread_exit();
  }
  return Val_unit;  /* not reached */
}

/* Allow re-scheduling */

CAMLprim value caml_thread_yield(value unit)        /* ML */
{
  if (st_masterlock_waiters(&caml_master_lock) == 0) return Val_unit;
  enter_blocking_section();
  st_thread_yield();
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)          /* ML */
{
  st_retcode rc = caml_threadstatus_wait(Terminated(th));
  st_check_error(rc, "Thread.join");
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (* ((st_mutex *) Data_custom_val(v)))
#define Max_mutex_number 5000

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

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_compare,
  caml_mutex_hash,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_mutex_new(value unit)        /* ML */
{
  st_mutex mut = NULL;          /* suppress warning */
  value wrapper;
  st_check_error(st_mutex_create(&mut), "Mutex.create");
  wrapper = alloc_custom(&caml_mutex_ops, sizeof(st_mutex *),
                         1, Max_mutex_number);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_mutex_lock(value wrapper)     /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;

  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (st_mutex_trylock(mut) == PREVIOUSLY_UNLOCKED) return Val_unit;
  /* If unsuccessful, block on mutex */
  Begin_root(wrapper)           /* prevent the deallocation of mutex */
    enter_blocking_section();
    retcode = st_mutex_lock(mut);
    leave_blocking_section();
  End_roots();
  st_check_error(retcode, "Mutex.lock");
  return Val_unit;
}

CAMLprim value caml_mutex_unlock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;
  /* PR#4351: no need to release and reacquire master lock */
  retcode = st_mutex_unlock(mut);
  st_check_error(retcode, "Mutex.unlock");
  return Val_unit;
}

CAMLprim value caml_mutex_try_lock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;
  retcode = st_mutex_trylock(mut);
  if (retcode == ALREADY_LOCKED) return Val_false;
  st_check_error(retcode, "Mutex.try_lock");
  return Val_true;
}

/* Conditions operations */

#define Condition_val(v) (* (st_condvar *) Data_custom_val(v))
#define Max_condition_number 5000

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
  custom_compare_ext_default
};

CAMLprim value caml_condition_new(value unit)        /* ML */
{
  st_condvar cond = NULL;       /* suppress warning */
  value wrapper;
  st_check_error(st_condvar_create(&cond), "Condition.create");
  wrapper = alloc_custom(&caml_condition_ops, sizeof(st_condvar *),
                         1, Max_condition_number);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_condition_wait(value wcond, value wmut)           /* ML */
{
  st_condvar cond = Condition_val(wcond);
  st_mutex mut = Mutex_val(wmut);
  st_retcode retcode;

  Begin_roots2(wcond, wmut)     /* prevent deallocation of cond and mutex */
    enter_blocking_section();
    retcode = st_condvar_wait(cond, mut);
    leave_blocking_section();
  End_roots();
  st_check_error(retcode, "Condition.wait");
  return Val_unit;
}

CAMLprim value caml_condition_signal(value wrapper)           /* ML */
{
  st_check_error(st_condvar_signal(Condition_val(wrapper)),
                 "Condition.signal");
  return Val_unit;
}

CAMLprim value caml_condition_broadcast(value wrapper)           /* ML */
{
  st_check_error(st_condvar_broadcast(Condition_val(wrapper)),
                 "Condition.broadcast");
  return Val_unit;
}

/* Thread status blocks */

#define Threadstatus_val(v) (* ((st_event *) Data_custom_val(v)))
#define Max_threadstatus_number 500

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
  custom_compare_ext_default
};

static value caml_threadstatus_new (void)
{
  st_event ts = NULL;           /* suppress warning */
  value wrapper;
  st_check_error(st_event_create(&ts), "Thread.create");
  wrapper = alloc_custom(&caml_threadstatus_ops, sizeof(st_event *),
                         1, Max_threadstatus_number);
  Threadstatus_val(wrapper) = ts;
  return wrapper;
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
    enter_blocking_section();
    retcode = st_event_wait(ts);
    leave_blocking_section();
  End_roots();
  return retcode;
}
