/***********************************************************************/
/*                                                                     */
/*                             Objective Caml                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for POSIX 1003.1c threads */

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#ifdef __sun
#define _POSIX_PTHREAD_SEMANTICS
#endif
#include <signal.h>
#include <sys/time.h>
#ifdef __linux__
#include <unistd.h>
#endif
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "custom.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "printexc.h"
#include "roots.h"
#include "signals.h"
#ifdef NATIVE_CODE
#include "stack.h"
#else
#include "stacks.h"
#endif
#include "sys.h"

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in microseconds (50ms) */
#define Thread_timeout 50000

/* The ML value describing a thread (heap-allocated) */

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  value terminated;             /* Mutex held while the thread is running */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Start_closure(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Terminated(v) (((struct caml_thread_descr *)(v))->terminated)

/* The infos on threads (allocated via malloc()) */

struct caml_thread_struct {
  pthread_t pthread;            /* The Posix thread id */
  value descr;                  /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
#ifdef NATIVE_CODE
  char * bottom_of_stack;       /* Saved value of caml_bottom_of_stack */
  uintnat last_retaddr;         /* Saved value of caml_last_return_address */
  value * gc_regs;              /* Saved value of caml_gc_regs */
  char * exception_pointer;     /* Saved value of caml_exception_pointer */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
  struct longjmp_buffer * exit_buf; /* For thread exit */
#else
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  value * trapsp;               /* Saved value of trapsp for this thread */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
  struct longjmp_buffer * external_raise; /* Saved external_raise */
#endif
  int backtrace_pos;            /* Saved backtrace_pos */
  code_t * backtrace_buffer;    /* Saved backtrace_buffer */
  value backtrace_last_exn;     /* Saved backtrace_last_exn (root) */
};

typedef struct caml_thread_struct * caml_thread_t;

/* The descriptor for the currently executing thread */
static caml_thread_t curr_thread = NULL;

/* Track whether one thread is running Caml code.  There can be
   at most one such thread at any time. */
static volatile int caml_runtime_busy = 1;

/* Number of threads waiting to run Caml code. */
static volatile int caml_runtime_waiters = 0;

/* Mutex that protects the two variables above. */
static pthread_mutex_t caml_runtime_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Condition signaled when caml_runtime_busy becomes 0 */
static pthread_cond_t caml_runtime_is_free = PTHREAD_COND_INITIALIZER;

/* Whether the ``tick'' thread is already running */
static int caml_tick_thread_running = 0;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding Posix thread. */
static pthread_key_t thread_descriptor_key;

/* The key used for unlocking I/O channels on exceptions */
static pthread_key_t last_channel_locked_key;

/* Identifier for next thread creation */
static intnat thread_next_ident = 0;

/* Forward declarations */
value caml_threadstatus_new (void);
void caml_threadstatus_terminate (value);
int caml_threadstatus_wait (value);
static void caml_pthread_check (int, char *);

/* Imports for the native-code compiler */
extern struct longjmp_buffer caml_termination_jmpbuf;
extern void (*caml_termination_hook)(void *);

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action);

static void caml_thread_scan_roots(scanning_action action)
{
  caml_thread_t th;

  th = curr_thread;
  do {
    (*action)(th->descr, &th->descr);
    (*action)(th->backtrace_last_exn, &th->backtrace_last_exn);
    /* Don't rescan the stack of the current thread, it was done already */
    if (th != curr_thread) {
#ifdef NATIVE_CODE
      if (th->bottom_of_stack != NULL)
        do_local_roots(action, th->bottom_of_stack, th->last_retaddr,
                       th->gc_regs, th->local_roots);
#else
      do_local_roots(action, th->sp, th->stack_high, th->local_roots);
#endif
    }
    th = th->next;
  } while (th != curr_thread);
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Hooks for enter_blocking_section and leave_blocking_section */

static void caml_thread_enter_blocking_section(void)
{
  /* Save the stack-related global variables in the thread descriptor
     of the current thread */
#ifdef NATIVE_CODE
  curr_thread->bottom_of_stack = caml_bottom_of_stack;
  curr_thread->last_retaddr = caml_last_return_address;
  curr_thread->gc_regs = caml_gc_regs;
  curr_thread->exception_pointer = caml_exception_pointer;
  curr_thread->local_roots = local_roots;
#else
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->local_roots = local_roots;
  curr_thread->external_raise = external_raise;
#endif
  curr_thread->backtrace_pos = backtrace_pos;
  curr_thread->backtrace_buffer = backtrace_buffer;
  curr_thread->backtrace_last_exn = backtrace_last_exn;
  /* Tell other threads that the runtime is free */
  pthread_mutex_lock(&caml_runtime_mutex);
  caml_runtime_busy = 0;
  pthread_mutex_unlock(&caml_runtime_mutex);
  pthread_cond_signal(&caml_runtime_is_free);
}

static void caml_thread_leave_blocking_section(void)
{
  /* Wait until the runtime is free */
  pthread_mutex_lock(&caml_runtime_mutex);
  while (caml_runtime_busy) {
    caml_runtime_waiters++;
    pthread_cond_wait(&caml_runtime_is_free, &caml_runtime_mutex);
    caml_runtime_waiters--;
  }
  caml_runtime_busy = 1;
  pthread_mutex_unlock(&caml_runtime_mutex);
  /* Update curr_thread to point to the thread descriptor corresponding
     to the thread currently executing */
  curr_thread = pthread_getspecific(thread_descriptor_key);
  /* Restore the stack-related global variables */
#ifdef NATIVE_CODE
  caml_bottom_of_stack= curr_thread->bottom_of_stack;
  caml_last_return_address = curr_thread->last_retaddr;
  caml_gc_regs = curr_thread->gc_regs;
  caml_exception_pointer = curr_thread->exception_pointer;
  local_roots = curr_thread->local_roots;
#else
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
  local_roots = curr_thread->local_roots;
  external_raise = curr_thread->external_raise;
#endif
  backtrace_pos = curr_thread->backtrace_pos;
  backtrace_buffer = curr_thread->backtrace_buffer;
  backtrace_last_exn = curr_thread->backtrace_last_exn;
}

static int caml_thread_try_leave_blocking_section(void)
{
  /* Disable immediate processing of signals (PR#3659).
     try_leave_blocking_section always fails, forcing the signal to be
     recorded and processed at the next leave_blocking_section or
     polling. */
  return 0;
}

/* Hooks for I/O locking */

static void caml_io_mutex_free(struct channel *chan)
{
  pthread_mutex_t * mutex = chan->mutex;
  if (mutex != NULL) {
    pthread_mutex_destroy(mutex);
    stat_free((char *) mutex);
  }
}

static void caml_io_mutex_lock(struct channel *chan)
{
  if (chan->mutex == NULL) {
    pthread_mutex_t * mutex =
      (pthread_mutex_t *) stat_alloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex, NULL);
    chan->mutex = (void *) mutex;
  }
  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (pthread_mutex_trylock(chan->mutex) == 0) {
    pthread_setspecific(last_channel_locked_key, (void *) chan);
    return;
  }
  /* If unsuccessful, block on mutex */
  enter_blocking_section();
  pthread_mutex_lock(chan->mutex);
  /* Problem: if a signal occurs at this point,
     and the signal handler raises an exception, we will not
     unlock the mutex.  The alternative (doing the setspecific
     before locking the mutex is also incorrect, since we could
     then unlock a mutex that is unlocked or locked by someone else. */
  pthread_setspecific(last_channel_locked_key, (void *) chan);
  leave_blocking_section();
}

static void caml_io_mutex_unlock(struct channel *chan)
{
  pthread_mutex_unlock(chan->mutex);
  pthread_setspecific(last_channel_locked_key, NULL);
}

static void caml_io_mutex_unlock_exn(void)
{
  struct channel * chan = pthread_getspecific(last_channel_locked_key);
  if (chan != NULL) caml_io_mutex_unlock(chan);
}

/* The "tick" thread fakes a SIGVTALRM signal at regular intervals. */

static void * caml_thread_tick(void * arg)
{
  struct timeval timeout;
  sigset_t mask;
#ifdef __linux__
  int tickcount = 0;
#endif

  /* Block all signals so that we don't try to execute
     a Caml signal handler */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
  while(1) {
    /* select() seems to be the most efficient way to suspend the
       thread for sub-second intervals */
    timeout.tv_sec = 0;
    timeout.tv_usec = Thread_timeout;
    select(0, NULL, NULL, NULL, &timeout);
    /* This signal should never cause a callback, so don't go through
       handle_signal(), tweak the global variable directly. */
    caml_pending_signals[SIGVTALRM] = 1;
    caml_signals_are_pending = 1;
#ifdef NATIVE_CODE
    young_limit = young_end;
#else
    something_to_do = 1;
#endif
#ifdef __linux__
    /* Hack around LinuxThreads' non-standard signal handling:
       if program is killed on a signal, e.g. SIGINT, the current
       thread will not die on this signal (because of the signal blocking
       above).  Hence, periodically check that the thread manager (our
       parent process) still exists. */
    tickcount++;
    if (tickcount >= 2000000 / Thread_timeout) { /* every 2 secs approx */
      tickcount = 0;
      if (getppid() == 1) pthread_exit(NULL);
    }
#endif
  }
  return NULL;                  /* prevents compiler warning */
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */

static void caml_thread_reinitialize(void)
{
  caml_thread_t thr, next;
  struct channel * chan;

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
  /* Reinitialize the master lock machinery,
     just in case the fork happened while other threads were doing
     leave_blocking_section */
  pthread_mutex_init(&caml_runtime_mutex, NULL);
  pthread_cond_init(&caml_runtime_is_free, NULL);
  caml_runtime_waiters = 0;     /* no other thread is waiting for the RTS */
  caml_runtime_busy = 1;        /* normally useless */
  /* Tick thread is not currently running in child process, will be
     re-created at next Thread.create */
  caml_tick_thread_running = 0;
  /* Reinitialize all IO mutexes */
  for (chan = caml_all_opened_channels;
       chan != NULL;
       chan = chan->next) {
    if (chan->mutex != NULL) pthread_mutex_init(chan->mutex, NULL);
  }
}

/* Initialize the thread machinery */

value caml_thread_initialize(value unit)   /* ML */
{
  value mu = Val_unit;
  value descr;

  /* Protect against repeated initialization (PR#1325) */
  if (curr_thread != NULL) return Val_unit;
  Begin_root (mu);
    /* Initialize the keys */
    pthread_key_create(&thread_descriptor_key, NULL);
    pthread_key_create(&last_channel_locked_key, NULL);
    /* Create and initialize the termination semaphore */
    mu = caml_threadstatus_new();
    /* Create a descriptor for the current thread */
    descr = alloc_small(3, 0);
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = Val_unit;
    Terminated(descr) = mu;
    thread_next_ident++;
    /* Create an info block for the current thread */
    curr_thread =
      (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    curr_thread->pthread = pthread_self();
    curr_thread->descr = descr;
    curr_thread->next = curr_thread;
    curr_thread->prev = curr_thread;
    curr_thread->backtrace_last_exn = Val_unit;
#ifdef NATIVE_CODE
    curr_thread->exit_buf = &caml_termination_jmpbuf;
#endif
    /* The stack-related fields will be filled in at the next
       enter_blocking_section */
    /* Associate the thread descriptor with the thread */
    pthread_setspecific(thread_descriptor_key, (void *) curr_thread);
    /* Set up the hooks */
    prev_scan_roots_hook = scan_roots_hook;
    scan_roots_hook = caml_thread_scan_roots;
    enter_blocking_section_hook = caml_thread_enter_blocking_section;
    leave_blocking_section_hook = caml_thread_leave_blocking_section;
    try_leave_blocking_section_hook = caml_thread_try_leave_blocking_section;
#ifdef NATIVE_CODE
    caml_termination_hook = pthread_exit;
#endif
    caml_channel_mutex_free = caml_io_mutex_free;
    caml_channel_mutex_lock = caml_io_mutex_lock;
    caml_channel_mutex_unlock = caml_io_mutex_unlock;
    caml_channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;
    /* Set up fork() to reinitialize the thread machinery in the child
       (PR#4577) */
    pthread_atfork(NULL, NULL, caml_thread_reinitialize);
  End_roots();
  return Val_unit;
}

/* Thread cleanup at termination */

static void caml_thread_stop(void)
{
  caml_thread_t th = curr_thread;

  /* Signal that the thread has terminated */
  caml_threadstatus_terminate(Terminated(th->descr));
  /* Remove th from the doubly-linked list of threads */
  th->next->prev = th->prev;
  th->prev->next = th->next;
  /* Release the runtime system */
  pthread_mutex_lock(&caml_runtime_mutex);
  caml_runtime_busy = 0;
  pthread_mutex_unlock(&caml_runtime_mutex);
  pthread_cond_signal(&caml_runtime_is_free);
#ifndef NATIVE_CODE
  /* Free the memory resources */
  stat_free(th->stack_low);
#endif
  if (th->backtrace_buffer != NULL) free(th->backtrace_buffer);
  /* Free the thread descriptor */
  stat_free(th);
}

/* Create a thread */

static void * caml_thread_start(void * arg)
{
  caml_thread_t th = (caml_thread_t) arg;
  value clos;
#ifdef NATIVE_CODE
  struct longjmp_buffer termination_buf;
#endif

  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) th);
  /* Acquire the global mutex and set up the stack variables */
  leave_blocking_section();
#ifdef NATIVE_CODE
  /* Setup termination handler (for caml_thread_exit) */
  if (sigsetjmp(termination_buf.buf, 0) == 0) {
    th->exit_buf = &termination_buf;
#endif
    /* Callback the closure */
    clos = Start_closure(th->descr);
    modify(&(Start_closure(th->descr)), Val_unit);
    callback_exn(clos, Val_unit);
    caml_thread_stop();
#ifdef NATIVE_CODE
  }
#endif
  /* The thread now stops running */
  return NULL;
}  

value caml_thread_new(value clos)          /* ML */
{
  pthread_attr_t attr;
  caml_thread_t th;
  pthread_t tick_pthread;
  value mu = Val_unit;
  value descr;
  int err;

  Begin_roots2 (clos, mu)
    /* Create and initialize the termination semaphore */
    mu = caml_threadstatus_new();
    /* Create a descriptor for the new thread */
    descr = alloc_small(3, 0);
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = clos;
    Terminated(descr) = mu;
    thread_next_ident++;
    /* Create an info block for the current thread */
    th = (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    th->descr = descr;
#ifdef NATIVE_CODE
    th->bottom_of_stack = NULL;
    th->exception_pointer = NULL;
    th->local_roots = NULL;
#else
    /* Allocate the stacks */
    th->stack_low = (value *) stat_alloc(Thread_stack_size);
    th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
    th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
    th->sp = th->stack_high;
    th->trapsp = th->stack_high;
    th->local_roots = NULL;
    th->external_raise = NULL;
#endif
    th->backtrace_pos = 0;
    th->backtrace_buffer = NULL;
    th->backtrace_last_exn = Val_unit;
    /* Add thread info block to the list of threads */
    th->next = curr_thread->next;
    th->prev = curr_thread;
    curr_thread->next->prev = th;
    curr_thread->next = th;
    /* Create the new thread */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    err = pthread_create(&th->pthread, &attr, caml_thread_start, (void *) th);
    if (err != 0) {
      /* Creation failed, remove thread info block from list of threads */
      th->next->prev = curr_thread;
      curr_thread->next = th->next;
#ifndef NATIVE_CODE
      stat_free(th->stack_low);
#endif
      stat_free(th);
      caml_pthread_check(err, "Thread.create");
    }
  End_roots();
  /* Create the tick thread if not already done.  
     Because of PR#4666, we start the tick thread late, only when we create
     the first additional thread in the current process*/
  if (! caml_tick_thread_running) {
    caml_tick_thread_running = 1;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    err = pthread_create(&tick_pthread, &attr, caml_thread_tick, NULL);
    caml_pthread_check(err, "Thread.create");
  }
  return descr;
}

/* Return the current thread */

value caml_thread_self(value unit)         /* ML */
{
  if (curr_thread == NULL) invalid_argument("Thread.self: not initialized");
  return curr_thread->descr;
}

/* Return the identifier of a thread */

value caml_thread_id(value th)          /* ML */
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

value caml_thread_uncaught_exception(value exn)  /* ML */
{
  char * msg = format_caml_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(curr_thread->descr)), msg);
  free(msg);
  if (caml_backtrace_active) print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Terminate current thread */

value caml_thread_exit(value unit)   /* ML */
{
#ifdef NATIVE_CODE
  /* We cannot call pthread_exit here because on some systems this
     raises a C++ exception, and ocamlopt-generated stack frames
     cannot be unwound.  Instead, we longjmp to the thread creation
     point (in caml_thread_start) or to the point in caml_main
     where caml_termination_hook will be called. */
  struct longjmp_buffer * exit_buf;
  if (curr_thread == NULL) invalid_argument("Thread.exit: not initialized");
  exit_buf = curr_thread->exit_buf;
  caml_thread_stop();
  siglongjmp(exit_buf->buf, 1);
#else
  /* No such problem in bytecode */
  if (curr_thread == NULL) invalid_argument("Thread.exit: not initialized");
  caml_thread_stop();
  pthread_exit(NULL);
#endif
  return Val_unit;  /* not reached */
}

/* Allow re-scheduling */

value caml_thread_yield(value unit)        /* ML */
{
  if (caml_runtime_waiters == 0) return Val_unit;
  enter_blocking_section();
#ifndef __linux__
  /* sched_yield() doesn't do what we want in Linux 2.6 and up (PR#2663) */
  sched_yield();
#endif
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value caml_thread_join(value th)          /* ML */
{
  int retcode = caml_threadstatus_wait(Terminated(th));
  caml_pthread_check(retcode, "Thread.join");
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (* ((pthread_mutex_t **) Data_custom_val(v)))
#define Max_mutex_number 1000

static void caml_mutex_finalize(value wrapper)
{
  pthread_mutex_t * mut = Mutex_val(wrapper);
  pthread_mutex_destroy(mut);
  stat_free(mut);
}

static int caml_mutex_condition_compare(value wrapper1, value wrapper2)
{
  pthread_mutex_t * mut1 = Mutex_val(wrapper1);
  pthread_mutex_t * mut2 = Mutex_val(wrapper2);
  return mut1 == mut2 ? 0 : mut1 < mut2 ? -1 : 1;
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value caml_mutex_new(value unit)        /* ML */
{
  pthread_mutex_t * mut;
  value wrapper;
  mut = stat_alloc(sizeof(pthread_mutex_t));
  caml_pthread_check(pthread_mutex_init(mut, NULL), "Mutex.create");
  wrapper = alloc_custom(&caml_mutex_ops, sizeof(pthread_mutex_t *),
                         1, Max_mutex_number);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

value caml_mutex_lock(value wrapper)     /* ML */
{
  int retcode;
  pthread_mutex_t * mut = Mutex_val(wrapper);
  /* PR#4351: first try to acquire mutex without releasing the master lock */
  retcode = pthread_mutex_trylock(mut);
  if (retcode == 0) return Val_unit;
  /* If unsuccessful, block on mutex */
  Begin_root(wrapper)           /* prevent the deallocation of mutex */
    enter_blocking_section();
    retcode = pthread_mutex_lock(mut);
    leave_blocking_section();
  End_roots();
  caml_pthread_check(retcode, "Mutex.lock");
  return Val_unit;
}

value caml_mutex_unlock(value wrapper)           /* ML */
{
  int retcode;
  pthread_mutex_t * mut = Mutex_val(wrapper);
  /* PR#4351: no need to release and reacquire master lock */
  retcode = pthread_mutex_unlock(mut);
  caml_pthread_check(retcode, "Mutex.unlock");
  return Val_unit;
}

value caml_mutex_try_lock(value wrapper)           /* ML */
{
  int retcode;
  pthread_mutex_t * mut = Mutex_val(wrapper);
  retcode = pthread_mutex_trylock(mut);
  if (retcode == EBUSY) return Val_false;
  caml_pthread_check(retcode, "Mutex.try_lock");
  return Val_true;
}

/* Conditions operations */

#define Condition_val(v) (* ((pthread_cond_t **) Data_custom_val(v)))
#define Max_condition_number 1000

static void caml_condition_finalize(value wrapper)
{
  pthread_cond_t * cond = Condition_val(wrapper);
  pthread_cond_destroy(cond);
  stat_free(cond);
}

static struct custom_operations caml_condition_ops = {
  "_condition",
  caml_condition_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value caml_condition_new(value unit)        /* ML */
{
  pthread_cond_t * cond;
  value wrapper;
  cond = stat_alloc(sizeof(pthread_cond_t));
  caml_pthread_check(pthread_cond_init(cond, NULL), "Condition.create");
  wrapper = alloc_custom(&caml_condition_ops, sizeof(pthread_cond_t *),
                         1, Max_condition_number);
  Condition_val(wrapper) = cond;
  return wrapper;
}

value caml_condition_wait(value wcond, value wmut)           /* ML */
{
  int retcode;
  pthread_cond_t * cond = Condition_val(wcond);
  pthread_mutex_t * mut = Mutex_val(wmut);
  Begin_roots2(wcond, wmut)     /* prevent deallocation of cond and mutex */
    enter_blocking_section();
    retcode = pthread_cond_wait(cond, mut);
    leave_blocking_section();
  End_roots();
  caml_pthread_check(retcode, "Condition.wait");
  return Val_unit;
}

value caml_condition_signal(value wrapper)           /* ML */
{
  int retcode;
  pthread_cond_t * cond = Condition_val(wrapper);
  retcode = pthread_cond_signal(cond);
  caml_pthread_check(retcode, "Condition.signal");
  return Val_unit;
}

value caml_condition_broadcast(value wrapper)           /* ML */
{
  int retcode;
  pthread_cond_t * cond = Condition_val(wrapper);
  retcode = pthread_cond_broadcast(cond);
  caml_pthread_check(retcode, "Condition.broadcast");
  return Val_unit;
}

/* Thread status blocks */

struct caml_threadstatus {
  pthread_mutex_t lock;          /* mutex for mutual exclusion */
  enum { ALIVE, TERMINATED } status;   /* status of thread */
  pthread_cond_t terminated;    /* signaled when thread terminates */
};

#define Threadstatus_val(v) \
  (* ((struct caml_threadstatus **) Data_custom_val(v)))
#define Max_threadstatus_number 500

static void caml_threadstatus_finalize(value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  pthread_mutex_destroy(&ts->lock);
  pthread_cond_destroy(&ts->terminated);
  stat_free(ts);
}

static struct custom_operations caml_threadstatus_ops = {
  "_threadstatus",
  caml_threadstatus_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value caml_threadstatus_new (void)
{
  struct caml_threadstatus * ts;
  value wrapper;
  ts = stat_alloc(sizeof(struct caml_threadstatus));
  caml_pthread_check(pthread_mutex_init(&ts->lock, NULL), "Thread.create");
  caml_pthread_check(pthread_cond_init(&ts->terminated, NULL),
                     "Thread.create");
  ts->status = ALIVE;
  wrapper = alloc_custom(&caml_threadstatus_ops,
                         sizeof(struct caml_threadstatus *),
                         1, Max_threadstatus_number);
  Threadstatus_val(wrapper) = ts;
  return wrapper;
}

void caml_threadstatus_terminate (value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  pthread_mutex_lock(&ts->lock);
  ts->status = TERMINATED;
  pthread_mutex_unlock(&ts->lock);
  pthread_cond_broadcast(&ts->terminated);
}

int caml_threadstatus_wait (value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  int retcode;

  Begin_roots1(wrapper)         /* prevent deallocation of ts */
    enter_blocking_section();
    retcode = pthread_mutex_lock(&ts->lock);
    if (retcode != 0) goto error;
    while (ts->status != TERMINATED) {
      retcode = pthread_cond_wait(&ts->terminated, &ts->lock);
      if (retcode != 0) goto error;
    }
    retcode = pthread_mutex_unlock(&ts->lock);
 error:
    leave_blocking_section();
  End_roots();
  return retcode;
}

/* Signal mask */

static void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

#ifndef NSIG
#define NSIG 64
#endif

static value encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i) > 0) {
        value newcons = alloc_small(2, 0);
        Field(newcons, 0) = Val_int(caml_rev_convert_signal_number(i));
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigmask_cmd[Int_val(cmd)];
  decode_sigset(sigs, &set);
  enter_blocking_section();
  retcode = pthread_sigmask(how, &set, &oldset);
  leave_blocking_section();
  caml_pthread_check(retcode, "Thread.sigmask");
  return encode_sigset(&oldset);
}

/* Synchronous signal wait */

value caml_wait_signal(value sigs) /* ML */
{
#ifdef HAS_SIGWAIT
  sigset_t set;
  int retcode, signo;

  decode_sigset(sigs, &set);
  enter_blocking_section();
  retcode = sigwait(&set, &signo);
  leave_blocking_section();
  caml_pthread_check(retcode, "Thread.wait_signal");
  return Val_int(signo);
#else
  invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);		/* not reached */
#endif
}

/* Error report */

static void caml_pthread_check(int retcode, char *msg)
{
  char * err;
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  err = strerror(retcode);
  msglen = strlen(msg);
  errlen = strlen(err);
  str = alloc_string(msglen + 2 + errlen);
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  memmove (&Byte(str, msglen + 2), err, errlen);
  raise_sys_error(str);
}

