/***********************************************************************/
/*                                                                     */
/*                             Objective Caml                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for POSIX 1003.1c threads */

#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include "alloc.h"
#include "callback.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
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
  value descr;                  /* The heap-allocated descriptor */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
#ifdef NATIVE_CODE
  char * bottom_of_stack;       /* Saved value of caml_bottom_of_stack */
  unsigned long last_retaddr;   /* Saved value of caml_last_return_address */
  value * gc_regs;              /* Saved value of caml_gc_regs */
  char * exception_pointer;     /* Saved value of caml_exception_pointer */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
#else
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  value * trapsp;               /* Saved value of trapsp for this thread */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
  struct longjmp_buffer * external_raise; /* Saved external_raise */
#endif
};

typedef struct caml_thread_struct * caml_thread_t;

/* The descriptor for the currently executing thread */

static caml_thread_t curr_thread = NULL;

/* The global mutex used to ensure that at most one thread is running
   Caml code */
static pthread_mutex_t caml_mutex;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding Posix thread. */
static pthread_key_t thread_descriptor_key;

/* The key used for unlocking I/O channels on exceptions */
static pthread_key_t last_channel_locked_key;

/* Identifier for next thread creation */
static long thread_next_ident = 0;

/* Forward declarations */

value caml_mutex_new (value);
value caml_mutex_lock (value);
value caml_mutex_unlock (value);
static void caml_pthread_check (int, char *);

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action);

static void caml_thread_scan_roots(scanning_action action)
{
  caml_thread_t th;

  th = curr_thread;
  do {
    (*action)(th->descr, &th->descr);
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

static void (*prev_enter_blocking_section_hook) () = NULL;
static void (*prev_leave_blocking_section_hook) () = NULL;

static void caml_thread_enter_blocking_section(void)
{
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();
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
  /* Release the global mutex */
  pthread_mutex_unlock(&caml_mutex);
}

static void caml_thread_leave_blocking_section(void)
{
  /* Re-acquire the global mutex */
  pthread_mutex_lock(&caml_mutex);
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
  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
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
  enter_blocking_section();
  pthread_mutex_lock(chan->mutex);
  leave_blocking_section();
  pthread_setspecific(last_channel_locked_key, (void *) chan);
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
  while(1) {
    /* select() seems to be the most efficient way to suspend the
       thread for sub-second intervals */
    timeout.tv_sec = 0;
    timeout.tv_usec = Thread_timeout;
    select(0, NULL, NULL, NULL, &timeout);
    /* This signal should never cause a callback, so don't go through
       handle_signal(), tweak the global variables directly. */
    pending_signal = SIGVTALRM;
#ifdef NATIVE_CODE
    young_limit = young_end;
#else
    something_to_do = 1;
#endif
  }
  return NULL;                  /* prevents compiler warning */
}

/* Initialize the thread machinery */

value caml_thread_initialize(value unit)   /* ML */
{
  pthread_t tick_pthread;
  pthread_attr_t attr;
  value mu = Val_unit;
  value descr;

  Begin_root (mu);
    /* Initialize the main mutex */
    caml_pthread_check(pthread_mutex_init(&caml_mutex, NULL),
                       "Thread.init");
    pthread_mutex_lock(&caml_mutex);
    /* Initialize the keys */
    pthread_key_create(&thread_descriptor_key, NULL);
    pthread_key_create(&last_channel_locked_key, NULL);
    /* Create and acquire a termination lock for the current thread */
    mu = caml_mutex_new(Val_unit);
    caml_mutex_lock(mu);
    /* Create a descriptor for the current thread */
    descr = alloc_tuple(3);
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
    /* The stack-related fields will be filled in at the next
       enter_blocking_section */
    /* Associate the thread descriptor with the thread */
    pthread_setspecific(thread_descriptor_key, (void *) curr_thread);
    /* Set up the hooks */
    prev_scan_roots_hook = scan_roots_hook;
    scan_roots_hook = caml_thread_scan_roots;
    prev_enter_blocking_section_hook = enter_blocking_section_hook;
    enter_blocking_section_hook = caml_thread_enter_blocking_section;
    prev_leave_blocking_section_hook = leave_blocking_section_hook;
    leave_blocking_section_hook = caml_thread_leave_blocking_section;
    channel_mutex_free = caml_io_mutex_free;
    channel_mutex_lock = caml_io_mutex_lock;
    channel_mutex_unlock = caml_io_mutex_unlock;
    channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;
    /* Fork the tick thread */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    caml_pthread_check(
        pthread_create(&tick_pthread, &attr, caml_thread_tick, NULL),
        "Thread.init");
    pthread_detach(tick_pthread);
  End_roots();
  return Val_unit;
}

/* Create a thread */

static void * caml_thread_start(void * arg)
{
  caml_thread_t th = (caml_thread_t) arg;
  value clos;

  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) th);
  /* Acquire the global mutex and set up the stack variables */
  leave_blocking_section();
  /* Callback the closure */
  clos = Start_closure(th->descr);
  Modify(&(Start_closure(th->descr)), Val_unit);
  callback(clos, Val_unit);
  /* Signal that the thread has terminated */
  caml_mutex_unlock(Terminated(th->descr));
  /* Remove th from the doubly-linked list of threads */
  th->next->prev = th->prev;
  th->prev->next = th->next;
#ifndef NATIVE_CODE
  /* Free the memory resources */
  stat_free(th->stack_low);
#endif
  /* Free the thread descriptor */
  stat_free(th);
  /* Release the main mutex (forever) */
  enter_blocking_section();
  /* The thread now stops running */
  return NULL;
}  

value caml_thread_new(value clos)          /* ML */
{
  pthread_attr_t attr;
  caml_thread_t th;
  value mu = Val_unit;
  value descr;
  int err;

  Begin_roots2 (clos, mu)
    /* Create and acquire the termination lock */
    mu = caml_mutex_new(Val_unit);
    caml_mutex_lock(mu);
    /* Create a descriptor for the new thread */
    descr = alloc_tuple(3);
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
    /* Add thread info block to the list of threads */
    th->next = curr_thread->next;
    th->prev = curr_thread;
    curr_thread->next->prev = th;
    curr_thread->next = th;
    /* Fork the new thread */
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    err = pthread_create(&th->pthread, &attr, caml_thread_start, (void *) th);
    if (err != 0) {
      /* Fork failed, remove thread info block from list of threads */
      th->next->prev = curr_thread;
      curr_thread->next = th->next;
#ifndef NATIVE_CODE
      stat_free(th->stack_low);
#endif
      stat_free(th);
      caml_pthread_check(err, "Thread.create");
    }
  End_roots();
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

/* Allow re-scheduling */

value caml_thread_yield(value unit)        /* ML */
{
  enter_blocking_section();
  sched_yield();
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value caml_thread_join(value th)          /* ML */
{
  Begin_root(th)
    caml_mutex_lock(Terminated(th));
    caml_mutex_unlock(Terminated(th));
  End_roots();
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (*((pthread_mutex_t *)(&Field(v, 1))))
#define Max_mutex_number 1000

static void caml_mutex_finalize(value mut)
{
  pthread_mutex_destroy(&Mutex_val(mut));
}

value caml_mutex_new(value unit)        /* ML */
{
  value mut;
  mut = alloc_final(1 + sizeof(pthread_mutex_t) / sizeof(value),
                    caml_mutex_finalize, 1, Max_mutex_number);
  caml_pthread_check(pthread_mutex_init(&Mutex_val(mut), NULL),
                     "Mutex.create");
  return mut;
}

value caml_mutex_lock(value mut)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_lock(&(Mutex_val(mut)));
  leave_blocking_section();
  caml_pthread_check(retcode, "Mutex.lock");
  return Val_unit;
}

value caml_mutex_unlock(value mut)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_unlock(&(Mutex_val(mut)));
  leave_blocking_section();
  caml_pthread_check(retcode, "Mutex.unlock");
  return Val_unit;
}

value caml_mutex_try_lock(value mut)           /* ML */
{
  int retcode;
  retcode = pthread_mutex_trylock(&(Mutex_val(mut)));
  if (retcode == EBUSY) return Val_false;
  caml_pthread_check(retcode, "Mutex.try_lock");
  return Val_true;
}

/* Conditions operations */

#define Condition_val(v) (*((pthread_cond_t *)(&Field(v, 1))))
#define Max_condition_number 1000

static void caml_condition_finalize(value cond)
{
  pthread_cond_destroy(&Condition_val(cond));
}

value caml_condition_new(value unit)        /* ML */
{
  value cond;
  cond = alloc_final(1 + sizeof(pthread_cond_t) / sizeof(value),
                     caml_condition_finalize, 1, Max_condition_number);
  caml_pthread_check(pthread_cond_init(&Condition_val(cond), NULL),
                     "Condition.create");
  return cond;
}

value caml_condition_wait(value cond, value mut)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_wait(&Condition_val(cond), &Mutex_val(mut));
  leave_blocking_section();
  caml_pthread_check(retcode, "Condition.wait");
  return Val_unit;
}

value caml_condition_signal(value cond)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_signal(&Condition_val(cond));
  leave_blocking_section();
  caml_pthread_check(retcode, "Condition.signal");
  return Val_unit;
}

value caml_condition_broadcast(value cond)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_broadcast(&Condition_val(cond));
  leave_blocking_section();
  caml_pthread_check(retcode, "Condition.broadcast");
  return Val_unit;
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
  bcopy(msg, &Byte(str, 0), msglen);
  bcopy(": ", &Byte(str, msglen), 2);
  bcopy(err, &Byte(str, msglen + 2), errlen);
  raise_sys_error(str);
}
