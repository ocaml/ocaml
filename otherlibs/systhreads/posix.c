/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for POSIX 1003.1c threads */

#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in microseconds (50ms) */
#define Thread_timeout 50000

/* The thread descriptors */

struct caml_thread_struct {
  pthread_t pthread;            /* The Posix thread id */
  value ident;                  /* Unique id */
  value terminated;             /* Mutex held while the thread is running */
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  value * trapsp;               /* Saved value of trapsp for this thread */
  value * local_roots;          /* Saved value of local_roots for this thr. */
  struct longjmp_buffer * external_raise; /* Saved external_raise */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
};

typedef struct caml_thread_struct * caml_thread_t;

#define Assign(dst,src) modify((value *)&(dst), (value)(src))

/* The global mutex used to ensure that at most one thread is running
   Caml code */
pthread_mutex_t caml_mutex;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding Posix thread. */
pthread_key_t thread_descriptor_key;

/* Identifier for next thread creation */
static long thread_next_ident = 0;

/* Forward declarations */

value caml_mutex_new P((value));
value caml_mutex_lock P((value));
value caml_mutex_unlock P((value));

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) P((scanning_action));

static void caml_thread_scan_roots(action)
     scanning_action action;
{
  caml_thread_t curr_thread, new_curr_thread, th;
  register value * sp;
  value * block;

  curr_thread = pthread_getspecific(thread_descriptor_key);
  /* Scan all thread descriptors */
  (*action)((value) curr_thread, (value *) &new_curr_thread);
  Assert(curr_thread == new_curr_thread);
  /* Scan the stacks, except that of the current thread (already done). */
  for (th = curr_thread->next; th != curr_thread; th = th->next) {
    for (sp = th->sp; sp < th->stack_high; sp++) {
      (*action)(*sp, sp);
    }
    for (block = th->local_roots; block != NULL; block = (value *) block [1]){
      for (sp = block - (long) block [0]; sp < block; sp++){
        (*action)(*sp, sp);
      }
    }
  }
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Hooks for enter_blocking_section and leave_blocking_section */

static void (*prev_enter_blocking_section_hook) ();
static void (*prev_leave_blocking_section_hook) ();

static void caml_thread_enter_blocking_section()
{
  caml_thread_t curr_thread;
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();
  /* Save the stack-related global variables in the thread descriptor
     of the current thread */
  curr_thread = pthread_getspecific(thread_descriptor_key);
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->local_roots = local_roots;
  curr_thread->external_raise = external_raise;
  /* Release the global mutex */
  pthread_mutex_unlock(&caml_mutex);
}

static void caml_thread_leave_blocking_section()
{
  caml_thread_t curr_thread;
  /* Re-acquire the global mutex */
  pthread_mutex_lock(&caml_mutex);
  /* Restore the stack-related global variables */
  curr_thread = pthread_getspecific(thread_descriptor_key);
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
  local_roots = curr_thread->local_roots;
  external_raise = curr_thread->external_raise;
  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
}

/* The "tick" thread fakes a SIGVTALRM signal at regular intervals. */

static void * caml_thread_tick()
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
    something_to_do = 1;
  }
}

/* Thread cleanup: remove the descriptor from the list and free
   the stack space. */

static void caml_thread_cleanup(th)
     caml_thread_t th;
{
  /* Signal that the thread has terminated */
  caml_mutex_unlock(th->terminated);
  /* Remove th from the doubly-linked list of threads */
  Assign(th->next->prev, th->prev);
  Assign(th->prev->next, th->next);
  /* Free the memory resources */
  stat_free((char *) th->stack_low);
  th->stack_low = NULL;
  th->stack_high = NULL;
  th->stack_threshold = NULL;
  th->sp = NULL;
  th->trapsp = NULL;
  th->local_roots = NULL;
  th->external_raise = NULL;
  /* Release the main mutex */
  pthread_mutex_unlock(&caml_mutex);
}

/* Initialize the thread machinery */

value caml_thread_initialize(unit)   /* ML */
     value unit;
{
  pthread_t tick_pthread;
  pthread_attr_t attr;
  caml_thread_t th;
  Push_roots(r, 1);

  /* Initialize the main mutex */
  if (pthread_mutex_init(&caml_mutex, NULL) != 0) sys_error("Thread.init");
  pthread_mutex_lock(&caml_mutex);
  /* Initialize the key */
  pthread_key_create(&thread_descriptor_key, NULL);
  /* Create and acquire a termination lock for the current thread */
  r[0] = caml_mutex_new(Val_unit);
  caml_mutex_lock(r[0]);
  /* Create a descriptor for the current thread */
  th = (caml_thread_t)
    alloc_shr(sizeof(struct caml_thread_struct) / sizeof(value), 0);
  th->pthread = pthread_self();
  th->ident = Val_long(thread_next_ident);
  th->terminated = r[0];
  thread_next_ident++;
  /* The stack-related fields will be filled in at the next
     enter_blocking_section */
  th->next = th;
  th->prev = th;
  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) th);
  /* Allow cancellation */
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  /* Set up the hooks */
  prev_scan_roots_hook = scan_roots_hook;
  scan_roots_hook = caml_thread_scan_roots;
  prev_enter_blocking_section_hook = enter_blocking_section_hook;
  enter_blocking_section_hook = caml_thread_enter_blocking_section;
  prev_leave_blocking_section_hook = leave_blocking_section_hook;
  leave_blocking_section_hook = caml_thread_leave_blocking_section;
  /* Fork the tick thread */
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&tick_pthread, &attr, caml_thread_tick, NULL) != 0)
    sys_error("Thread.init");
  pthread_detach(tick_pthread);
  Pop_roots();
  return Val_unit;
}

/* Create a thread */

static void * caml_thread_start(th)
     caml_thread_t th;
{
  value clos;
  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) th);
  /* Set up termination routine */
  pthread_cleanup_push(caml_thread_cleanup, (void *) th);
  /* Allow cancellation */
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  /* Acquire the global mutex and set up the stack variables */
  leave_blocking_section();
  /* Callback the closure */
  clos = *extern_sp++;
  callback(clos, Val_unit);
  /* Cleanup: free the thread resources and release the mutex */
  pthread_cleanup_pop(1);
  return 0;
}  

value caml_thread_new(clos)          /* ML */
     value clos;
{
  pthread_attr_t attr;
  caml_thread_t th, curr_thread;
  Push_roots(r, 1);

  /* Create and acquire the termination lock */
  r[0] = caml_mutex_new(Val_unit);
  caml_mutex_lock(r[0]);
  /* Allocate the thread and its stack */
  th = (caml_thread_t)
    alloc_shr(sizeof(struct caml_thread_struct) / sizeof(value), 0);
  th->ident = Val_long(thread_next_ident);
  thread_next_ident++;
  th->terminated = r[0];
  th->stack_low = (value *) stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  th->local_roots = NULL;
  th->external_raise = NULL;
  /* Add it to the list of threads */
  curr_thread = pthread_getspecific(thread_descriptor_key);
  th->next = curr_thread->next;
  th->prev = curr_thread;
  Assign(curr_thread->next->prev, th);
  Assign(curr_thread->next, th);
  /* Pass the closure in the newly created stack, so that it will be
     preserved by garbage collection */
  *--(th->sp) = clos;
  /* Fork the new thread */
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&th->pthread, &attr, caml_thread_start, (void *) th) != 0)
    sys_error("Thread.new");
  Pop_roots();
  return (value) th;
}

/* Return the current thread */

value caml_thread_self(unit)         /* ML */
     value unit;
{
  caml_thread_t curr_thread;
  curr_thread = pthread_getspecific(thread_descriptor_key);
  return (value) curr_thread;
}

/* Return the identifier of a thread */

value caml_thread_id(th)          /* ML */
     caml_thread_t th;
{
  return th->ident;
}

/* Allow re-scheduling */

value caml_thread_yield(unit)        /* ML */
     value unit;
{
  enter_blocking_section();
#if defined(HAS_SCHED_YIELD)
  sched_yield();
#elif defined(HAS_PTHREAD_YIELD)
  pthread_yield();
#endif
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value caml_thread_join(th)          /* ML */
     caml_thread_t th;
{
  caml_mutex_lock(th->terminated);
  caml_mutex_unlock(th->terminated);
  return Val_unit;
}

/* Terminate the current thread */

value caml_thread_exit(unit)       /* ML */
     value unit;
{
  pthread_exit(0);
  return Val_unit;              /* never reached */
}

/* Kill another thread */

value caml_thread_kill(th)       /* ML */
     caml_thread_t th;
{
  pthread_cancel(th->pthread);
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (*((pthread_mutex_t *)(&Field(v, 1))))
#define Max_mutex_number 1000

static void caml_mutex_finalize(mut)
     value mut;
{
  pthread_mutex_destroy(&Mutex_val(mut));
}

value caml_mutex_new(unit)        /* ML */
     value unit;
{
  value mut;
  mut = alloc_final(1 + sizeof(pthread_mutex_t) / sizeof(value),
                    caml_mutex_finalize, 1, Max_mutex_number);
  if (pthread_mutex_init(&Mutex_val(mut), NULL) != 0)
    sys_error("Mutex.new");
  return mut;
}

value caml_mutex_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_lock(&(Mutex_val(mut)));
  leave_blocking_section();
  if (retcode != 0) sys_error("Mutex.lock");
  return Val_unit;
}

value caml_mutex_unlock(mut)           /* ML */
     value mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_unlock(&(Mutex_val(mut)));
  leave_blocking_section();
  if (retcode != 0) sys_error("Mutex.unlock");
  return Val_unit;
}

value caml_mutex_try_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  retcode = pthread_mutex_trylock(&(Mutex_val(mut)));
  return retcode == 0 ? Val_true : Val_false;
}

/* Conditions operations */

#define Condition_val(v) (*((pthread_cond_t *)(&Field(v, 1))))
#define Max_condition_number 1000

static void caml_condition_finalize(cond)
     value cond;
{
  pthread_cond_destroy(&Condition_val(cond));
}

value caml_condition_new(unit)        /* ML */
     value unit;
{
  value cond;
  cond = alloc_final(1 + sizeof(pthread_cond_t) / sizeof(value),
                     caml_condition_finalize, 1, Max_condition_number);
  if (pthread_cond_init(&Condition_val(cond), NULL) != 0)
    sys_error("Condition.new");
  return cond;
}

value caml_condition_wait(cond, mut)           /* ML */
     value cond, mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_wait(&Condition_val(cond), &Mutex_val(mut));
  leave_blocking_section();
  if (retcode != 0) sys_error("Condition.wait");
  return Val_unit;
}

value caml_condition_signal(cond)           /* ML */
     value cond;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_signal(&Condition_val(cond));
  leave_blocking_section();
  if (retcode != 0) sys_error("Condition.signal");
  return Val_unit;
}

value caml_condition_broadcast(cond)           /* ML */
     value cond;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_broadcast(&Condition_val(cond));
  leave_blocking_section();
  if (retcode != 0) sys_error("Condition.broadcast");
  return Val_unit;
}

