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

#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include "alloc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"

#ifdef HAS_THR_YIELD
#include <thread.h>
#endif

/* Max computation time before rescheduling, in microseconds (50ms) */
#define Thread_timeout 50000

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* The thread descriptors */

struct csl_thread_struct {
  pthread_t pthread;            /* The corresponding Posix thread */
  value ident;                  /* Unique id */
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  value * trapsp;               /* Saved value of trapsp for this thread */
  struct csl_thread_struct * next;  /* Double linking of threads */
  struct csl_thread_struct * prev;
};

typedef struct csl_thread_struct * csl_thread_t;

#define Assign(dst,src) modify((value *)&(dst), (value)(src))

/* The global mutex used to ensure that at most one thread is running
   Caml code */
pthread_mutex_t csl_mutex;

/* Head of the list of thread descriptors */
csl_thread_t thread_list = NULL;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding Posix thread. */
pthread_key_t thread_descriptor_key;

/* Identifier for next thread creation */
static value thread_next_ident = 0;

/* Compatibility code for DEC OSF1 */

#ifdef __osf__
#define Attr_default pthread_attr_default                    
#define Mutexattr_default pthread_mutexattr_default
#define Condattr_default pthread_condattr_default
#define Getspecific(res,key) pthread_getspecific(key, (void **) &(res))
#define Pthread_key_create pthread_keycreate
#define Pthread_detach(th) pthread_detach(&(th))
#else
#define Attr_default NULL
#define Mutexattr_default NULL
#define Condattr_default NULL
#define Getspecific(res,key) (res) = pthread_getspecific(key)
#define Pthread_key_create pthread_key_create
#define Pthread_detach pthread_detach
#endif

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) P((scanning_action));

static void csl_thread_scan_roots(action)
     scanning_action action;
{
  csl_thread_t th;
  register value * sp;
  /* Scan all thread descriptors */
  (*action)((value) thread_list, (value *) &thread_list);
  /* Scan the stacks */
  for (th = thread_list; th != NULL; th = th->next) {
    /* If this is the current thread, don't scan its stack, this
       has already been done */
    if (th->stack_low == stack_low) continue;
    for (sp = th->sp; sp < th->stack_high; sp++) {
      (*action)(*sp, sp);
    }
  }
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Hooks for enter_blocking_section and leave_blocking_section */

static void (*prev_enter_blocking_section_hook) ();
static void (*prev_leave_blocking_section_hook) ();

static void csl_thread_enter_blocking_section()
{
  csl_thread_t curr_thread;
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();
  /* Save the stack-related global variables in the thread descriptor
     of the current thread */
  Getspecific(curr_thread, thread_descriptor_key);
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  /* Release the global mutex */
  pthread_mutex_unlock(&csl_mutex);
}

static void csl_thread_leave_blocking_section()
{
  csl_thread_t curr_thread;
  /* Re-acquire the global mutex */
  pthread_mutex_lock(&csl_mutex);
  /* Restore the stack-related global variables */
  Getspecific(curr_thread, thread_descriptor_key);
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
}

/* The "tick" thread fakes a SIGVTALRM signal at regular intervals. */

static void * csl_thread_tick()
{
  struct timeval timeout;
  while(1) {
    /* select() seems to be the most efficient way to suspend the
       thread for sub-second intervals */
    timeout.tv_sec = 0;
    timeout.tv_usec = Thread_timeout;
    select(0, NULL, NULL, NULL, &timeout);
    if (!async_signal_mode) handle_signal(SIGVTALRM);
  }
}

/* Thread cleanup: remove the descriptor from the list and
   free the stack space and the descriptor itself. */

static void csl_thread_cleanup(th)
     csl_thread_t th;
{
  /* Remove th from the doubly-linked list of threads */
  if (th == thread_list) {
    thread_list = th->next;
  } else {
    Assign(th->next->prev, th->prev);
    Assign(th->prev->next, th->next);
  }
  /* Free the memory resources */
  stat_free((char *) th->stack_low);
  th->stack_low = NULL;
  th->stack_high = NULL;
  th->stack_threshold = NULL;
  th->sp = NULL;
  th->trapsp = NULL;
}

/* Initialize the thread machinery */

value csl_thread_initialize(unit)   /* ML */
     value unit;
{
  pthread_t tick_pthread;
  /* Initialize the mutex */
  if (pthread_mutex_init(&csl_mutex, Mutexattr_default) == -1)
    sys_error("Thread.init");
  pthread_mutex_lock(&csl_mutex);
  /* Initialize the key */
  Pthread_key_create(&thread_descriptor_key, NULL);
  /* Create a descriptor for the current thread */
  thread_list = (csl_thread_t)
    alloc_shr(sizeof(struct csl_thread_struct) / sizeof(value), 0);
  thread_list->pthread = pthread_self();
  thread_list->ident = Val_long(thread_next_ident);
  thread_next_ident++;
  /* The stack-related fields will be filled in at the next
     enter_blocking_section */
  thread_list->next = NULL;
  thread_list->prev = NULL;
  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) thread_list);
  /* Set up the hooks */
  prev_scan_roots_hook = scan_roots_hook;
  scan_roots_hook = csl_thread_scan_roots;
  prev_enter_blocking_section_hook = enter_blocking_section_hook;
  enter_blocking_section_hook = csl_thread_enter_blocking_section;
  prev_leave_blocking_section_hook = leave_blocking_section_hook;
  leave_blocking_section_hook = csl_thread_leave_blocking_section;
  /* Fork the tick thread */
  if (pthread_create(&tick_pthread, Attr_default, csl_thread_tick, 0) == -1)
    sys_error("Thread.init");
  Pthread_detach(tick_pthread);
  return Val_unit;
}

/* Create a thread */

static void * csl_thread_start(th)
     csl_thread_t th;
{
  value clos;
  /* Associate the thread descriptor with the thread */
  pthread_setspecific(thread_descriptor_key, (void *) th);
  /* Set up termination routine */
  pthread_cleanup_push(csl_thread_cleanup, (void *) th);
  /* Acquire the global mutex before running the thread */
  pthread_mutex_lock(&csl_mutex);  
  /* Set up the stack variables */
  stack_low = th->stack_low;
  stack_high = th->stack_high;
  stack_threshold = th->stack_threshold;
  extern_sp = th->sp;
  trapsp = th->trapsp;
  /* Callback the closure */
  clos = *extern_sp++;
  callback(clos, Val_unit);
  /* Cleanup: free the thread resources */
  pthread_cleanup_pop(1);
  /* Release the mutex and die quietly */
  pthread_mutex_unlock(&csl_mutex);
  return 0;
}  

value csl_thread_new(clos)          /* ML */
     value clos;
{
  csl_thread_t th;
  /* Allocate the thread and its stack */
  th = (csl_thread_t)
    alloc_shr(sizeof(struct csl_thread_struct) / sizeof(value), 0);
  th->ident = Val_long(thread_next_ident);
  thread_next_ident++;
  th->stack_low = (value *) stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  /* Add it to the list of threads */
  th->next = thread_list;
  th->prev = NULL;
  Assign(thread_list->prev, th);
  thread_list = th;
  /* Pass the closure in the newly created stack, so that it will be
     preserved by garbage collection */
  *--(th->sp) = clos;
  /* Fork the new thread */
  if (pthread_create(&(th->pthread), Attr_default, csl_thread_start,
                     (void *) th) == -1)
    sys_error("Thread.new");
  return (value) th;
}

/* Return the current thread */

value csl_thread_self(unit)         /* ML */
     value unit;
{
  csl_thread_t curr_thread;
  Getspecific(curr_thread, thread_descriptor_key);
  return (value) curr_thread;
}

/* Return the identifier of a thread */

value csl_thread_id(th)          /* ML */
     csl_thread_t th;
{
  return th->ident;
}

/* Allow re-scheduling */

value csl_thread_yield(unit)        /* ML */
     value unit;
{
  enter_blocking_section();
#if defined(HAS_PTHREAD_YIELD)
  pthread_yield();
#elif defined(HAS_THR_YIELD)
  thr_yield();
#endif
  leave_blocking_section();
  return Val_unit;
}

/* Detach a thread */

value csl_thread_detach(th)         /* ML */
     csl_thread_t th;
{
  if (Pthread_detach(th->pthread) == -1) sys_error("Thread.detach");
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value csl_thread_join(th)          /* ML */
     csl_thread_t th;
{
  void * status;
  int retcode;
  enter_blocking_section();
  retcode = pthread_join(th->pthread, &status);
  leave_blocking_section();
  if (retcode == -1) sys_error("Thread.join");
  return Val_unit;
}

/* Terminate the current thread */

value csl_thread_exit(unit)       /* ML */
     value unit;
{
  enter_blocking_section();
  pthread_exit(0);
  return Val_unit;              /* never reached */
}

/* Mutex operations */

#define Mutex_val(v) (*((pthread_mutex_t *)(&Field(v, 1))))
#define Max_mutex_number 1000

static void csl_mutex_finalize(mut)
     value mut;
{
  pthread_mutex_destroy(&Mutex_val(mut));
}

value csl_mutex_new(unit)        /* ML */
     value unit;
{
  value mut;
  mut = alloc_final(1 + sizeof(pthread_mutex_t) / sizeof(value),
                    csl_mutex_finalize, 1, Max_mutex_number);
  if (pthread_mutex_init(&Mutex_val(mut), Mutexattr_default) == -1)
    sys_error("Mutex.new");
  return mut;
}

value csl_mutex_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_lock(&(Mutex_val(mut)));
  leave_blocking_section();
  if (retcode == -1) sys_error("Mutex.lock");
  return Val_unit;
}

value csl_mutex_unlock(mut)           /* ML */
     value mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_mutex_unlock(&(Mutex_val(mut)));
  leave_blocking_section();
  if (retcode == -1) sys_error("Mutex.unlock");
  return Val_unit;
}

value csl_mutex_try_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  retcode = pthread_mutex_trylock(&(Mutex_val(mut)));
  if (retcode == -1) sys_error("Mutex.try_lock");
  return Val_bool(retcode);
}

/* Conditions operations */

#define Condition_val(v) (*((pthread_cond_t *)(&Field(v, 1))))
#define Max_condition_number 1000

static void csl_condition_finalize(cond)
     value cond;
{
  pthread_cond_destroy(&Condition_val(cond));
}

value csl_condition_new(unit)        /* ML */
     value unit;
{
  value cond;
  cond = alloc_final(1 + sizeof(pthread_cond_t) / sizeof(value),
                     csl_condition_finalize, 1, Max_condition_number);
  if (pthread_cond_init(&Condition_val(cond), Condattr_default) == -1)
    sys_error("Condition.new");
  return cond;
}

value csl_condition_wait(cond, mut)           /* ML */
     value cond, mut;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_wait(&Condition_val(cond), &Mutex_val(mut));
  leave_blocking_section();
  if (retcode == -1) sys_error("Condition.wait");
  return Val_unit;
}

value csl_condition_signal(cond)           /* ML */
     value cond;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_signal(&Condition_val(cond));
  leave_blocking_section();
  if (retcode == -1) sys_error("Condition.signal");
  return Val_unit;
}

value csl_condition_broadcast(cond)           /* ML */
     value cond;
{
  int retcode;
  enter_blocking_section();
  retcode = pthread_cond_broadcast(&Condition_val(cond));
  leave_blocking_section();
  if (retcode == -1) sys_error("Condition.broadcast");
  return Val_unit;
}

