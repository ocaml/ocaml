/***********************************************************************/
/*                                                                     */
/*                         Objective Caml                              */
/*                                                                     */
/*           Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for Win32 threads */

#include <windows.h>
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

/* Signal used for timer preemption (any unused signal number) */
#define SIGTIMER 1

/* The ML value describing a thread (heap-allocated) */

struct caml_thread_handle {
  value final_fun;              /* Finalization function */
  HANDLE handle;                /* Windows handle */
};

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  struct caml_thread_handle * thread_handle; /* Finalized object with handle */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Start_closure(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Threadhandle(v) (((struct caml_thread_descr *)(v))->thread_handle)

/* The infos on threads (allocated via malloc()) */

struct caml_thread_struct {
  HANDLE wthread;               /* The Windows thread handle */
  value descr;                  /* The heap-allocated descriptor */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
#ifdef NATIVE_CODE
  struct caml_context * last_context;
                                /* Saved value of caml_last_context */
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

/* The descriptor for the currently executing thread (thread-specific) */

static __declspec( thread ) caml_thread_t curr_thread = NULL;

/* The global mutex used to ensure that at most one thread is running
   Caml code */
static HANDLE caml_mutex;

/* The thread-specific variable holding last locked I/O channel */

static __declspec( thread ) struct channel * last_channel_locked = NULL;

/* Identifier for next thread creation */
static long thread_next_ident = 0;

/* These declarations should go in some include file */

#ifdef NATIVE_CODE
extern char * caml_bottom_of_stack;
extern unsigned long caml_last_return_address;
extern char * caml_exception_pointer;
#endif

/* Forward declarations */

static void caml_wthread_error (char * msg);

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
      if (th->last_context != NULL)
        do_local_roots(action, th->last_context, th->local_roots);
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
  curr_thread->last_context = caml_last_context;
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
  ReleaseMutex(caml_mutex);
}

static void caml_thread_leave_blocking_section(void)
{
  /* Re-acquire the global mutex */
  WaitForSingleObject(caml_mutex, INFINITE);
  /* Restore the stack-related global variables */
#ifdef NATIVE_CODE
  caml_last_context = curr_thread->last_context;
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

static void caml_io_mutex_free(struct channel * chan)
{
  HANDLE mutex = chan->mutex;
  if (mutex != NULL) {
    CloseHandle(mutex);
  }
}

static void caml_io_mutex_lock(struct channel * chan)
{
  if (chan->mutex == NULL) {
    HANDLE mutex = CreateMutex(NULL, FALSE, NULL);
    if (mutex == NULL) caml_wthread_error("Thread.iolock");
    chan->mutex = (void *) mutex;
  }
  enter_blocking_section();
  WaitForSingleObject((HANDLE) chan->mutex, INFINITE);
  leave_blocking_section();
  last_channel_locked = chan;
}

static void caml_io_mutex_unlock(struct channel * chan)
{
  ReleaseMutex((HANDLE) chan->mutex);
  last_channel_locked = NULL;
}

static void caml_io_mutex_unlock_exn(void)
{
  if (last_channel_locked != NULL) caml_io_mutex_unlock(last_channel_locked);
}

/* The "tick" thread fakes a SIGVTALRM signal at regular intervals. */

static void * caml_thread_tick(void)
{
  while(1) {
    Sleep(Thread_timeout);
    pending_signal = SIGTIMER;
#ifdef NATIVE_CODE
    young_limit = young_end;
#else
    something_to_do = 1;
#endif
  }
}

static void caml_thread_finalize(value vthread)
{
  CloseHandle(((struct caml_thread_handle *)vthread)->handle);
}

/* Initialize the thread machinery */

value caml_thread_initialize(value unit)   /* ML */
{
  value vthread = Val_unit;
  value descr;
  HANDLE tick_thread;
  unsigned long tick_id;

  Begin_root (vthread);
    /* Initialize the main mutex and acquire it */
    caml_mutex = CreateMutex(NULL, TRUE, NULL);
    if (caml_mutex == NULL) caml_wthread_error("Thread.init");
    /* Create a finalized value to hold thread handle */
    vthread = alloc_final(sizeof(struct caml_thread_handle) / sizeof(value),
			  caml_thread_finalize, 1, 1000);
    ((struct caml_thread_handle *)vthread)->handle = NULL;
    /* Create a descriptor for the current thread */
    descr = alloc_tuple(sizeof(struct caml_thread_descr) / sizeof(value));
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = Val_unit;
    Threadhandle(descr) = (struct caml_thread_handle *) vthread;
    thread_next_ident++;
    /* Create an info block for the current thread */
    curr_thread =
      (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &(curr_thread->wthread),
                    0, FALSE, DUPLICATE_SAME_ACCESS);
    if (curr_thread->wthread == NULL) caml_wthread_error("Thread.init");
    ((struct caml_thread_handle *)vthread)->handle = curr_thread->wthread;
    curr_thread->descr = descr;
    curr_thread->next = curr_thread;
    curr_thread->prev = curr_thread;
    /* The stack-related fields will be filled in at the next
       enter_blocking_section */
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
    tick_thread =
      CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)&caml_thread_tick,
                   NULL, 0, &tick_id);
    if (tick_thread == NULL) caml_wthread_error("Thread.init");
    CloseHandle(tick_thread);
  End_roots();
  return Val_unit;
}

/* Create a thread */

static void caml_thread_start(caml_thread_t th)
{
  value clos;

  /* Initialize the per-thread variables */
  curr_thread = th;
  last_channel_locked = NULL;
  /* Acquire the global mutex and set up the stack variables */
  leave_blocking_section();
  /* Callback the closure */
  clos = Start_closure(th->descr);
  Modify(&(Start_closure(th->descr)), Val_unit);
  callback(clos, Val_unit);
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
}  

value caml_thread_new(value clos)          /* ML */
{
  caml_thread_t th;
  value vthread = Val_unit;
  value descr;
  unsigned long th_id;

  Begin_roots2 (clos, vthread)
    /* Create a finalized value to hold thread handle */
    vthread = alloc_final(sizeof(struct caml_thread_handle) / sizeof(value),
			  caml_thread_finalize, 1, 1000);
    ((struct caml_thread_handle *)vthread)->handle = NULL;
    /* Create a descriptor for the new thread */
    descr = alloc_tuple(sizeof(struct caml_thread_descr) / sizeof(value));
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = clos;
    Threadhandle(descr) = (struct caml_thread_handle *) vthread;
    thread_next_ident++;
    /* Create an info block for the current thread */
    th = (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    th->descr = descr;
#ifdef NATIVE_CODE
    th->bottom_of_stack = NULL;
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
    th->wthread =
      CreateThread(NULL,0, (LPTHREAD_START_ROUTINE) caml_thread_start,
                   (void *) th, 0, &th_id);
    if (th->wthread == NULL) {
      /* Fork failed, remove thread info block from list of threads */
      th->next->prev = curr_thread;
      curr_thread->next = th->next;
#ifndef NATIVE_CODE
      stat_free(th->stack_low);
#endif
      stat_free(th);
      caml_wthread_error("Thread.create");
    }
    ((struct caml_thread_handle *)vthread)->handle = th->wthread;
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
  Sleep(0);
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value caml_thread_join(value th)          /* ML */
{
  HANDLE h = Threadhandle(th)->handle;
  enter_blocking_section();
  WaitForSingleObject(h, INFINITE);
  leave_blocking_section();
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (*((HANDLE *)(&Field(v, 1))))
#define Max_mutex_number 1000

static void caml_mutex_finalize(value mut)
{
  CloseHandle(Mutex_val(mut));
}

value caml_mutex_new(value unit)        /* ML */
{
  value mut;
  mut = alloc_final(1 + sizeof(HANDLE) / sizeof(value),
                    caml_mutex_finalize, 1, Max_mutex_number);
  Mutex_val(mut) = CreateMutex(0, FALSE, NULL);
  if (Mutex_val(mut) == NULL) caml_wthread_error("Mutex.create");
  return mut;
}

value caml_mutex_lock(value mut)           /* ML */
{
  int retcode;
  enter_blocking_section();
  retcode = WaitForSingleObject(Mutex_val(mut), INFINITE);
  leave_blocking_section();
  if (retcode == WAIT_FAILED) caml_wthread_error("Mutex.lock");
  return Val_unit;
}

value caml_mutex_unlock(value mut)           /* ML */
{
  BOOL retcode;
  enter_blocking_section();
  retcode = ReleaseMutex(Mutex_val(mut));
  leave_blocking_section();
  if (!retcode) caml_wthread_error("Mutex.unlock");
  return Val_unit;
}

value caml_mutex_try_lock(value mut)           /* ML */
{
  int retcode;
  retcode = WaitForSingleObject(Mutex_val(mut), 0);
  if (retcode == WAIT_FAILED || retcode == WAIT_ABANDONED)
    caml_wthread_error("Mutex.try_lock");
  return Val_bool(retcode == WAIT_OBJECT_0);
}

/* Delay */

value caml_thread_delay(value val)        /* ML */
{
  enter_blocking_section();
  Sleep((DWORD)(Double_val(val)*1000)); /* milliseconds */
  leave_blocking_section();
  return Val_unit;
}

/* Conditions operations */

struct caml_condvar {
  void (*final_fun)();          /* Finalization function */
  unsigned long count;          /* Number of waiting threads */
  HANDLE sem;                   /* Semaphore on which threads are waiting */
};

#define Condition_val(v) ((struct caml_condvar *)(v))
#define Max_condition_number 1000

static void caml_condition_finalize(value cond)
{
  CloseHandle(Condition_val(cond)->sem);
}

value caml_condition_new(value unit)        /* ML */
{
  value cond;
  cond = alloc_final(sizeof(struct caml_condvar) / sizeof(value),
                     caml_condition_finalize, 1, Max_condition_number);
  Condition_val(cond)->sem = CreateSemaphore(NULL, 0, 0x7FFFFFFF, NULL);
  if (Condition_val(cond)->sem == NULL)
    caml_wthread_error("Condition.create");
  Condition_val(cond)->count = 0;
  return cond;
}

value caml_condition_wait(value cond, value mut)           /* ML */
{
  int retcode;
  HANDLE m = Mutex_val(mut);
  HANDLE s = Condition_val(cond)->sem;
  HANDLE handles[2];

  Condition_val(cond)->count ++;
  enter_blocking_section();
  /* Release mutex */
  ReleaseMutex(m);
  /* Wait for semaphore to be non-null, and decrement it.
     Simultaneously, re-acquire mutex. */
  handles[0] = s;
  handles[1] = m;
  retcode = WaitForMultipleObjects(2, handles, TRUE, INFINITE);
  leave_blocking_section();
  if (retcode == WAIT_FAILED) caml_wthread_error("Condition.wait");
  return Val_unit;
}

value caml_condition_signal(value cond)           /* ML */
{
  HANDLE s = Condition_val(cond)->sem;

  if (Condition_val(cond)->count > 0) {
    Condition_val(cond)->count --;
    enter_blocking_section();
    /* Increment semaphore by 1, waking up one waiter */
    ReleaseSemaphore(s, 1, NULL);
    leave_blocking_section();
  }
  return Val_unit;
}

value caml_condition_broadcast(value cond)           /* ML */
{
  HANDLE s = Condition_val(cond)->sem;
  unsigned long c = Condition_val(cond)->count;

  if (c > 0) {
    Condition_val(cond)->count = 0;
    enter_blocking_section();
    /* Increment semaphore by c, waking up all waiters */
    ReleaseSemaphore(s, c, NULL);
    leave_blocking_section();
  }
  return Val_unit;
}

/* Error report */

static void caml_wthread_error(char * msg)
{
  char errmsg[1024];
  sprintf(errmsg, "%s: error code %x\n", msg, GetLastError());
  raise_sys_error(copy_string(errmsg));
}
