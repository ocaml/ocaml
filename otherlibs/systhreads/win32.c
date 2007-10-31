/***********************************************************************/
/*                                                                     */
/*                         Objective Caml                              */
/*                                                                     */
/*           Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for Win32 threads */

#include <windows.h>
#include <process.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
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

/* Max computation time before rescheduling, in milliseconds (50ms) */
#define Thread_timeout 50

/* Signal used for timer preemption (any unused, legal signal number) */
#define SIGTIMER SIGTERM

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
  value descr;                  /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
#ifdef NATIVE_CODE
  char * bottom_of_stack;       /* Saved value of caml_bottom_of_stack */
  uintnat last_retaddr;         /* Saved value of caml_last_return_address */
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
  int backtrace_pos;            /* Saved backtrace_pos */
  code_t * backtrace_buffer;    /* Saved backtrace_buffer */
  value backtrace_last_exn;     /* Saved backtrace_last_exn (root) */
#endif
};

typedef struct caml_thread_struct * caml_thread_t;

/* The descriptor for the currently executing thread (thread-specific) */

static caml_thread_t curr_thread = NULL;

/* The global mutex used to ensure that at most one thread is running
   Caml code */
static HANDLE caml_mutex;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding Posix thread. */
static DWORD thread_descriptor_key;

/* The key used for unlocking I/O channels on exceptions */
static DWORD last_channel_locked_key;

/* Identifier for next thread creation */
static intnat thread_next_ident = 0;

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
#ifndef NATIVE_CODE
    (*action)(th->backtrace_last_exn, &th->backtrace_last_exn);
#endif
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
  curr_thread->backtrace_pos = backtrace_pos;
  curr_thread->backtrace_buffer = backtrace_buffer;
  curr_thread->backtrace_last_exn = backtrace_last_exn;
#endif
  /* Release the global mutex */
  ReleaseMutex(caml_mutex);
}

static void caml_thread_leave_blocking_section(void)
{
  WaitForSingleObject(caml_mutex, INFINITE);
  /* Update curr_thread to point to the thread descriptor corresponding
     to the thread currently executing */
  curr_thread = TlsGetValue(thread_descriptor_key);
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
  backtrace_pos = curr_thread->backtrace_pos;
  backtrace_buffer = curr_thread->backtrace_buffer;
  backtrace_last_exn = curr_thread->backtrace_last_exn;
#endif
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
  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (WaitForSingleObject((HANDLE) chan->mutex, 0) == WAIT_OBJECT_0) {
    TlsSetValue(last_channel_locked_key, (void *) chan);
    return;
  }
  enter_blocking_section();
  WaitForSingleObject((HANDLE) chan->mutex, INFINITE);
  /* Problem: if a signal occurs at this point,
     and the signal handler raises an exception, we will not
     unlock the mutex.  The alternative (doing the setspecific
     before locking the mutex is also incorrect, since we could
     then unlock a mutex that is unlocked or locked by someone else. */
  TlsSetValue(last_channel_locked_key, (void *) chan);
  leave_blocking_section();
}

static void caml_io_mutex_unlock(struct channel * chan)
{
  ReleaseMutex((HANDLE) chan->mutex);
  TlsSetValue(last_channel_locked_key, NULL);
}

static void caml_io_mutex_unlock_exn(void)
{
  struct channel * chan = TlsGetValue(last_channel_locked_key);
  if (chan != NULL) caml_io_mutex_unlock(chan);
}

/* The "tick" thread fakes a signal at regular intervals. */

static DWORD WINAPI caml_thread_tick(void * arg)
{
  while(1) {
    Sleep(Thread_timeout);
    caml_pending_signals[SIGTIMER] = 1;
    caml_signals_are_pending = 1;
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

CAMLprim value caml_thread_initialize(value unit)
{
  value vthread = Val_unit;
  value descr;
  HANDLE tick_thread;
  DWORD th_id;

  /* Protect against repeated initialization (PR#1325) */
  if (curr_thread != NULL) return Val_unit;
  Begin_root (vthread);
    /* Initialize the main mutex and acquire it */
    caml_mutex = CreateMutex(NULL, TRUE, NULL);
    if (caml_mutex == NULL) caml_wthread_error("Thread.init");
    /* Initialize the TLS keys */
    thread_descriptor_key = TlsAlloc();
    last_channel_locked_key = TlsAlloc();
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
    /* Associate the thread descriptor with the thread */
    TlsSetValue(thread_descriptor_key, (void *) curr_thread);
    /* Set up the hooks */
    prev_scan_roots_hook = scan_roots_hook;
    scan_roots_hook = caml_thread_scan_roots;
    enter_blocking_section_hook = caml_thread_enter_blocking_section;
    leave_blocking_section_hook = caml_thread_leave_blocking_section;
    try_leave_blocking_section_hook = caml_thread_try_leave_blocking_section;
    caml_channel_mutex_free = caml_io_mutex_free;
    caml_channel_mutex_lock = caml_io_mutex_lock;
    caml_channel_mutex_unlock = caml_io_mutex_unlock;
    caml_channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;
    /* Fork the tick thread */
    tick_thread = CreateThread(NULL, 0, caml_thread_tick, NULL, 0, &th_id);
    if (tick_thread == NULL) caml_wthread_error("Thread.init");
    CloseHandle(tick_thread);
  End_roots();
  return Val_unit;
}

/* Create a thread */

static DWORD WINAPI caml_thread_start(void * arg)
{
  caml_thread_t th = (caml_thread_t) arg;
  value clos;

  /* Associate the thread descriptor with the thread */
  TlsSetValue(thread_descriptor_key, (void *) th);
  TlsSetValue(last_channel_locked_key, NULL);
  /* Acquire the global mutex and set up the stack variables */
  leave_blocking_section();
  /* Callback the closure */
  clos = Start_closure(th->descr);
  modify(&(Start_closure(th->descr)), Val_unit);
  callback_exn(clos, Val_unit);
  /* Remove th from the doubly-linked list of threads */
  th->next->prev = th->prev;
  th->prev->next = th->next;
  /* Release the main mutex (forever) */
  ReleaseMutex(caml_mutex);
#ifndef NATIVE_CODE
  /* Free the memory resources */
  stat_free(th->stack_low);
  if (th->backtrace_buffer != NULL) free(th->backtrace_buffer);
#endif
  /* Free the thread descriptor */
  stat_free(th);
  /* The thread now stops running */
  return 0;
}

CAMLprim value caml_thread_new(value clos)
{
  caml_thread_t th;
  value vthread = Val_unit;
  value descr;
  DWORD th_id;

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
    th->backtrace_pos = 0;
    th->backtrace_buffer = NULL;
    th->backtrace_last_exn = Val_unit;
#endif
    /* Add thread info block to the list of threads */
    th->next = curr_thread->next;
    th->prev = curr_thread;
    curr_thread->next->prev = th;
    curr_thread->next = th;
    /* Fork the new thread */
    th->wthread =
      CreateThread(NULL, 0, caml_thread_start, (void *) th, 0, &th_id);
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

CAMLprim value caml_thread_self(value unit)
{
  if (curr_thread == NULL) invalid_argument("Thread.self: not initialized");
  return curr_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)
{
  char * msg = format_caml_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(curr_thread->descr)), msg);
  free(msg);
#ifndef NATIVE_CODE
  if (backtrace_active) print_exception_backtrace();
#endif
  fflush(stderr);
  return Val_unit;
}

/* Allow re-scheduling */

CAMLprim value caml_thread_yield(value unit)
{
  enter_blocking_section();
  Sleep(0);
  leave_blocking_section();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)
{
  HANDLE h;

  Begin_root(th)                /* prevent deallocation of handle */
    h = Threadhandle(th)->handle;
    enter_blocking_section();
    WaitForSingleObject(h, INFINITE);
    leave_blocking_section();
  End_roots();
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (*((HANDLE *) Data_custom_val(v)))
#define Max_mutex_number 1000

static void caml_mutex_finalize(value mut)
{
  CloseHandle(Mutex_val(mut));
}

static int caml_mutex_compare(value wrapper1, value wrapper2)
{
  HANDLE h1 = Mutex_val(wrapper1);
  HANDLE h2 = Mutex_val(wrapper2);
  return h1 == h2 ? 0 : h1 < h2 ? -1 : 1;
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_mutex_new(value unit)
{
  value mut;
  mut = alloc_custom(&caml_mutex_ops, sizeof(HANDLE), 1, Max_mutex_number);
  Mutex_val(mut) = CreateMutex(0, FALSE, NULL);
  if (Mutex_val(mut) == NULL) caml_wthread_error("Mutex.create");
  return mut;
}

CAMLprim value caml_mutex_lock(value mut)
{
  int retcode;
  /* PR#4351: first try to acquire mutex without releasing the master lock */
  retcode =  WaitForSingleObject(Mutex_val(mut), 0);
  if (retcode == WAIT_OBJECT_0) return Val_unit;
  Begin_root(mut)               /* prevent deallocation of mutex */
    enter_blocking_section();
    retcode = WaitForSingleObject(Mutex_val(mut), INFINITE);
    leave_blocking_section();
  End_roots();
  if (retcode == WAIT_FAILED) caml_wthread_error("Mutex.lock");
  return Val_unit;
}

CAMLprim value caml_mutex_unlock(value mut)
{
  BOOL retcode;
  /* PR#4351: no need to release and reacquire master lock */
  retcode = ReleaseMutex(Mutex_val(mut));
  if (!retcode) caml_wthread_error("Mutex.unlock");
  return Val_unit;
}

CAMLprim value caml_mutex_try_lock(value mut)
{
  int retcode;
  retcode = WaitForSingleObject(Mutex_val(mut), 0);
  if (retcode == WAIT_FAILED || retcode == WAIT_ABANDONED)
    caml_wthread_error("Mutex.try_lock");
  return Val_bool(retcode == WAIT_OBJECT_0);
}

/* Delay */

CAMLprim value caml_thread_delay(value val)
{
  enter_blocking_section();
  Sleep((DWORD)(Double_val(val)*1000)); /* milliseconds */
  leave_blocking_section();
  return Val_unit;
}

/* Conditions operations */

struct caml_condvar {
  uintnat count;          /* Number of waiting threads */
  HANDLE sem;                   /* Semaphore on which threads are waiting */
};

#define Condition_val(v) ((struct caml_condvar *) Data_custom_val(v))
#define Max_condition_number 1000

static void caml_condition_finalize(value cond)
{
  CloseHandle(Condition_val(cond)->sem);
}

static int caml_condition_compare(value wrapper1, value wrapper2)
{
  HANDLE h1 = Condition_val(wrapper1)->sem;
  HANDLE h2 = Condition_val(wrapper2)->sem;
  return h1 == h2 ? 0 : h1 < h2 ? -1 : 1;
}

static struct custom_operations caml_condition_ops = {
  "_condition",
  caml_condition_finalize,
  caml_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_condition_new(value unit)
{
  value cond;
  cond = alloc_custom(&caml_condition_ops, sizeof(struct caml_condvar),
                      1, Max_condition_number);
  Condition_val(cond)->sem = CreateSemaphore(NULL, 0, 0x7FFFFFFF, NULL);
  if (Condition_val(cond)->sem == NULL)
    caml_wthread_error("Condition.create");
  Condition_val(cond)->count = 0;
  return cond;
}

CAMLprim value caml_condition_wait(value cond, value mut)
{
  int retcode;
  HANDLE m = Mutex_val(mut);
  HANDLE s = Condition_val(cond)->sem;
  HANDLE handles[2];

  Condition_val(cond)->count ++;
  Begin_roots2(cond, mut)       /* prevent deallocation of cond and mutex */
    enter_blocking_section();
    /* Release mutex */
    ReleaseMutex(m);
    /* Wait for semaphore to be non-null, and decrement it.
       Simultaneously, re-acquire mutex. */
    handles[0] = s;
    handles[1] = m;
    retcode = WaitForMultipleObjects(2, handles, TRUE, INFINITE);
    leave_blocking_section();
  End_roots();
  if (retcode == WAIT_FAILED) caml_wthread_error("Condition.wait");
  return Val_unit;
}

CAMLprim value caml_condition_signal(value cond)
{
  HANDLE s = Condition_val(cond)->sem;

  if (Condition_val(cond)->count > 0) {
    Condition_val(cond)->count --;
    /* Increment semaphore by 1, waking up one waiter */
    ReleaseSemaphore(s, 1, NULL);
  }
  return Val_unit;
}

CAMLprim value caml_condition_broadcast(value cond)
{
  HANDLE s = Condition_val(cond)->sem;
  uintnat c = Condition_val(cond)->count;

  if (c > 0) {
    Condition_val(cond)->count = 0;
    /* Increment semaphore by c, waking up all waiters */
    ReleaseSemaphore(s, c, NULL);
  }
  return Val_unit;
}

/* Error report */

static void caml_wthread_error(char * msg)
{
  char errmsg[1024];
  sprintf(errmsg, "%s: error code %lx", msg, GetLastError());
  raise_sys_error(copy_string(errmsg));
}
