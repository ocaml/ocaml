/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt  */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Thread interface for Win32 threads */

#include <windows.h>
#include <stdio.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#include <winsock.h>

/* Assert with systematic evaluation.*/
#ifdef DEBUG
#define AssertEv(x) Assert(x)
#else
#define AssertEv(x) x
#endif

/* Signal used for timer preemption (any unused signal number) */
#define SIGTIMER 1

/* Max computation time before rescheduling, in milliseconds (50ms) */
#define Thread_timeout 50

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* The thread descriptors */

struct win32_thread_struct {
  value final_fun;              /* Finalization function */
  HANDLE thread;                /* Win32 thread handle */
  HANDLE wakeup_event;          /* Win32 event for sleep/wakeup */
};

struct csl_thread_struct {
  struct win32_thread_struct * win32;
  value ident;                  /* Unique id */
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  value * trapsp;               /* Saved value of trapsp for this thread */
  struct longjmp_buffer * external_raise; /* Saved value of external_raise */
  value * local_roots;          /* Saved value of local_roots */
  struct csl_thread_struct * next;  /* Double linking of threads */
  struct csl_thread_struct * prev;
};

typedef struct csl_thread_struct * csl_thread_t;

#define Assign(dst,src) modify((value *)&(dst), (value)(src))

/* The global mutex used to ensure that at most one thread is running
   Caml code */
HANDLE csl_mutex;

/* Head of the list of thread descriptors */
csl_thread_t thread_list = NULL;

/* Thread-specific variable holding the thread descriptor for the current
   thread. */
__declspec( thread ) csl_thread_t curr_thread;

/* Identifier for next thread creation */
static long thread_next_ident = 0;

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) P((scanning_action)); 

static void csl_thread_scan_roots(action)
     scanning_action action;
{
  csl_thread_t th;
  register value * sp;
  value * block;

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
    /* Scan local C roots for that thread */
    for (block = th->local_roots; block != NULL; block = (value *) block [1]) {
      for (sp = block - (long) block [0]; sp < block; sp++) {
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

static void csl_thread_enter_blocking_section()
{
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();
  /* Save the stack-related global variables in the thread descriptor
     of the current thread */
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->external_raise = external_raise;
  curr_thread->local_roots = local_roots;
  /* Release the global mutex */
  AssertEv(ReleaseMutex(csl_mutex));
}


static void csl_thread_leave_blocking_section()
{
  /* Re-acquire the global mutex */
  AssertEv(WaitForSingleObject(csl_mutex, INFINITE) == WAIT_OBJECT_0);
  /* Restore the stack-related global variables */
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
  external_raise = curr_thread->external_raise;
  local_roots = curr_thread->local_roots;
  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
}

/* The "tick" thread fakes a SIGTIMER signal at regular intervals. */

static void* csl_thread_tick()
{
  while(1) {
    Sleep(Thread_timeout);
    pending_signal = SIGTIMER;
    something_to_do = 1;
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
    thread_list->prev = NULL; 
  } else {
    Assign(th->next->prev, th->prev);
    Assign(th->prev->next, th->next);
  }
  /* Free the memory resources */
  stat_free((char *) th->stack_low);
  /* Don't leave dangling pointers into possibly freed blocks,
     this may confuse the GC if thsoe blocks are later added to
     the heap. */
  th->stack_low = NULL;
  th->stack_high = NULL;
  th->stack_threshold = NULL;
  th->sp = NULL;
  th->trapsp = NULL;
  th->external_raise = NULL;
  th->local_roots = NULL;
}

static void csl_thread_finalize(vfin)
     value vfin;
{
  struct win32_thread_struct * win32 = (struct win32_thread_struct *) vfin;
  AssertEv(CloseHandle(win32->thread));
  AssertEv(CloseHandle(win32->wakeup_event));
}

/* Allocate a new thread descriptor */

#define Max_thread_number 100

static csl_thread_t csl_alloc_thread()
{
  csl_thread_t th;
  Push_roots(root, 1);

  root[0] =
    alloc_final(sizeof(struct win32_thread_struct) / sizeof(value),
                csl_thread_finalize, 1, Max_thread_number);
  th = (csl_thread_t)
    alloc_shr(sizeof(struct csl_thread_struct) / sizeof(value), 0);
  th->win32 = (struct win32_thread_struct *) root[0];
  th->win32->wakeup_event = CreateEvent(NULL, FALSE, FALSE, NULL);
  th->ident = Val_long(thread_next_ident);
  thread_next_ident++;
  th->next = NULL;
  th->prev = NULL;
  return th;
}

/* Initialize the thread machinery */

value csl_thread_initialize(unit)   /* ML */
     value unit;
{
  unsigned long th_id;
  HANDLE tick_thread;

  /* Initialize the master mutex */
  csl_mutex = CreateMutex(NULL, TRUE, NULL);
  if (csl_mutex == NULL) sys_error("Thread.init");
  /* Build a descriptor for the initial thread */
  thread_list = csl_alloc_thread();
  DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                  GetCurrentProcess(), &(thread_list->win32->thread),
                  0, FALSE, DUPLICATE_SAME_ACCESS);
  if (thread_list->win32->thread == NULL ||
      thread_list->win32->wakeup_event == NULL) sys_error("Thread.init");
  /* The stack-related fields will be filled in at the next
     enter_blocking_section */
  /* Associate the thread descriptor with the current thread */
  curr_thread = thread_list;
  /* Set up the hooks */
  prev_scan_roots_hook = scan_roots_hook;
  scan_roots_hook = csl_thread_scan_roots;
  prev_enter_blocking_section_hook = enter_blocking_section_hook;
  enter_blocking_section_hook = csl_thread_enter_blocking_section;
  prev_leave_blocking_section_hook = leave_blocking_section_hook;
  leave_blocking_section_hook = csl_thread_leave_blocking_section;
  /* Fork the tick thread */
  tick_thread =
    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)&csl_thread_tick,
                 NULL, 0, &th_id);
  if (tick_thread == NULL) sys_error("Thread.init");
  AssertEv(CloseHandle(tick_thread));
  Pop_roots();
  return Val_unit;
}

/* Create a thread */

static void csl_thread_start(th)
     csl_thread_t th;
{
  value clos;
  /* Associate the thread descriptor with the thread */
  curr_thread = th;

  /* Acquire the global mutex before running the thread */
  AssertEv(WaitForSingleObject(csl_mutex,INFINITE) == WAIT_OBJECT_0);  
  /* Set up the stack variables */
  stack_low = th->stack_low;
  stack_high = th->stack_high;
  stack_threshold = th->stack_threshold;
  extern_sp = th->sp;
  trapsp = th->trapsp;
  external_raise = th->external_raise;
  local_roots = th->local_roots;
  /* Callback the closure */
  clos = *extern_sp++;
  callback(clos, Val_unit);
  /* Cleanup: free the thread resources */
  csl_thread_cleanup(th);
  /* Release the mutex and die quietly */
  ReleaseMutex(csl_mutex);
}  

value csl_thread_new(clos)          /* ML */
     value clos;
{
  csl_thread_t th;
  unsigned long th_id;
  Push_roots(root, 1);

  root[0] = clos;
  /* Allocate the thread and its stack */
  th = csl_alloc_thread();
  th->stack_low = (value *) stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  th->external_raise = NULL;
  th->local_roots = NULL;
  /* Add it to the list of threads */
  th->next = thread_list;
  Assign(thread_list->prev, th);
  thread_list = th;
  /* Pass the closure in the newly created stack, so that it will be
     preserved by garbage collection */
  *--(th->sp) = root[0];
  /* Fork the new thread */
  th->win32->thread =
    CreateThread(NULL,0, (LPTHREAD_START_ROUTINE) csl_thread_start,
                 (void *) th, 0, &th_id);
  if (th->win32->thread == NULL || th->win32->wakeup_event == NULL)
    sys_error("Thread.new");
  Pop_roots();
  return (value) th;
}

/* Return the current thread */

value csl_thread_self(unit)         /* ML */
     value unit;
{
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
  Sleep(0);
  leave_blocking_section();
  return Val_unit;
}

/* Detach a thread */

value csl_thread_detach(th)         /* ML */
     csl_thread_t th;
{
  if (CloseHandle(th->win32->thread) == 0) sys_error("Thread.detach");
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value csl_thread_join(th)          /* ML */
     csl_thread_t th;
{
  int retcode;
  enter_blocking_section();
  retcode = WaitForSingleObject(th->win32->thread, INFINITE);
  leave_blocking_section();
  if (retcode == WAIT_FAILED) sys_error("Thread.join");
  return Val_unit;
}

/* Terminate the current thread */

value csl_thread_exit(unit)       /* ML */
     value unit;
{
  csl_thread_cleanup(curr_thread);
  enter_blocking_section();
  ExitThread(0);
  return Val_unit;              /* never reached */
}

value csl_thread_kill(th)
        csl_thread_t th;
{
  csl_thread_cleanup(th);
  if (TerminateThread(th->win32->thread, 1) == 0) sys_error("Thread.kill");
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (*((HANDLE *)(&Field(v, 1))))
#define Max_mutex_number 1000

static void csl_mutex_finalize(mut)
     value mut;
{
  AssertEv(CloseHandle(Mutex_val(mut)));
}

value csl_mutex_new(unit)        /* ML */
     value unit;
{
  value mut;
  mut = alloc_final(1 + sizeof(HANDLE) / sizeof(value),
                    csl_mutex_finalize, 1, Max_mutex_number);
  Mutex_val(mut) = CreateMutex(0, FALSE, NULL);
  if (Mutex_val(mut) == NULL) sys_error("Mutex.new");
  return mut;
}

value csl_mutex_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  enter_blocking_section();
  retcode = WaitForSingleObject(Mutex_val(mut), INFINITE);
  leave_blocking_section();
  if (retcode == WAIT_FAILED) sys_error("Mutex.lock");
  return Val_unit;
}

value csl_mutex_unlock(mut)           /* ML */
     value mut;
{
  BOOL retcode;
  enter_blocking_section();
  retcode = ReleaseMutex(Mutex_val(mut));
  leave_blocking_section();
  if (!retcode) sys_error("Mutex.unlock");
  return Val_unit;
}

value csl_mutex_try_lock(mut)           /* ML */
     value mut;
{
  int retcode;
  retcode = WaitForSingleObject(Mutex_val(mut), 0);
  if (retcode == WAIT_FAILED || retcode == WAIT_ABANDONED)
    sys_error("Mutex.try_lock");
  return Val_bool(retcode == WAIT_OBJECT_0);
}

/* Delay */

value csl_thread_delay(val)        /* ML */
     value val;
{
  enter_blocking_section();
  Sleep((DWORD)(Double_val(val)*1000)); /* milliseconds */
  leave_blocking_section();
  return Val_unit;
}

/* Sleep and wakeup */

value csl_thread_sleep(value unit) /* ML */
{
  enter_blocking_section();
  AssertEv(WaitForSingleObject(curr_thread->win32->wakeup_event, INFINITE) ==
           WAIT_OBJECT_0);
  leave_blocking_section();
  return Val_unit;
}

value csl_thread_wakeup(csl_thread_t th) /* ML */
{
  AssertEv(SetEvent(th->win32->wakeup_event));
  return Val_unit;
}
