#include "config.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "fail.h"
#include "io.h"
#include "roots.h"
#include "alloc.h"
#include "memory.h"

#if defined(HAS_SELECT) && defined(HAS_SETITIMER) && defined(HAS_GETTIMEOFDAY)
#else
#include "Cannot compile libthreads, system calls missing"
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef FD_ISSET
typedef int fd_set;
#define FD_SETSIZE (sizeof(int) * 8)
#define FD_SET(fd,fds) (*(fds) |= 1 << (fd))
#define FD_CLR(fd,fds) (*(fds) &= ~(1 << (fd)))
#define FD_ISSET(fd,fds) (*(fds) & (1 << (fd)))
#define FD_ZERO(fds) (*(fds) = 0)
#endif

/* Configuration */

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in microseconds (50ms) */
#define Thread_timeout 50000

/* The thread descriptors */

struct thread_struct {
  value ident;                  /* Unique id (for equality comparisons) */
  struct thread_struct * next;  /* Double linking of threads */
  struct thread_struct * prev;
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;
  value * trapsp;
  value status;                 /* RUNNABLE, KILLED. etc (see below) */
  value fd;               /* File descriptor on which this thread is waiting */
  value delay;                  /* Time until which this thread is blocked */
  value joining;                /* Thread we're trying to join */
};

typedef struct thread_struct * thread_t;

#define RUNNABLE Val_int(0)
#define KILLED Val_int(1)
#define SUSPENDED Val_int(2)
#define BLOCKED_READ Val_int(3)
#define BLOCKED_WRITE Val_int(4)
#define BLOCKED_DELAY Val_int(5)
#define BLOCKED_JOIN Val_int(6)

#define NO_FD Val_unit
#define NO_DELAY Val_unit
#define NO_JOINING Val_unit

#define DELAY_INFTY 1E30        /* +infty, for this purpose */

/* The thread currently active */
static thread_t curr_thread = NULL;
/* Identifier for next thread creation */
static value next_ident = Val_int(0);

#define Assign(dst,src) modify((value *)&(dst), (value)(src))

/* Scan the stacks of the other threads */

static void (*prev_scan_roots_hook) P((scanning_action));

static void thread_scan_roots(action)
     scanning_action action;
{
  thread_t th;
  register value * sp;
  /* Scan all active descriptors */
  (*action)((value) curr_thread, (value *) &curr_thread);
  /* Don't scan curr_thread->sp, this has already been done */
  for (th = curr_thread->next; th != curr_thread; th = th->next) {
    (*action)((value) th, (value *) &th);
    for (sp = th->sp; sp < th->stack_high; sp++) {
      (*action)(*sp, sp);
    }
  }
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Initialize the thread machinery */

value thread_initialize(unit)       /* ML */
     value unit;
{
  struct itimerval timer;
  /* Create a descriptor for the current thread */
  curr_thread =
    (thread_t) alloc_shr(sizeof(struct thread_struct) / sizeof(value), 0);
  curr_thread->ident = next_ident;
  next_ident = Val_int(Int_val(next_ident) + 1);
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->status = RUNNABLE;
  curr_thread->fd = NO_FD;
  curr_thread->delay = NO_DELAY;
  curr_thread->joining = NO_JOINING;
  /* Initialize GC */
  prev_scan_roots_hook = scan_roots_hook;
  scan_roots_hook = thread_scan_roots;
  /* Initialize interval timer */
  timer.it_interval.tv_sec = 0;
  timer.it_interval.tv_usec = Thread_timeout;
  timer.it_value = timer.it_interval;
  setitimer(ITIMER_VIRTUAL, &timer, NULL);
  return Val_unit;
}

/* Create a thread */

value thread_new(clos)          /* ML */
     value clos;
{
  thread_t th;
  /* Allocate the thread and its stack */
  Push_roots(r, 1);
  r[0] = clos;
  th = (thread_t) alloc_shr(sizeof(struct thread_struct) / sizeof(value), 0);
  clos = r[0];
  Pop_roots();
  th->ident = next_ident;
  next_ident = Val_int(Int_val(next_ident) + 1);
  th->stack_low = (value *) stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  /* Set up a return frame that pretends we're applying clos to ().
     This way, when this thread is activated, the RETURN will take us
     to the entry point of the closure. */
  th->sp -= 4;
  th->sp[0] = Val_unit;
  th->sp[1] = (value) Code_val(clos);
  th->sp[2] = clos;
  th->sp[3] = Val_long(0);      /* no extra args */
  /* Fake a C call frame */
  th->sp--;
  th->sp[0] = Val_unit;         /* a dummy environment */
  /* The thread is initially runnable */
  th->status = RUNNABLE;
  th->fd = NO_FD;
  th->delay = NO_DELAY;
  th->joining = NO_JOINING;
  /* Insert thread in doubly linked list of threads */
  Assign(th->prev, curr_thread->prev);
  Assign(curr_thread->prev->next, th);
  Assign(th->next, curr_thread);
  Assign(curr_thread->prev, th);
  /* Return thread */
  return (value) th;
}

/* Return the current time as a floating-point number */

double timeofday()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
}

/* Find a runnable thread and activate it */

#define FOREACH_THREAD(x) x = curr_thread; do { x = x->next;
#define END_FOREACH(x) } while (x != curr_thread)

void schedule_thread()
{
  thread_t run_thread, th;
  fd_set readfds, writefds;
  double delay, now;
  int need_select;

  /* Save the status of the current thread */
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;

try_again:
  /* Build fdsets and delay for select.
     See if some join operation succeeded. */
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  delay = DELAY_INFTY;
  now = -1.0;
  need_select = 0;

  FOREACH_THREAD(th)
    switch (th->status) {
    case BLOCKED_READ:
      FD_SET(Int_val(th->fd), &readfds);
      need_select = 1;
      break;
    case BLOCKED_WRITE:
      FD_SET(Int_val(th->fd), &writefds);
      need_select = 1;
      break;
    case BLOCKED_DELAY:
      { double th_delay = Double_val(th->delay);
        if (now < 0.0) now = timeofday();
        if (th_delay < now) {
          th->status = RUNNABLE;
          Assign(th->delay, NO_DELAY);
        } else {
          if (th_delay < delay) delay = th_delay;
        }
        break;
      }
    case BLOCKED_JOIN:
      if (((thread_t)(th->joining))->status == KILLED) {
        th->status = RUNNABLE;
        Assign(th->joining, NO_JOINING);
      }
      break;
    }
  END_FOREACH(th);

  /* Find if a thread is runnable. */
  run_thread = NULL;
  FOREACH_THREAD(th)
    if (th->status == RUNNABLE) { run_thread = th; break; }
  END_FOREACH(th);

  /* Do the select if needed */
  if (need_select || run_thread == NULL) {
    struct timeval delay_tv, * delay_ptr;
    int retcode;
    /* Convert delay to a timeval */
    /* If a thread is runnable, just poll */
    if (run_thread != NULL) {
      delay_tv.tv_sec = 0;
      delay_tv.tv_usec = 0;
      delay_ptr = &delay_tv;
    }
    else if (delay == DELAY_INFTY) {
      delay_ptr = NULL;
    } else {
      delay = delay - now;
      delay_tv.tv_sec = (unsigned int) delay;
      delay_tv.tv_usec = (delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
    }
    retcode = select(FD_SETSIZE, &readfds, &writefds, NULL, delay_ptr);
    if (retcode > 0) {
      /* Some descriptors are ready. 
         Mark the corresponding threads runnable. */
      FOREACH_THREAD(th)
        switch (th->status) {
          case BLOCKED_READ:
            if (FD_ISSET(Int_val(th->fd), &readfds)) {
              /* Wake up only one thread per fd. */
              FD_CLR(Int_val(th->fd), &readfds);
              th->status = RUNNABLE;
              th->fd = NO_FD;
              if (run_thread == NULL) run_thread = th; /* Found one. */
            }
            break;
          case BLOCKED_WRITE:
            if (FD_ISSET(Int_val(th->fd), &writefds)) {
              /* Wake up only one thread per fd. */
              FD_CLR(Int_val(th->fd), &writefds);
              th->status = RUNNABLE;
              th->fd = NO_FD;
              if (run_thread == NULL) run_thread = th; /* Found one. */
            }
            break;
        }
      END_FOREACH(th);
    }
    /* If we get here with run_thread still NULL, some of the delays 
       have expired. We go through the loop once more to make the
       corresponding threads runnable. */
    if (run_thread == NULL && delay != DELAY_INFTY) goto try_again;
  }

  /* If we haven't something to run at that point, we're in big trouble. */
  if (run_thread == NULL) invalid_argument("Thread: deadlock");

  /* Activate the thread */
  curr_thread = run_thread;
  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  trapsp = curr_thread->trapsp;
}

/* Reschedule without suspending the current thread */

value thread_yield(unit)        /* ML */
     value unit;
{
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread */

value thread_sleep(unit)        /* ML */
     value unit;
{
  curr_thread->status = SUSPENDED;
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread on a Unix file descriptor */

value thread_wait_read(fd)        /* ML */
     value fd;
{
  curr_thread->status = BLOCKED_READ;
  curr_thread->fd = fd;
  schedule_thread();
  return Val_unit;
}

value thread_wait_write(fd)        /* ML */
     value fd;
{
  curr_thread->status = BLOCKED_WRITE;
  curr_thread->fd = fd;
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread on a buffered input channel */

value thread_wait_inchan(vchan)       /* ML */
     value vchan;
{
  struct channel * chan = (struct channel *) vchan;
  if (chan->curr < chan->max) return Val_unit;
  curr_thread->status = BLOCKED_READ;
  curr_thread->fd = Val_int(chan->fd);
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread for some time */

value thread_wait_for(time)          /* ML */
     value time;
{
  double date = timeofday() + Double_val(time);
  curr_thread->status = BLOCKED_DELAY;
  Assign(curr_thread->delay, copy_double(date));
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

value thread_join(th)          /* ML */
     value th;
{
  if (((thread_t)th)->status == KILLED) return Val_unit;
  curr_thread->status = BLOCKED_JOIN;
  Assign(curr_thread->joining, th);
  schedule_thread();
  return Val_unit;
}

/* Reactivate another thread */

value thread_wakeup(thread)     /* ML */
     value thread;
{
  thread_t th = (thread_t) thread;
  switch (th->status) {
  case SUSPENDED:
    th->status = RUNNABLE;
    break;
  case KILLED:
    failwith("Thread.wakeup: killed thread");
    break;
  default:
    failwith("Thread.wakeup: thread was not suspended");
    break;
  }
  return Val_unit;
}

/* Return the current thread */

value thread_self(unit)         /* ML */
     value unit;
{
  return (value) curr_thread;
}

/* Kill a thread */

value thread_kill(thread)       /* ML */
     value thread;
{
  thread_t th = (thread_t) thread;
  /* Don't paint ourselves in a corner */
  if (th == th->next) failwith("Thread.kill: cannot kill the last thread");
  /* This thread is no longer waiting on anything */
  th->status = KILLED;
  Assign(th->delay, NO_DELAY);
  Assign(th->joining, NO_JOINING);
  /* If this is the current thread, activate another one */
  if (th == curr_thread) schedule_thread();
  /* Remove thread from the doubly-linked list */
  Assign(th->prev->next, th->next);
  Assign(th->next->prev, th->prev);
  /* Free its resources */
  stat_free((char *) th->stack_low);
  return Val_unit;
}

