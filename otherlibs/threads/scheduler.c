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
#include <sys/wait.h>
#include <fcntl.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
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
  value fd;             /* File descriptor on which this thread is waiting */
  value delay;                  /* Time until which this thread is blocked */
  value joining;                /* Thread we're trying to join */
  value waitpid;                /* PID of process we're waiting for */
  value retval;                 /* Value to return when thread resumes */
};

typedef struct thread_struct * thread_t;

#define RUNNABLE Val_int(0)
#define KILLED Val_int(1)
#define SUSPENDED Val_int(2)
#define BLOCKED_READ Val_int(4)
#define BLOCKED_WRITE Val_int(8)
#define BLOCKED_DELAY Val_int(16)
#define BLOCKED_JOIN Val_int(32)
#define BLOCKED_WAIT Val_int(64)

#define RESUMED_WAKEUP Val_int(0)
#define RESUMED_IO Val_int(1)
#define RESUMED_DELAY Val_int(2)
#define RESUMED_JOIN Val_int(3)

#define NO_FD Val_int(0)
#define NO_DELAY Val_unit
#define NO_JOINING Val_unit
#define NO_WAITPID Val_int(0)

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
  curr_thread->waitpid = NO_WAITPID;
  curr_thread->retval = Val_unit;
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
  th->waitpid = NO_WAITPID;
  th->retval = Val_unit;
  /* Insert thread in doubly linked list of threads */
  th->prev = curr_thread->prev;
  th->next = curr_thread;
  Assign(curr_thread->prev->next, th);
  Assign(curr_thread->prev, th);
  /* Return thread */
  return (value) th;
}

/* Return the thread identifier */

value thread_id(th)             /* ML */
     value th;
{
  return ((struct thread_struct *)th)->ident;
}

/* Return the current time as a floating-point number */

static double timeofday()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
}

/* Find a runnable thread and activate it */

#define FOREACH_THREAD(x) x = curr_thread; do { x = x->next;
#define END_FOREACH(x) } while (x != curr_thread)
static value alloc_process_status();

static value schedule_thread()
{
  thread_t run_thread, th;
  fd_set readfds, writefds;
  double delay, now;
  int need_select, need_wait;

  /* Save the status of the current thread */
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;

try_again:
  /* Build fdsets and delay for select.
     See if some join or wait operations succeeded. */
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  delay = DELAY_INFTY;
  now = -1.0;
  need_select = 0;
  need_wait = 0;

  FOREACH_THREAD(th)
    if (th->status & (BLOCKED_READ - 1)) {
      FD_SET(Int_val(th->fd), &readfds);
      need_select = 1;
    }
    if (th->status & (BLOCKED_WRITE - 1)) {
      FD_SET(Int_val(th->fd), &writefds);
      need_select = 1;
    }
    if (th->status & (BLOCKED_DELAY - 1)) {
      double th_delay = Double_val(th->delay);
      if (now < 0.0) now = timeofday();
      if (th_delay < now) {
        th->status = RUNNABLE;
        Assign(th->delay, NO_DELAY);
        th->retval = RESUMED_DELAY;
      } else {
        if (th_delay < delay) delay = th_delay;
      }
    }
    if (th->status & (BLOCKED_JOIN - 1)) {
      if (((thread_t)(th->joining))->status == KILLED) {
        th->status = RUNNABLE;
        Assign(th->joining, NO_JOINING);
        th->retval = RESUMED_JOIN;
      }
    }
    if (th->status & (BLOCKED_WAIT - 1)) {
      int status, pid;
      pid = waitpid(Int_val(th->waitpid), &status, WNOHANG);
      if (pid > 0) {
        th->status = RUNNABLE;
        th->waitpid = NO_WAITPID;
        Assign(th->retval, alloc_process_status(pid, status));
      } else {
        need_wait = 1;
      }
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
    /* If a thread is blocked on wait, don't block forever */
    if (run_thread != NULL) {
      delay_tv.tv_sec = 0;
      delay_tv.tv_usec = 0;
      delay_ptr = &delay_tv;
    }
    else if (delay != DELAY_INFTY) {
      delay = delay - now;
      delay_tv.tv_sec = (unsigned int) delay;
      delay_tv.tv_usec = (delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
    }
    else if (need_wait) {
      delay_tv.tv_sec = 0;
      delay_tv.tv_usec = Thread_timeout;
      delay_ptr = &delay_tv;
    } else {
      delay_ptr = NULL;
    }
    retcode = select(FD_SETSIZE, &readfds, &writefds, NULL, delay_ptr);
    if (retcode > 0) {
      /* Some descriptors are ready. 
         Mark the corresponding threads runnable. */
      FOREACH_THREAD(th)
        if (th->status & (BLOCKED_READ - 1)
            && FD_ISSET(Int_val(th->fd), &readfds)) {
          /* Wake up only one thread per fd. */
          FD_CLR(Int_val(th->fd), &readfds);
          th->status = RUNNABLE;
          th->fd = NO_FD;
          th->retval = RESUMED_IO;
          if (run_thread == NULL) run_thread = th; /* Found one. */
        }
        if (th->status & (BLOCKED_WRITE - 1)
            && FD_ISSET(Int_val(th->fd), &writefds)) {
          /* Wake up only one thread per fd. */
          FD_CLR(Int_val(th->fd), &writefds);
          th->status = RUNNABLE;
          th->fd = NO_FD;
          th->retval = RESUMED_IO;
          if (run_thread == NULL) run_thread = th; /* Found one. */
        }
      END_FOREACH(th);
    }
    /* If we get here with run_thread still NULL, some of the delays 
       have expired, or some wait() need to be polled again.
       We go through the loop once more to make the
       corresponding threads runnable. */
    if (run_thread == NULL && (delay != DELAY_INFTY || need_wait))
      goto try_again;
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
  return curr_thread->retval;
}

/* Reschedule without suspending the current thread */

value thread_yield(unit)        /* ML */
     value unit;
{
  curr_thread->retval = Val_unit;
  return schedule_thread();
}

/* Suspend the current thread */

value thread_sleep(unit)        /* ML */
     value unit;
{
  curr_thread->status = SUSPENDED;
  return schedule_thread();
}

/* Suspend the current thread on a Unix file descriptor */

value thread_wait_read(fd)        /* ML */
     value fd;
{
  curr_thread->status = BLOCKED_READ;
  curr_thread->fd = fd;
  return schedule_thread();
}

value thread_wait_write(fd)        /* ML */
     value fd;
{
  curr_thread->status = BLOCKED_WRITE;
  curr_thread->fd = fd;
  return schedule_thread();
}

/* Primitives to implement suspension on buffered channels */

value thread_inchan_ready(chan) /* ML */
     struct channel * chan;
{
  return Val_bool(chan->curr < chan->max);
}

value thread_outchan_ready(chan, vsize) /* ML */
     struct channel * chan;
     value vsize;
{
  long size = Long_val(vsize);
  /* Negative size means we want to flush the buffer entirely */
  if (size < 0) {
    return Val_bool(chan->curr == chan->buff);
  } else {
    int free = chan->end - chan->curr;
    if (chan->curr == chan->buff)
      return Val_bool(size < free);
    else
      return Val_bool(size <= free);
  }
}

/* Suspend the current thread for some time */

value thread_delay(time)          /* ML */
     value time;
{
  double date = timeofday() + Double_val(time);
  curr_thread->status = BLOCKED_DELAY;
  Assign(curr_thread->delay, copy_double(date));
  return schedule_thread();
}

/* Suspend the current thread on a Unix file descriptor, with timeout */

value thread_wait_timed_read(fd, time)        /* ML */
     value fd, time;
{
  double date = timeofday() + Double_val(time);
  curr_thread->status = BLOCKED_READ | BLOCKED_DELAY;
  curr_thread->fd = fd;
  Assign(curr_thread->delay, copy_double(date));
  return schedule_thread();
}

value thread_wait_timed_write(fd, time)        /* ML */
     value fd, time;
{
  double date = timeofday() + Double_val(time);
  curr_thread->status = BLOCKED_WRITE | BLOCKED_DELAY;
  curr_thread->fd = fd;
  Assign(curr_thread->delay, copy_double(date));
  return schedule_thread();
}

/* Suspend the current thread until another thread terminates */

value thread_join(th)          /* ML */
     value th;
{
  if (((thread_t)th)->status == KILLED) return Val_unit;
  curr_thread->status = BLOCKED_JOIN;
  Assign(curr_thread->joining, th);
  return schedule_thread();
}

/* Suspend the current thread until a Unix process exits */

value thread_wait_pid(pid)          /* ML */
     value pid;
{
  curr_thread->status = BLOCKED_WAIT;
  curr_thread->waitpid = pid;
  return schedule_thread();
}

/* Reactivate another thread */

value thread_wakeup(thread)     /* ML */
     value thread;
{
  thread_t th = (thread_t) thread;
  switch (th->status) {
  case SUSPENDED:
    th->status = RUNNABLE;
    th->retval = RESUMED_WAKEUP;
    break;
  case KILLED:
    failwith("Thread.wakeup: killed thread");
  default:
    failwith("Thread.wakeup: thread not suspended");
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
  value retval = Val_unit;
  thread_t th = (thread_t) thread;
  /* Don't paint ourselves in a corner */
  if (th == th->next) failwith("Thread.kill: cannot kill the last thread");
  /* This thread is no longer waiting on anything */
  th->status = KILLED;
  Assign(th->delay, NO_DELAY);
  Assign(th->joining, NO_JOINING);
  /* If this is the current thread, activate another one */
  if (th == curr_thread) retval = schedule_thread();
  /* Remove thread from the doubly-linked list */
  Assign(th->prev->next, th->next);
  Assign(th->next->prev, th->prev);
  /* Free its resources */
  stat_free((char *) th->stack_low);
  th->stack_low = NULL;
  th->stack_high = NULL;
  th->stack_threshold = NULL;
  th->sp = NULL;
  th->trapsp = NULL;
  return retval;
}

/* Auxiliary function for allocating the result of a waitpid() call */

#ifndef WIFEXITED
#define WIFEXITED(status) ((status) & 0xFF == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) ((status) & 0xFF == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

static value alloc_process_status(pid, status)
     int pid, status;
{
  value st, res;
  Push_roots(r, 1);

  if (WIFEXITED(status)) {
    st = alloc(1, 0);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc(1, 2);
    Field(st, 0) = Val_int(WSTOPSIG(status));
  }
  else {
    st = alloc(1, 1);
    Field(st, 0) = Val_int(WTERMSIG(status));
  }
  r[0] = st;
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = r[0];
  Pop_roots();
  return res;
}
