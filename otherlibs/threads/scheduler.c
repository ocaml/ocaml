#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "config.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "fail.h"
#include "io.h"
#include "roots.h"

#if defined(HAS_SELECT) && defined(HAS_SETITIMER) && defined(HAS_GETTIMEOFDAY)
#else
#include "Cannot compile libthreads, system calls missing"
#endif

/* Configuration */

/* Initial size of stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)

/* Max computation time before rescheduling, in microseconds (100ms) */
#define Thread_timeout 100000

/* The thread descriptors */

struct thread_struct {
  struct thread_struct * next;  /* Double linking of threads */
  struct thread_struct * prev;
  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;
  value * trapsp;
  int runnable;                 /* 1 if runnable, 0 if stopped */
  int fd;               /* File descriptor on which this thread is waiting */
  double delay;                 /* Time until which this thread is blocked */
};

typedef struct thread_struct * thread_t;

#define NO_FD (-1)
#define NO_DELAY 1E20           /* +infty, for all purposes */

thread_t curr_thread = NULL;    /* The thread currently active */
int num_waiting_on_fd;          /* Number of threads waiting on file descrs. */
int num_waiting_on_timer;       /* Number of threads waiting on the timer */

/* Scan the stacks of the other threads */

static void (*prev_scan_roots_hook) P((scanning_action));

static void thread_scan_roots(action)
     scanning_action action;
{
  thread_t th;
  register value * sp;
  /* Don't scan curr_thread->sp, this has already been done */
  for (th = curr_thread->next; th != curr_thread; th = th->next) {
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
  curr_thread = (thread_t) stat_alloc(sizeof(struct thread_struct));
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;
  curr_thread->runnable = 1;
  curr_thread->fd = NO_FD;
  curr_thread->delay = NO_DELAY;
  /* Initialize scheduling */
  num_waiting_on_fd = 0;
  num_waiting_on_timer = 0;
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
  th = (thread_t) stat_alloc(sizeof(struct thread_struct));
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
  th->runnable = 1;
  th->fd = NO_FD;
  th->delay = NO_DELAY;
  /* Insert thread in doubly linked list of threads */
  th->prev = curr_thread->prev;
  curr_thread->prev->next = th;
  th->next = curr_thread;
  curr_thread->prev = th;
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
  fd_set readfds;
  struct timeval delay_tv, * delay_ptr;
  double delay, now;
  int retcode;

  /* Save the status of the current thread */
  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->trapsp = trapsp;

  /* Find the next runnable thread */
  run_thread = NULL;
  FOREACH_THREAD(th)
    if (th->runnable) { run_thread = th; break; }
  END_FOREACH(th);

  /* If some threads are blocked on I/O,  get ready to select on all
     descriptors they are waiting for */
  FD_ZERO(&readfds);
  if (num_waiting_on_fd > 0) {
    FOREACH_THREAD(th)
      if (th->fd != NO_FD) FD_SET(th->fd, &readfds);
    END_FOREACH(th);
  }
  /* If some threads are blocked on the timer, activate those for which the
     time has elapsed, and compute the minimal delay until one gets
     ready. */
  delay = NO_DELAY;
  if (num_waiting_on_timer > 0) {
    now = timeofday();
    FOREACH_THREAD(th)
      if (th->delay < now) {
          th->runnable = 1;
          th->delay = NO_DELAY;
          num_waiting_on_timer--;
          if (run_thread == NULL) run_thread = th; /* Found a runnable one. */
      } else {
        if (th->delay <= delay) delay = th->delay;
      }
    END_FOREACH(th);
    delay -= now;
  }
  /* Do the select if needed */
  if (num_waiting_on_fd > 0 ||
      (run_thread == NULL && num_waiting_on_timer > 0)) {
    /* If a thread is runnable, just poll */
    if (run_thread != NULL) delay = 0.0;
    /* Convert delay to a timeval */
    if (delay == NO_DELAY)
      delay_ptr = NULL;
    else {
      delay_tv.tv_sec = (unsigned int) delay;
      delay_tv.tv_usec = (delay - (double) delay_tv.tv_sec) * 1e6;
      delay_ptr = &delay_tv;
    }
    retcode = select(FD_SETSIZE, &readfds, NULL, NULL, delay_ptr);
    if (retcode > 0) {
      /* Some descriptors are ready. 
         Make the corresponding threads runnable. */
      FOREACH_THREAD(th)
        if (th->fd != NO_FD && FD_ISSET(th->fd, &readfds)) {
          FD_CLR(th->fd, &readfds); /* Wake up only one thread per fd. */
          th->runnable = 1;
          th->fd = NO_FD;
          num_waiting_on_fd--;
          if (run_thread == NULL) run_thread = th; /* Found a runnable one. */
        }
      END_FOREACH(th);
    }
    if (delay != 0.0 && delay != NO_DELAY) {
      /* The delays for some of the threads should have expired.
         Make these threads runnable. */
      now = timeofday();
      FOREACH_THREAD(th)
        if (th->delay <= now) {
          th->runnable = 1;
          th->delay = NO_DELAY;
          num_waiting_on_timer--;
          if (run_thread == NULL) run_thread = th; /* Found a runnable one. */
        }
      END_FOREACH(th);
    }
  }
  /* If we haven't something to run at that point, we're in big trouble. */
  if (run_thread == NULL) fatal_error("Deadlock.");

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
  curr_thread->runnable = 0;
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread on a Unix file descriptor */

value thread_wait_descr(fd)        /* ML */
     value fd;
{
  curr_thread->runnable = 0;
  curr_thread->fd = Int_val(fd);
  num_waiting_on_fd++;
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread on a buffered input channel */

value thread_wait_inchan(vchan)       /* ML */
     value vchan;
{
  struct channel * chan = (struct channel *) vchan;
  if (chan->curr < chan->max) return Val_unit;
  curr_thread->runnable = 0;
  curr_thread->fd = chan->fd;
  num_waiting_on_fd++;
  schedule_thread();
  return Val_unit;
}

/* Suspend the current thread for some time */

value thread_wait_for(time)          /* ML */
     value time;
{
  double now = timeofday();
  curr_thread->runnable = 0;
  curr_thread->delay = now + Double_val(time);
  num_waiting_on_timer++;
  schedule_thread();
  return Val_unit;
}

/* Reactivate another thread */

value thread_wakeup(thread)     /* ML */
     value thread;
{
  thread_t th = (thread_t) thread;
  /* The thread is no longer waiting on I/O or timer. */
  if (th->fd != NO_FD) { th->fd = NO_FD; num_waiting_on_fd--; }
  if (th->delay != NO_DELAY) { th->delay = NO_DELAY; num_waiting_on_timer--; }
  th->runnable = 1;
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
  if (th->fd != NO_FD) { th->fd = NO_FD; num_waiting_on_fd--; }
  if (th->delay != NO_DELAY) { th->delay = NO_DELAY; num_waiting_on_timer--; }
  th->runnable = 0;
  /* If this is the current thread, activate another one */
  if (th == curr_thread) schedule_thread();
  /* Remove thread from the doubly-linked list */
  th->prev->next = th->next;
  th->next->prev = th->prev;
  /* Free its resources */
  stat_free((char *) th->stack_low);
  stat_free((char *) th);
  return Val_unit;
}

