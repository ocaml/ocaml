#include <signal.h>
#include "alloc.h"
#include "config.h"
#include "fail.h"
#include "interp.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

static Volatile int async_signal_mode = 0;
Volatile int pending_signal = 0;
value signal_handlers = 0;

static void execute_signal(signal_number)
     int signal_number;
{
  Assert (!async_signal_mode);
  callback(Field(signal_handlers, signal_number), Val_int(signal_number));
}

void handle_signal(signal_number)
     int signal_number;
{
#ifndef BSD_SIGNALS
  signal(signal_number, handle_signal);
#endif
  if (async_signal_mode){
    leave_blocking_section ();
    execute_signal(signal_number);
    enter_blocking_section ();
  }else{
    pending_signal = signal_number;
    something_to_do = 1;
  }
}

void enter_blocking_section()
{
  int temp;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = pending_signal;   pending_signal = 0;
    if (temp) execute_signal(temp);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
}

/* This function may be called from outside a blocking section. */
void leave_blocking_section()
{
  async_signal_mode = 0;
}

#ifndef SIGABRT
#define SIGABRT 0
#endif
#ifndef SIGALRM
#define SIGALRM 0
#endif
#ifndef SIGFPE
#define SIGFPE 0
#endif
#ifndef SIGHUP
#define SIGHUP 0
#endif
#ifndef SIGILL
#define SIGILL 0
#endif
#ifndef SIGINT
#define SIGINT 0
#endif
#ifndef SIGKILL
#define SIGKILL 0
#endif
#ifndef SIGPIPE
#define SIGPIPE 0
#endif
#ifndef SIGQUIT
#define SIGQUIT 0
#endif
#ifndef SIGSEGV
#define SIGSEGV 0
#endif
#ifndef SIGTERM
#define SIGTERM 0
#endif
#ifndef SIGUSR1
#define SIGUSR1 0
#endif
#ifndef SIGUSR2
#define SIGUSR2 0
#endif
#ifndef SIGCHLD
#define SIGCHLD 0
#endif
#ifndef SIGCONT
#define SIGCONT 0
#endif
#ifndef SIGSTOP
#define SIGSTOP 0
#endif
#ifndef SIGTSTP
#define SIGTSTP 0
#endif
#ifndef SIGTTIN
#define SIGTTIN 0
#endif
#ifndef SIGTTOU
#define SIGTTOU 0
#endif

int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU
};

value install_signal_handler(signal_number, action) /* ML */
     value signal_number, action;
{
  int sig = Int_val(signal_number);
  if (sig < 0) {
    sig = posix_signals[-sig-1];
    if (sig == 0) invalid_argument("Sys.signal: unavailable signal");
  }
  switch(Tag_val(action)) {
  case 0:                       /* Signal_default */
    signal(sig, SIG_DFL);
    break;
  case 1:                       /* Signal_ignore */
    signal(sig, SIG_IGN);
    break;
  case 2:                       /* Signal_handle */
    if (signal_handlers == 0) {
      int i;
      Push_roots(r, 1);
      r[0] = action;
      signal_handlers = alloc_tuple(32);
      action = r[0];
      Pop_roots();
      for (i = 0; i < 32; i++) Field(signal_handlers, i) = Val_int(0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
    signal(sig, handle_signal);
    break;
  default:
    Assert(0);
  }
  return Val_unit;
}
