/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "signals.h"

void enter_blocking_section()
{
}

void leave_blocking_section()
{
}

value install_signal_handler(signal_number, action) /* ML */
     value signal_number, action;
{
  invalid_argument("Sys.signal: not implemented");
  return Val_unit;
}

/* Machine- and OS-dependent initialization of traps */

#ifdef TARGET_sparc

#ifdef SYS_sunos
#include <stdio.h>
#include <signal.h>

static void trap_handler(sig, code, context, address)
     int sig, code;
     struct sigcontext * context;
     char * address;
{
  if (sig == SIGILL && code == ILL_TRAP_FAULT(5)) {
    array_bound_error();
  } else {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n", code);
    exit(100);
  }
}

void init_signals()
{
  signal(SIGILL, trap_handler);
}
#endif

#ifdef SYS_solaris
#include <stdio.h>
#include <signal.h>
#include <siginfo.h>

static void trap_handler(sig, info, context)
     int sig;
     siginfo_t * info;
     void * context;
{
  if (sig == SIGILL && info->si_code == ILL_ILLTRP) {
    array_bound_error();
  } else {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n",
            info->si_code);
    exit(100);
  }
}

void init_signals()
{
  struct sigaction act;
  act.sa_sigaction = trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGILL, &act, NULL);
}
#endif

#else

void init_signals()
{
}

#endif

