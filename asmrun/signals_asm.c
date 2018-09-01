/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Signal handling, code specific to the native-code compiler */

#if defined(TARGET_amd64) && defined (SYS_linux)
#define _GNU_SOURCE
#endif
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/domain.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/spacetime.h"

#ifndef NSIG
#define NSIG 64
#endif

typedef void (*signal_handler)(int signo);

#ifdef _WIN32
extern signal_handler caml_win32_signal(int sig, signal_handler action);
#define signal(sig,act) caml_win32_signal(sig,act)
#endif

extern char * caml_code_area_start, * caml_code_area_end;
extern char caml_system__code_begin, caml_system__code_end;

/* Do not use the macro from address_class.h here. */
#undef Is_in_code_area
#define Is_in_code_area(pc) \
 ( ((char *)(pc) >= caml_code_area_start && \
    (char *)(pc) <= caml_code_area_end)     \
|| ((char *)(pc) >= &caml_system__code_begin && \
    (char *)(pc) <= &caml_system__code_end)     \
/* FIXME || (Classify_addr(pc) & In_code_area) */ )

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to OCaml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection()
{
  caml_handle_gc_interrupt();

#ifdef WITH_SPACETIME
  if (caml_young_ptr == caml_young_alloc_end) {
    caml_spacetime_automatic_snapshot();
  }
#endif

  caml_process_pending_signals();
}

static void handle_signal(int sig, siginfo_t* info, void* context)
{
  int saved_errno;
  /* Save the value of errno (PR#5982). */
  saved_errno = errno;
  if (sig < 0 || sig >= NSIG) return;
  caml_record_signal(sig);
  errno = saved_errno;
}

int caml_set_signal_action(int signo, int action)
{
  signal_handler oldact;
  struct sigaction sigact, oldsigact;

  switch(action) {
  case 0:
    sigact.sa_handler = SIG_DFL;
    sigact.sa_flags = 0;
    break;
  case 1:
    sigact.sa_handler = SIG_IGN;
    sigact.sa_flags = 0;
    break;
  default:
    sigact.sa_sigaction = handle_signal;
    sigact.sa_flags = SA_ONSTACK | SA_SIGINFO;
#if defined(TARGET_amd64) && defined(SYS_macosx)
    sigact.sa_flags |= SA_64REGSET;
#endif
    break;
  }
  sigemptyset(&sigact.sa_mask);
  if (sigaction(signo, &sigact, &oldsigact) == -1) return -1;
  oldact = oldsigact.sa_handler;
  if (oldact == (signal_handler) handle_signal)
    return 2;
  else if (oldact == SIG_IGN)
    return 1;
  else
    return 0;
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_power) \
  || defined(TARGET_s390x)
#error "Architecture requires a bounds-check trap handler"
DECLARE_SIGNAL_HANDLER(trap_handler)
{
  /* TODO: raise a real exception here */
  caml_fatal_error ("bounds check failed");
}
#endif

/* Initialization of signal stuff */

void caml_init_signals(void)
{
  /* Bound-check trap handling */

#if defined(TARGET_power)
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = trap_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigaction(SIGTRAP, &act, NULL);
  }
#endif

#if defined(TARGET_s390x)
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = trap_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigaction(SIGFPE, &act, NULL);
  }
#endif
}
