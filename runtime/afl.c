/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2016 Stephen Dolan.                                        */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Runtime support for afl-fuzz */
#include "caml/config.h"

/* Values used by the instrumentation logic (see cmmgen.ml) */
static unsigned char afl_area_initial[1 << 16];
unsigned char* caml_afl_area_ptr = afl_area_initial;
uintnat caml_afl_prev_loc;

#if !defined(HAS_SYS_SHM_H) || !defined(HAS_SHMAT)

#include "caml/mlvalues.h"
#include "caml/domain.h"

CAMLprim value caml_reset_afl_instrumentation(value full)
{
  return Val_unit;
}

CAMLexport value caml_setup_afl(value unit)
{
  /* AFL is not supported */
  return Val_unit;
}

#else

#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <stdio.h>
#include <string.h>

#define CAML_INTERNALS
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"

static int afl_initialised = 0;

/* afl uses abnormal termination (SIGABRT) to check whether
   to count a testcase as "crashing" */
extern int caml_abort_on_uncaught_exn;

/* File descriptors used to synchronise with afl-fuzz */
#define FORKSRV_FD_READ 198
#define FORKSRV_FD_WRITE 199

static void afl_write(uint32_t msg)
{
  if (write(FORKSRV_FD_WRITE, &msg, 4) != 4)
    caml_fatal_error("writing to afl-fuzz");
}

static uint32_t afl_read(void)
{
  uint32_t msg;
  if (read(FORKSRV_FD_READ, &msg, 4) != 4)
    caml_fatal_error("reading from afl-fuzz");
  return msg;
}

CAMLexport value caml_setup_afl(value unit)
{
  char* shm_id_str;
  char* shm_id_end;
  long int shm_id;
  uint32_t startup_msg = 0;

  if (afl_initialised) return Val_unit;
  afl_initialised = 1;

  shm_id_str = caml_secure_getenv("__AFL_SHM_ID");
  if (shm_id_str == NULL) {
    /* Not running under afl-fuzz, continue as normal */
    return Val_unit;
  }

  /* if afl-fuzz is attached, we want it to know about uncaught exceptions */
  caml_abort_on_uncaught_exn = 1;

  shm_id = strtol(shm_id_str, &shm_id_end, 10);
  if (!(*shm_id_str != '\0' && *shm_id_end == '\0'))
    caml_fatal_error("afl-fuzz: bad shm id");

  caml_afl_area_ptr = shmat((int)shm_id, NULL, 0);
  if (caml_afl_area_ptr == (void*)-1)
    caml_fatal_error("afl-fuzz: could not attach shm area");

  /* poke the bitmap so that afl-fuzz knows we exist, even if the
     application has sparse instrumentation */
  caml_afl_area_ptr[0] = 1;

  /* synchronise with afl-fuzz */
  if (write(FORKSRV_FD_WRITE, &startup_msg, 4) != 4) {
    /* initial write failed, so assume we're not meant to fork.
       afl-tmin uses this mode. */
    return Val_unit;
  }
  afl_read();

  /* ensure that another module has not already spawned a domain */
  if (caml_domain_is_multicore())
    caml_fatal_error("afl-fuzz: cannot fork with multiple domains running");

  while (1) {
    int child_pid = fork();
    if (child_pid < 0) caml_fatal_error("afl-fuzz: could not fork");
    else if (child_pid == 0) {
      caml_atfork_hook();
      /* Run the program */
      close(FORKSRV_FD_READ);
      close(FORKSRV_FD_WRITE);
      return Val_unit;
    }

    /* As long as the child keeps raising SIGSTOP, we re-use the same process */
    while (1) {
      int status;
      uint32_t was_killed;

      afl_write((uint32_t)child_pid);

      /* WUNTRACED means wait until termination or SIGSTOP */
      if (waitpid(child_pid, &status, WUNTRACED) < 0)
        caml_fatal_error("afl-fuzz: waitpid failed");

      afl_write((uint32_t)status);

      was_killed = afl_read();
      if (WIFSTOPPED(status)) {
        /* child stopped, waiting for another test case */
        if (was_killed) {
          /* we saw the child stop, but since then afl-fuzz killed it.
             we should wait for it before forking another child */
          if (waitpid(child_pid, &status, 0) < 0)
            caml_fatal_error("afl-fuzz: waitpid failed");
          break;
        } else {
          kill(child_pid, SIGCONT);
        }
      } else {
        /* child died */
        break;
      }
    }
  }
}

CAMLprim value caml_reset_afl_instrumentation(value full)
{
  if (full == Val_true) {
    memset(caml_afl_area_ptr, 0, sizeof(afl_area_initial));
  }
  caml_afl_prev_loc = 0;
  return Val_unit;
}

#endif /* HAS_SYS_SHM_H */
