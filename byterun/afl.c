/* Runtime support for afl-fuzz */

#ifdef _WIN32

#include "caml/mlvalues.h"

CAMLprim value caml_setup_afl (value unit)
{
  return Val_unit;
}

#else

#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <stdio.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"

static int afl_initialised = 0;

/* afl uses abnormal termination (SIGABRT) to check whether
   to count a testcase as "crashing" */
extern int caml_abort_on_uncaught_exn;

/* Values used by the instrumentation logic (see cmmgen.ml) */
static unsigned char afl_area_initial[1 << 16];
unsigned char* caml_afl_area_ptr = afl_area_initial;
uintnat caml_afl_prev_loc;

/* File descriptors used to synchronise with afl-fuzz */
#define FORKSRV_FD_READ 198
#define FORKSRV_FD_WRITE 199

static void afl_write(uint32_t msg)
{
  if (write(FORKSRV_FD_WRITE, &msg, 4) != 4)
    caml_fatal_error("writing to afl-fuzz");
}

static uint32_t afl_read()
{
  uint32_t msg;
  if (read(FORKSRV_FD_READ, &msg, 4) != 4)
    caml_fatal_error("reading from afl-fuzz");
  return msg;
}

CAMLprim value caml_setup_afl(value unit)
{
  if (afl_initialised) return Val_unit;
  afl_initialised = 1;

  char* shm_id_str = getenv("__AFL_SHM_ID");
  if (shm_id_str == NULL) {
    /* Not running under afl-fuzz, continue as normal */
    return Val_unit;
  }

  /* if afl-fuzz is attached, we want it to know about uncaught exceptions */
  caml_abort_on_uncaught_exn = 1;

  char* shm_id_end;
  long int shm_id = strtol(shm_id_str, &shm_id_end, 10);
  if (!(*shm_id_str != '\0' && *shm_id_end == '\0'))
    caml_fatal_error("afl-fuzz: bad shm id");

  caml_afl_area_ptr = shmat((int)shm_id, NULL, 0);
  if (caml_afl_area_ptr == (void*)-1)
    caml_fatal_error("afl-fuzz: could not attach shm area");

  /* poke the bitmap so that afl-fuzz knows we exist, even if the
     application has sparse instrumentation */
  caml_afl_area_ptr[0] = 1;

  /* synchronise with afl-fuzz */
  afl_write(0);
  afl_read();

  while (1) {
    int child_pid = fork();
    if (child_pid < 0) caml_fatal_error("afl-fuzz: could not fork");
    else if (child_pid == 0) {
      /* Run the program */
      close(FORKSRV_FD_READ);
      close(FORKSRV_FD_WRITE);
      return Val_unit;
    }

    /* As long as the child keeps raising SIGSTOP, we re-use the same process */
    while (1) {
      afl_write((uint32_t)child_pid);

      int status;
      /* WUNTRACED means wait until termination or SIGSTOP */
      if (waitpid(child_pid, &status, WUNTRACED) < 0)
        caml_fatal_error("afl-fuzz: waitpid failed");
      afl_write((uint32_t)status);

      uint32_t was_killed = afl_read();
      if (WIFSTOPPED(status)) {
        /* child stopped, waiting for another test case */
        if (was_killed) {
          /* we saw the child stop, but since then afl-fuzz killed it.
             we should wait for it before forking another child */
          if (waitpid(child_pid, &status, 0) < 0)
            caml_fatal_error("afl-fuzz: waitpid failed");
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

#endif /* _WIN32 */
