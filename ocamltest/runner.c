/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sébastien Hinderer, projet Gallium, INRIA Paris           */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Run programs and log their stdout/stderr, etc., with an optional timer */

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

typedef char *array[];
typedef array * arrayptr;

typedef struct {
  const char *filename;
  arrayptr argv;
  arrayptr envp;
  const char *stdout;
  const char *stderr;
  int timeout;
} command_settings;

void fatal_perror(const char *msg)
{
  perror(msg);
  exit(EXIT_FAILURE);
}

void fatal_error(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(EXIT_FAILURE);
}

void handle_alarm(int sig)
{
  if (sig!=SIGALARM)
  {
    char message[256];
    sprintf(message, "Received unexpected signal %s", strsignal(sig));
    fatal_error(message);
  }

}

int run_command(const command_settings *settings)
{
  pid_t child_pid;

  int stdout_fd = -1, stderr_fd = -1;
  child_pid = fork();
  if (child_pid == -1) fatal_perror("fork");
  if (child_pid == 0)
  { /* child */
    int flags = O_WRONLY | O_CREAT | O_TRUNC;
    int mode = 0644;
    if (settings->stdout && *(settings->stdout))
    {
      stdout_fd = open(settings->stdout, flags, mode);
      if (stdout_fd < 0) fatal_perror("open");
      if ( dup2(stdout_fd, 1) == -1 ) fatal_perror("dup2");
    }
    if (settings->stderr && *(settings->stderr))
    {
      if (stdout_fd != -1)
      {
        char stdout_realpath[PATH_MAX], stderr_realpath[PATH_MAX];
        if (realpath(settings->stdout, stdout_realpath) == NULL)
          fatal_perror("realpath");
        if ((realpath(settings->stderr, stderr_realpath) == NULL) && (errno != ENOENT))
          fatal_perror("realpath");
        if (strcmp(stdout_realpath, stderr_realpath) == 0)
          stderr_fd = stdout_fd;
      }
      if (stderr_fd == -1)
      {
        stderr_fd = open(settings->stderr, flags, mode);
        if (stderr_fd == -1) fatal_perror("open");
      }
      if ( dup2(stderr_fd, 2) == -1 ) fatal_perror("dup2");
    }
    if (execve(settings->filename, *settings->argv, *settings->envp) == -1)
      fatal_perror("execve");
  } else { /* father */
    int child_status, result;
    result = waitpid(child_pid, &child_status, 0);
    if (result == -1) fatal_perror("waitpid");
    else if (result != child_pid)
      fatal_error("waitpid returned a pid different from the expected one");
    if (WIFEXITED(child_status)) {
      int code = WEXITSTATUS(child_status);
      fprintf(stderr, "Child terminated, code=%d\n", code);
      return code;
    }
    if ( WIFSIGNALED(child_status) ) {
      int signal = WTERMSIG(child_status);
      int core = 0;
      char *corestr;
#ifdef WCOREDUMP
      core = WCOREDUMP(child_status);
#endif /* WCOREDUMP */
      corestr = core ? "" : "no ";
      fprintf(stderr,
        "Child got signal %d (%score dumped)\n", signal, corestr
      );
      if (core)
      {
        if ( access("core", F_OK) == -1)
          fprintf(stderr, "Could not find core file.\n");
        else {
          char corefile[strlen(settings->filename) + 6];
          sprintf(corefile, "%s.core", settings->filename);
          if ( rename("core", corefile) == -1)
            fprintf(stderr, "Tge core file exists but could not be renamed.\n");
          else
            fprintf(stderr,"The core file has been renamed to %s\n", corefile);
        }
      }
      return -signal;
    }
    fatal_error("Child neither terminated normally nor received a signal!?");
  }
  return -1; /* To please gcc. Never reached */
}

int main()
{
  int result;
  char *program = "./testprog";
  char *args[] = { program, NULL };
  char *env[] = { NULL };
  command_settings settings;
  settings.filename = program;
  /* Case 1:
    settings.stdout = NULL;
    settings.stderr = NULL;
  */
  /* Case 2:
    settings.stdout = "/tmp/output";
    settings.stderr = NULL;
  */
  /* Case 3:
    settings.stdout = NULL;
    settings.stderr = "/tmp/error";
  */
  /* Case 4:
    settings.stdout = "/tmp/output";
    settings.stderr = "/tmp/error";
  */
  /* Case 5: */
    settings.stdout = "/tmp/log";
    settings.stderr = "/tmp/log";
  settings.argv = &args;
  settings.envp = &env;
  result = run_command(&settings);
  fprintf(stderr, "run_command returned %d\n", result);
}
