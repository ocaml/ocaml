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
#include <stdarg.h>
#include <signal.h>

#include "run.h"

#define COREFILENAME "core"

static volatile int timeout_expired = 0;

/* is_defined(str) returns 1 iff str points to a non-empty string */
/* Otherwise returns 0 */
static inline int is_defined(const char *str)
{
  return (str != NULL) && (*str != '\0');
}

void defaultLogger(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
}

__attribute__((__noreturn__)) /* TODO: make sure compiler supports this */
void fatal_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg, ...)
{
  va_list ap;
  Logger *logger = (settings->logger != NULL) ? settings->logger
                                              : defaultLogger;
  va_start(ap, msg);
  logger("%s:%d: ", file, line);
  logger(msg, ap);
  va_end(ap);
  exit(EXIT_FAILURE);
}

#define fatal_error(msg, ...) \
fatal_error_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/*
  Note: the ## __VA_ARGS__ construct is gcc specific.
  For a more portable (but also more complex) solution, see
  http://stackoverflow.com/questions/20818800/variadic-macro-and-trailing-comma
*/

__attribute__((__noreturn__)) /* TODO: make sure compiler supports this */
void fatal_perror_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg, ...)
{
  va_list ap;
  Logger *logger = (settings->logger != NULL) ? settings->logger
                                              : defaultLogger;
  va_start(ap, msg);
  logger("%s:%d: ", file, line);
  logger(msg, ap);
  va_end(ap);
  logger(": %s\n", strerror(errno));
  exit(EXIT_FAILURE);
}

#define fatal_perror(msg, ...) \
fatal_perror_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/* Same remark as for the fatal_error macro. */

__attribute__((__noreturn__)) /* TODO: make sure compiler supports this */
void open_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  fatal_perror_with_location(file, line, settings, "Can not open %s", msg);
}

#define open_error(filename) \
open_error_with_location(__FILE__, __LINE__, settings, filename)

__attribute__((__noreturn__)) /* TODO: make sure compiler supports this */
void realpath_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  fatal_perror_with_location(file, line, settings, "realpath(\"%s\") failed", msg);
}

#define realpath_error(filename) \
realpath_error_with_location(__FILE__, __LINE__, settings, filename)

void handle_alarm(int sig)
{
  timeout_expired = 1;
}

int run_command_child(const command_settings *settings)
{
  int res;
  int stdout_fd = -1, stderr_fd = -1; /* -1 means not redirected */
  int flags = O_WRONLY | (settings->append ? O_APPEND : O_CREAT | O_TRUNC);
  int mode = 0666;

  if (setpgid(0, 0) == -1) fatal_perror("setpgid");

  if (is_defined(settings->stdout_filename))
  {
    stdout_fd = open(settings->stdout_filename, flags, mode);
    if (stdout_fd < 0)
      open_error(settings->stdout_filename);
    if ( dup2(stdout_fd, STDOUT_FILENO) == -1 )
      fatal_perror("dup2 for stdout");
  }

  if (is_defined(settings->stderr_filename))
  {
    if (stdout_fd != -1)
    {
#ifdef __GLIBC__
      char *stdout_realpath, *stderr_realpath;
      stdout_realpath = realpath(settings->stdout_filename, NULL);
      if (stdout_realpath == NULL)
        realpath_error(settings->stdout_filename);
      stderr_realpath = realpath(settings->stderr_filename, NULL);
      if ( (stderr_realpath == NULL)  && (errno != ENOENT) )
      {
        free(stdout_realpath);
        realpath_error(settings->stderr_filename);
      }
#else
      char stdout_realpath[PATH_MAX], stderr_realpath[PATH_MAX];
      if (realpath(settings->stdout_filename, stdout_realpath) == NULL)
        realpath_error(settings->stdout_filename);
      if ((realpath(settings->stderr_filename, stderr_realpath) == NULL) && (errno != ENOENT))
        realpath_error(settings->stderr_filename);
#endif /* __GLIBC__ */
      if (strcmp(stdout_realpath, stderr_realpath) == 0)
        stderr_fd = stdout_fd;
#ifdef __GLIBC__
      free(stdout_realpath);
      free(stderr_realpath);
#endif /* __GLIBC__ */
    }
    if (stderr_fd == -1)
    {
      stderr_fd = open(settings->stderr_filename, flags, mode);
      if (stderr_fd == -1) open_error(settings->stderr_filename);
    }
    if ( dup2(stderr_fd, STDERR_FILENO) == -1 )
      fatal_perror("dup2 for stderr");
  }

  res = execv(settings->program, *settings->argv); /* , *settings->envp); */

  fatal_perror("Cannot execute %s", settings->program);
  return res;
}

/* Handles the termination of a process. Arguments:
 * The pid of the terminated process
 * Its termination status as returned by wait(2)
 * A string giving a prefix for the core file name.
   (the file will be called prefix.pid.core but may come from a
   diffferent process)
 * Returns the code to return if this is the child process
 */
int handle_process_termination(
  const command_settings *settings,
  pid_t pid, int status, const char *corefilename_prefix)
{
  int signal, core = 0;
  char *corestr;

  if (WIFEXITED(status)) return WEXITSTATUS(status);

  if ( !WIFSIGNALED(status) )
    fatal_error("Process with pid %d neither terminated normally nor received a signal!?", pid);

  /* From here we know that the process terminated due to a signal */
  signal = WTERMSIG(status);
#ifdef WCOREDUMP
  core = WCOREDUMP(status);
#endif /* WCOREDUMP */
  corestr = core ? "" : "no ";
  fprintf(stderr,
    "Process %d got signal %d(%s), %score dumped\n",
    pid, signal, strsignal(signal), corestr
  );

  if (core)
  {
    if ( access(COREFILENAME, F_OK) == -1)
      fprintf(stderr, "Could not find core file.\n");
    else {
      char corefile[strlen(corefilename_prefix) + 128];
      snprintf(corefile, sizeof(corefile), "%s.%d.core", corefilename_prefix, pid);
      if ( rename(COREFILENAME, corefile) == -1)
        fprintf(stderr, "Tge core file exists but could not be renamed.\n");
      else
        fprintf(stderr,"The core file has been renamed to %s\n", corefile);
    }
  }

  return -signal;
}

int run_command_parent(const command_settings *settings, pid_t child_pid)
{
  int waiting = 1, status, code, child_code;
  pid_t pid;

  if (settings->timeout>0)
  {
    struct sigaction action;
    action.sa_handler = handle_alarm;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESETHAND;
    if (sigaction(SIGALRM, &action, NULL) == -1) fatal_perror("sigaction");
    if (alarm(settings->timeout) == -1) fatal_perror("alarm");
  }

  while (waiting)
  {
    pid = wait(&status);
    if (pid == -1)
    {
      switch errno
      {
        case EINTR:
          if ((settings->timeout > 0) && (timeout_expired))
          {
            timeout_expired = 0;
            fprintf(stderr, "Timeout expired, killing all child processes");
            if (kill(-child_pid, SIGKILL) == -1) fatal_perror("kill");
          };
          break;
        case ECHILD:
          waiting = 0;
          break;
        default:
          fatal_perror("wait");
      }
    } else { /* Got a pid */
      code = handle_process_termination(settings, pid, status, settings->program);
      if (pid == child_pid) child_code = code;
    }
  }

  return child_code;
}

int run_command(const command_settings *settings)
{
  pid_t child_pid = fork();
  if (child_pid == -1) fatal_perror("fork");
  if (child_pid == 0) return run_command_child(settings);
  else return run_command_parent(settings, child_pid);
}

#if 0
int main(int argc, char *argv[])
{
  int result;
  if (argc<2) fatal_error("Specify which program to execute");
  char *program = argv[1];
  char *args[] = { program, NULL };
  char *env[] = { NULL };
  command_settings settings;
  settings.program = program;
  /* Case 1:
    settings.stdout_filename = NULL;
    settings.stderr_filename = NULL;
  */
  /* Case 2:
    settings.stdout_filename = "/tmp/output";
    settings.stderr_filename = NULL;
  */
  /* Case 3:
    settings.stdout_filename = NULL;
    settings.stderr_filename = "/tmp/error";
  */
  /* Case 4:
    settings.stdout_filename = "/tmp/output";
    settings.stderr_filename = "/tmp/error";
  */
  /* Case 5:
    settings.stdout_filename = "/tmp/log";
    settings.stderr_filename = "/tmp/log";
  */
  settings.stdout_filename = settings.stderr_filename = NULL;
  settings.argv = &args;
  settings.envp = &env;
  settings.timeout = 2;
  result = run_command(&settings);
  fprintf(stderr, "run_command returned %d\n", result);
}
#endif /* 0 */
