/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Run programs with redirections and timeouts under Windows */

/* GetTickCount64() requires Windows Vista or Server 2008 */
#define _WIN32_WINNT 0x0600

#include <stdio.h>
#include <stdlib.h>
#include <wtypes.h>
#include <winbase.h>
#include <windows.h>
#include <process.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/types.h>

#include "caml/memory.h"
#include "caml/osdeps.h"

#include "run.h"
#include "run_common.h"

static void report_error(
  const char *file, int line,
  const command_settings *settings,
  const char *message, const WCHAR *argument)
{
  WCHAR windows_error_message[1024];
  DWORD error = GetLastError();
  char *caml_error_message;
  if (FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL, error, 0, windows_error_message,
    sizeof(windows_error_message)/sizeof(WCHAR), NULL) ) {
    caml_error_message = caml_stat_strdup_of_utf16(windows_error_message);
  } else {
    caml_error_message = caml_stat_alloc(256);
    sprintf(caml_error_message, "unknown Windows error #%lu", error);
  }
  if ( is_defined(argument) )
    error_with_location(file, line,
      settings, "%s %s: %s", message, argument, caml_error_message);
  else
    error_with_location(file, line,
      settings, "%s: %s", message, caml_error_message);
  caml_stat_free(caml_error_message);
}

static WCHAR *find_program(const WCHAR *program_name)
{
  int max_path_length = 512;
  DWORD result;
  LPCWSTR searchpath = NULL, extension = L".exe";
  WCHAR **filepart = NULL;
  WCHAR *fullpath = malloc(max_path_length*sizeof(WCHAR));
  if (fullpath == NULL) return NULL;

  result = SearchPath
  (
    searchpath,
    program_name,
    extension,
    max_path_length,
    fullpath,
    filepart
  );
  if (result == 0)
  {
    /* It may be an absolute path, return a copy of it */
    int l = wcslen(program_name) + 1;
    free(fullpath);
    fullpath = malloc(l*sizeof(WCHAR));
    if (fullpath != NULL) wcscpy(fullpath, program_name);
    return fullpath;
  }
  if (result <= max_path_length) return fullpath;

  /* fullpath was too small, allocate a bigger one */
  free(fullpath);

  result++; /* Take '\0' into account */

  fullpath = malloc(result*sizeof(WCHAR));
  if (fullpath == NULL) return NULL;
  SearchPath
  (
    searchpath,
    program_name,
    extension,
    result,
    fullpath,
    filepart
  );
  return fullpath;
}

static WCHAR *commandline_of_arguments(WCHAR **arguments)
{
  WCHAR *commandline = NULL, *commandline_p;
  int args = 0; /* Number of arguments */
  int commandline_length = 0;

  if (*arguments == NULL) return NULL;
  /* From here we know there is at least one argument */

  /* First compute number of arguments and commandline length */
  for (WCHAR **arguments_p = arguments; *arguments_p != NULL; arguments_p++)
  {
    args++;
    commandline_length += wcslen(*arguments_p);
  }
  commandline_length += args; /* args-1 ' ' between arguments + final '\0' */

  /* Allocate memory and accumulate arguments separated by spaces */
  commandline = malloc(commandline_length*sizeof(WCHAR));
  if (commandline == NULL) return NULL;
  commandline_p = commandline;
  for (WCHAR **arguments_p = arguments; *arguments_p != NULL; arguments_p++)
  {
    int l = wcslen(*arguments_p);
    memcpy(commandline_p, *arguments_p, l*sizeof(WCHAR));
    commandline_p += l;
    *commandline_p = L' ';
    commandline_p++;
  }
  commandline[commandline_length-1] = 0;
  return commandline;
}

static LPVOID prepare_environment(WCHAR **localenv)
{
  LPTCH p, r, env, process_env = NULL;
  int l, process_env_length, localenv_length, env_length;

  if (localenv == NULL) return NULL;

  process_env = GetEnvironmentStrings();
  if (process_env == NULL) return NULL;

  /* Compute length of process environment */
  process_env_length = 0;
  p = process_env;
  while (*p != L'\0') {
    l = wcslen(p) + 1; /* also count terminating '\0' */
    process_env_length += l;
    p += l;
  }

  /* Compute length of local environment */
  localenv_length = 0;
  for (WCHAR **q = localenv; *q != NULL; q++) {
    localenv_length += wcslen(*q) + 1;
  }

  /* Build new env that contains both process and local env */
  env_length = process_env_length + localenv_length + 1;
  env = malloc(env_length * sizeof(WCHAR));
  if (env == NULL) {
    FreeEnvironmentStrings(process_env);
    return NULL;
  }
  r = env;
  p = process_env;
  /* Copy process_env to env only if the given names are not in localenv */
  while (*p != L'\0') {
    wchar_t *pos_eq = wcschr(p, L'=');
    int copy = 1;
    l = wcslen(p) + 1; /* also count terminating '\0' */
    /* Temporarily change the = to \0 for wcscmp */
    *pos_eq = L'\0';
    for (WCHAR **q = localenv; *q != NULL; q++) {
      wchar_t *pos_eq2 = wcschr(*q, L'=');
      /* Compare this name in localenv with the current one in processenv */
      if (pos_eq2) *pos_eq2 = L'\0';
      if (!wcscmp(*q, p)) copy = 0;
      if (pos_eq2) *pos_eq2 = L'=';
    }
    *pos_eq = L'=';
    if (copy) {
      /* This name is not marked for deletion/update in localenv, so copy */
      memcpy(r, p, l * sizeof(WCHAR));
      r += l;
    }
    p += l;
  }
  FreeEnvironmentStrings(process_env);
  for (WCHAR **q = localenv; *q != NULL; q++) {
    /* A string in localenv without '=' signals deletion, which has been done */
    wchar_t *pos_eq = wcschr(*q, L'=');
    if (pos_eq) {
      l = wcslen(*q) + 1;
      memcpy(r, *q, l * sizeof(WCHAR));
      r += l;
    }
  }
  *r = L'\0';
  return env;
}

static SECURITY_ATTRIBUTES security_attributes = {
  sizeof(SECURITY_ATTRIBUTES), /* nLength */
  NULL, /* lpSecurityDescriptor */
  TRUE /* bInheritHandle */
};

static HANDLE create_input_handle(const WCHAR *filename)
{
  return CreateFile
  (
    filename,
    GENERIC_READ, /* DWORD desired_access */
    FILE_SHARE_READ, /* DWORD share_mode */
    &security_attributes,
    OPEN_EXISTING, /* DWORD creation_disposition */
    FILE_ATTRIBUTE_NORMAL, /* DWORD flags_and_attributes */
    NULL /* HANDLE template_file */
  );
}

static HANDLE create_output_handle(const WCHAR *filename, int append)
{
  DWORD desired_access = append ? FILE_APPEND_DATA : GENERIC_WRITE;
  DWORD share_mode = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
  DWORD creation_disposition = append ? OPEN_ALWAYS : CREATE_ALWAYS;
  return CreateFile
  (
    filename,
    desired_access,
    share_mode,
    &security_attributes,
    creation_disposition,
    FILE_ATTRIBUTE_NORMAL, /* DWORD flags_and_attributes */
    NULL /* HANDLE template_file */
  );
}

#define checkerr(condition, message, argument) \
if ( (condition) ) \
{ \
  report_error(__FILE__, __LINE__, settings, message, argument); \
  status = -1; \
  goto cleanup; \
} else { }

static WCHAR *translate_finename(WCHAR *filename)
{
  if (!wcscmp(filename, L"/dev/null")) return L"NUL"; else return filename;
}

int run_command(const command_settings *settings)
{
  BOOL process_created = FALSE;
  int stdin_redirected = 0, stdout_redirected = 0, stderr_redirected = 0;
  int combined = 0; /* 1 if stdout and stderr are redirected to the same file */
  int wait_again = 0;
  WCHAR *program = NULL;
  WCHAR *commandline = NULL;

  LPVOID environment = NULL;
  STARTUPINFO startup_info;
  PROCESS_INFORMATION process_info;
  BOOL wait_result;
  DWORD status;
  ULONGLONG stamp, cur;
  DWORD timeout = (settings->timeout > 0) ? settings->timeout * 1000 : INFINITE;

  JOBOBJECT_ASSOCIATE_COMPLETION_PORT port = {NULL, NULL};
  HANDLE hJob = NULL;
  DWORD completion_code;
  ULONG_PTR completion_key;
  LPOVERLAPPED pOverlapped;

  ZeroMemory(&startup_info, sizeof(STARTUPINFO));
  startup_info.cb = sizeof(STARTUPINFO);
  startup_info.dwFlags = STARTF_USESTDHANDLES;

  program = find_program(settings->program);
  checkerr(
    (program == NULL),
    "Could not find program to execute",
     settings->program
  );

  commandline = commandline_of_arguments(settings->argv);

  environment = prepare_environment(settings->envp);

  if (is_defined(settings->stdin_filename))
  {
    WCHAR *stdin_filename = translate_finename(settings->stdin_filename);
    startup_info.hStdInput = create_input_handle(stdin_filename);
    checkerr( (startup_info.hStdInput == INVALID_HANDLE_VALUE),
      "Could not redirect standard input",
      stdin_filename);
    stdin_redirected = 1;
  } else startup_info.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

  if (is_defined(settings->stdout_filename))
  {
    WCHAR *stdout_filename = translate_finename(settings->stdout_filename);
    startup_info.hStdOutput = create_output_handle(
      stdout_filename, settings->append
    );
    checkerr( (startup_info.hStdOutput == INVALID_HANDLE_VALUE),
      "Could not redirect standard output",
      stdout_filename);
    stdout_redirected = 1;
  } else startup_info.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);

  if (is_defined(settings->stderr_filename))
  {
    if (stdout_redirected)
    {
      if (wcscmp(settings->stdout_filename, settings->stderr_filename) == 0)
      {
        startup_info.hStdError = startup_info.hStdOutput;
        stderr_redirected = 1;
        combined = 1;
      }
    }

    if (! stderr_redirected)
    {
      WCHAR *stderr_filename = translate_finename(settings->stderr_filename);
      startup_info.hStdError = create_output_handle
      (
        stderr_filename, settings->append
      );
      checkerr( (startup_info.hStdError == INVALID_HANDLE_VALUE),
        "Could not redirect standard error",
        stderr_filename);
      stderr_redirected = 1;
    }
  } else startup_info.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  process_created = CreateProcess(
    program,
    commandline,
    NULL, /* SECURITY_ATTRIBUTES process_attributes */
    NULL, /* SECURITY_ATTRIBUTES thread_attributes */
    TRUE, /* BOOL inherit_handles */
    CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT, /* DWORD creation_flags */
    environment,
    NULL, /* LPCSTR current_directory */
    &startup_info,
    &process_info
  );
  checkerr( (! process_created), "CreateProcess failed", NULL);

  hJob = CreateJobObject(NULL, NULL);
  checkerr( (hJob == NULL), "CreateJobObject failed", NULL);
  checkerr( !AssignProcessToJobObject(hJob, process_info.hProcess),
    "AssignProcessToJob failed", NULL);
  port.CompletionPort =
    CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
  checkerr( (port.CompletionPort == NULL),
    "CreateIoCompletionPort failed", NULL);
  checkerr( !SetInformationJobObject(
    hJob,
    JobObjectAssociateCompletionPortInformation,
    &port, sizeof(port)), "SetInformationJobObject failed", NULL);

  ResumeThread(process_info.hThread);
  CloseHandle(process_info.hThread);

  stamp = GetTickCount64();
  while ((wait_result = GetQueuedCompletionStatus(port.CompletionPort,
                                                  &completion_code,
                                                  &completion_key,
                                                  &pOverlapped,
                                                  timeout))
         && completion_code != JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO)
  {
    if (timeout != INFINITE)
    {
      cur = GetTickCount64();
      if (cur > stamp) {
        ULONGLONG elapsed = cur - stamp;
        timeout = (timeout > elapsed ? timeout - elapsed : 0);
        stamp = cur;
      }
    }
  }
  if (wait_result)
  {
    /* The child has terminated before the timeout has expired */
    checkerr( (! GetExitCodeProcess(process_info.hProcess, &status)),
      "GetExitCodeProcess failed", NULL);
  } else if (pOverlapped == NULL) {
    /* The timeout has expired, terminate the process */
    checkerr( (! TerminateJobObject(hJob, 0)),
      "TerminateJob failed", NULL);
    status = -1;
    wait_again = 1;
  } else {
    error_with_location(__FILE__, __LINE__, settings,
      "GetQueuedCompletionStatus failed\n");
    report_error(__FILE__, __LINE__,
      settings, "Failure while waiting for process termination", NULL);
    status = -1;
  }

cleanup:
  free(program);
  free(commandline);
  if (stdin_redirected) CloseHandle(startup_info.hStdInput);
  if (stdout_redirected) CloseHandle(startup_info.hStdOutput);
  if (stderr_redirected && !combined) CloseHandle(startup_info.hStdError);
  if (wait_again)
  {
    /* Wait again but this time just 1sec to avoid being blocked */
    WaitForSingleObject(process_info.hProcess, 1000);
  }
  if (process_created) CloseHandle(process_info.hProcess);
  if (hJob != NULL) CloseHandle(hJob);
  if (port.CompletionPort != NULL) CloseHandle(port.CompletionPort);
  return status;
}
