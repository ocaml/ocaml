/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include "unixsupport.h"

/* From the Caml runtime */
extern char * searchpath(char * name);

static int win_has_console(void);

value win_create_process_native(value cmd, value cmdline, value env,
                                value fd1, value fd2, value fd3)
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char * exefile, * envp;
  int flags;

  exefile = searchpath(String_val(cmd));
  if (exefile == NULL) exefile = String_val(cmd);
  if (env != Val_int(0)) {
    envp = String_val(Field(env, 0));
  } else {
    envp = NULL;
  }
  /* Prepare stdin/stdout/stderr redirection */
  GetStartupInfo(&si);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(fd1);
  si.hStdOutput = Handle_val(fd2);
  si.hStdError = Handle_val(fd3);
  /* If we do not have a console window, then we must run
     console mode applications as detached processes.
     Otherwise, a new console is created and the redirections
     are ignored.  If we're running a GUI application, the
     detached / non-detached flag doesn't matter. */
  if (win_has_console())
    flags = 0;
  else
    flags = DETACHED_PROCESS;
  /* Create the process */
  if (! CreateProcess(exefile, String_val(cmdline), NULL, NULL,
                      TRUE, flags, envp, NULL, &si, &pi)) {
    _dosmaperr(GetLastError());
    uerror("create_process", cmd);
  }
  CloseHandle(pi.hThread);
  /* Return the process handle as pseudo-PID
     (this is consistent with the wait() emulation in the MSVC C library */
  return Val_int(pi.hProcess);
}

value win_create_process(value * argv, int argn) /* ML */
{
  return win_create_process_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}

static int win_has_console(void)
{
  HANDLE h;

  h = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
                 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    return 0;
  } else {
    CloseHandle(h);
    return 1;
  }
}
