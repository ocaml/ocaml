/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include "unixsupport.h"

/* From the Caml runtime */
extern char * searchpath(char * name);

value win_create_process_native(value cmd, value cmdline, value env,
                                value fd1, value fd2, value fd3)
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char * exefile, * envp;

  exefile = searchpath(String_val(cmd));
  if (exefile == NULL) exefile = String_val(cmd);
  if (env != Val_int(0)) {
    envp = String_val(Field(env, 0));
  } else {
    envp = NULL;
  }
  GetStartupInfo(&si);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(fd1);
  si.hStdOutput = Handle_val(fd2);
  si.hStdError = Handle_val(fd3);
  if (! CreateProcess(exefile, String_val(cmdline), NULL, NULL,
                      TRUE, 0, envp, NULL, &si, &pi)) {
    _dosmaperr(GetLastError());
    uerror("create_process", cmd);
  }
  return Val_int(pi.hProcess);
}

value win_create_process(value * argv, int argn) /* ML */
{
  return win_create_process_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}
