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

value win_create_process_native(exe, cmdline, env, fd1, fd2, fd3)
     value exe, cmdline, env, fd1, fd2, fd3;
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char * envp;

  if (env != Val_int(0)) {
    envp = String_val(Field(env, 0));
  } else {
    envp = NULL;
  }
  GetStartupInfo(&si);
  si.dwFlags |= STARTF_USESTDHANDLES;

  if ((si.hStdInput = _get_osfhandle(Int_val(fd1))) == -1 ||
      (si.hStdOutput = _get_osfhandle(Int_val(fd2))) == -1 ||
      (si.hStdError = _get_osfhandle(Int_val(fd3))) == -1 ||
      ! CreateProcess(String_val(exe), String_val(cmdline), NULL, NULL,
                      TRUE, 0, envp, NULL, &si, &pi)) {
    _dosmaperr(GetLastError());
    uerror("create_process", exe);
  }
  return Val_int(pi.hProcess);
}

value win_create_process(argv, argn)
	value * argv;
	int argn;
{
  return win_create_process_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}
