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

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"

#include <sys/types.h>
#include <sys/wait.h>

#ifndef WIFEXITED
#define WIFEXITED(status) ((status) & 0xFF == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) ((status) & 0xFF == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

static value alloc_process_status(pid, status)
     int pid, status;
{
  value st, res;
  Push_roots(r, 1);

  if (WIFEXITED(status)) {
    st = alloc(1, 0);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc(1, 2);
    Field(st, 0) = Val_int(WSTOPSIG(status));
  }
  else {
    st = alloc(1, 1);
    Field(st, 0) = Val_int(WTERMSIG(status));
  }
  r[0] = st;
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = r[0];
  Pop_roots();
  return res;
}

value unix_wait()                /* ML */
{
  int pid, status;
  Push_roots(r, 1);
  pid = wait(&status);
  if (pid == -1) uerror("wait", Nothing);
  return alloc_process_status(pid, status);
}

#ifdef HAS_WAITPID

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

value unix_waitpid(flags, pid_req)
     value flags, pid_req;
{
  int pid, status;
  
  pid = waitpid(Int_val(pid_req), &status, 
                convert_flag_list(flags, wait_flag_table));
  if (pid == -1) uerror("waitpid", Nothing);
  return alloc_process_status(pid, status);
}

#else

value unix_waitpid() { invalid_argument("waitpid not implemented"); }

#endif
