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

#include <mlvalues.h>
#include <windows.h>
#include "unixsupport.h"

#if 0

/* This works only under Windows NT, but not 95 */

value win_set_close_on_exec(fd)             /* ML */
     value fd;
{
  HANDLE h;

  h = (HANDLE) _get_osfhandle(Int_val(fd)) ;
  if (h == (HANDLE) -1 ||
      ! SetHandleInformation(h, HANDLE_FLAG_INHERIT, 0)) {
    _dosmaperr(GetLastError());
    uerror("set_close_on_exec", Nothing);
  }
  return Val_unit;
}

value win_clear_close_on_exec(fd)             /* ML */
     value fd;
{
  HANDLE h;
  h = (HANDLE) _get_osfhandle(Int_val(fd)) ;
  if (h == (HANDLE) -1 ||
      ! SetHandleInformation(h, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)) {
    _dosmaperr(GetLastError());
    uerror("clear_close_on_exec", Nothing);
  }
  return Val_unit;
}

#else

/* This works on Win 95, but is a terrible hack.
   Bug: the opening flags of the file descriptor (O_APPEND, O_TEXT) are lost. */

static int win_open_osfhandle2(handle, flags, reqd_fd)
     HANDLE handle;
     int flags;
     int reqd_fd;
{
  int fd, retcode;
  HANDLE new_handle;

  fd = _open_osfhandle((long)handle, flags);
  if (fd == -1)
    return -1;
  if (fd == reqd_fd)
    return 0;			/* Got it! */
  /* Make a copy of the handle, since we're going to close "handle" when
     we close "fd". */
  if (! DuplicateHandle(GetCurrentProcess(), handle,
			GetCurrentProcess(), &new_handle,
			0L, FALSE, DUPLICATE_SAME_ACCESS)) {
    _dosmaperr(GetLastError());
    return -1;
  }
  /* Keep fd open during the recursive call, thus forcing _open_osfhandle
     to return reqd_fd eventually. */
  retcode = win_open_osfhandle2(new_handle, flags, reqd_fd);
  close(fd);		     /* Also closes "handle" */
  return retcode;
}

static int win_set_noninherit(fd)
     int fd;
{
  HANDLE oldh, newh;
  oldh = (HANDLE) _get_osfhandle(fd);
  if (oldh == (HANDLE) -1) return -1;
  if (! DuplicateHandle(GetCurrentProcess(), oldh,
			GetCurrentProcess(), &newh,
			0L, FALSE, DUPLICATE_SAME_ACCESS)) {
    _dosmaperr(GetLastError());
    return -1;
  }
  if (close(fd) == -1) return -1;
  return win_open_osfhandle2(newh, 0, fd);
}

value win_set_close_on_exec(vfd)             /* ML */
     value vfd;
{
  if (win_set_noninherit(Int_val(vfd)) == -1)
    uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

value win_clear_close_on_exec(vfd)             /* ML */
     value vfd;
{
  int fd, newfd;

  fd = Int_val(vfd);
  newfd = dup(fd);
  if (newfd == -1) {
    uerror("clear_close_on_exec", Nothing);
  }
  if (dup2(newfd, fd) == -1) {
    close(newfd);
    uerror("clear_close_on_exec", Nothing);
  }
  close(newfd);
  return Val_unit;
}

#endif
