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

extern int _set_osfhnd(int fd, long value);

static int win_realloc_handle(fd, inherit)
        int fd;
        BOOL inherit;
{
  HANDLE oldh, newh;

  oldh = (HANDLE) _get_osfhandle(fd);
  if (oldh == (HANDLE) -1) return -1;
  if (! DuplicateHandle(GetCurrentProcess(), oldh,
	                    GetCurrentProcess(), &newh,
						0L, inherit, DUPLICATE_SAME_ACCESS)) {
	_dosmaperr(GetLastError());
    return -1;
  }
  _close(fd);
  _set_osfhnd(fd, (long) newh);
  return 0;
}

value win_set_close_on_exec(fd)             /* ML */
     value fd;
{
  if (win_realloc_handle(Int_val(fd), FALSE) == -1) {
    uerror("set_close_on_exec", Nothing);
  }
  return Val_unit;
}

value win_clear_close_on_exec(fd)             /* ML */
     value fd;
{
  if (win_realloc_handle(Int_val(fd), TRUE) == -1) {
    uerror("clear_close_on_exec", Nothing);
  }
  return Val_unit;
}

#endif
