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
  };
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
    uerror("set_close_on_exec", Nothing);
  };
  return Val_unit;
}

