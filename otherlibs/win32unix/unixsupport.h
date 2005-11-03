/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
/* Include io.h in current dir, which is a copy of the system's io.h,
   not io.h from ../../byterun */
/*#include "io.h"*/
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#include <winsock.h>

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;
};

#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define CRT_fd_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)

extern value win_alloc_handle_or_socket(HANDLE);
extern value win_alloc_handle(HANDLE);
extern value win_alloc_socket(SOCKET);

#define NO_CRT_FD (-1)
#define Nothing ((value) 0)

extern void win32_maperr(DWORD errcode);
extern void unix_error (int errcode, char * cmdname, value arg);
extern void uerror (char * cmdname, value arg);
extern value unix_freeze_buffer (value);

#define UNIX_BUFFER_SIZE 16384
