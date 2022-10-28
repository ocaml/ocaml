/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_UNIXSUPPORT_H
#define CAML_UNIXSUPPORT_H

#include <caml/misc.h>

#ifdef _WIN32 /* Windows */
#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <wspiapi.h>
#else /* Unix */
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;                   /* Real windows handle */
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  _Atomic int crt_fd;     /* C runtime descriptor */
  unsigned int flags_fd;  /* See FLAGS_FD_* */
};

/* Information stored in flags_fd, describing more precisely the socket
 * and its status. The whole flags_fd is initialized to 0.
 */

/* Blocking or nonblocking.  By default a filedescr is in blocking state */
#define FLAGS_FD_IS_BLOCKING (1<<0)


#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define Flags_fd_val(v) (((struct filedescr *) Data_custom_val(v))->flags_fd)

extern value caml_win32_alloc_handle(HANDLE);
extern value caml_win32_alloc_socket(SOCKET);

#define NO_CRT_FD (-1)
#define GETTING_CRT_FD (-2)

/* Returns the C file descriptor associated with the handle, allocating it if
   necessary. */
extern int caml_win32_CRT_fd_of_filedescr(value handle);

/* Returns the C file descriptor associated with the handle, or NO_CRT_FD
   if none was allocated. The crt_fd field should only be accessed through this
   function and not directly, to avoid race conditions with
   win_CRT_fd_of_filedescr. */
extern int caml_win32_get_CRT_fd(value handle);

// Export this macro as an alias for the getter function, for compatibility
#define CRT_fd_val caml_win32_get_CRT_fd

extern SOCKET caml_win32_socket(int domain, int type, int protocol,
                                LPWSAPROTOCOL_INFO info,
                                BOOL inherit);

extern void caml_win32_maperr(DWORD errcode);

/* There are 11644473600 seconds between 1 January 1601 (the NT Epoch) and 1
 * January 1970 (the Unix Epoch). FILETIME is measured in 100ns ticks.
 */
#define CAML_NT_EPOCH_100ns_TICKS 116444736000000000ULL

#endif /* _WIN32 */

#define Nothing ((value) 0)

extern value caml_unix_error_of_code (int errcode);
extern int caml_unix_code_of_unix_error (value error);

CAMLnoreturn_start
extern void caml_unix_error (int errcode, const char * cmdname, value arg)
CAMLnoreturn_end;

CAMLnoreturn_start
extern void caml_uerror (const char * cmdname, value arg)
CAMLnoreturn_end;

extern void caml_unix_check_path(value path, const char * cmdname);

#define UNIX_BUFFER_SIZE 65536

#define DIR_Val(v) *((DIR **) &Field(v, 0))

extern char_os ** caml_unix_cstringvect(value arg, char * cmdname);
extern void caml_unix_cstringvect_free(char_os **);

extern int caml_unix_cloexec_default;
extern int caml_unix_cloexec_p(value cloexec);

#ifdef _WIN32
extern int caml_win32_set_inherit(HANDLE fd, BOOL inherit);
/* This is a best effort, not guaranteed to work, so don't fail on error */
#define caml_win32_set_cloexec(fd, cloexec) \
  caml_win32_set_inherit((fd), ! caml_unix_cloexec_p((cloexec)))
#else
extern void caml_unix_set_cloexec(int fd, char * cmdname, value arg);
extern void caml_unix_clear_cloexec(int fd, char * cmdname, value arg);
#endif /* _WIN32 */

/* Compatibility definitions for the pre-5.0 names of these functions */
#ifndef CAML_BUILDING_UNIX
#ifdef _WIN32
#define win_alloc_handle caml_win32_alloc_handle
#define win_alloc_socket caml_win32_alloc_socket
#define win_CRT_fd_of_filedescr caml_win32_CRT_fd_of_filedescr
#define win32_maperr caml_win32_maperr
#endif /* _WIN32 */

#define unix_error_of_code caml_unix_error_of_code
#define code_of_unix_error caml_unix_code_of_unix_error

#define uerror caml_uerror
#define unix_error caml_unix_error

#define cstringvect caml_unix_cstringvect
#define cstringvect_free caml_unix_cstringvect_free

#define unix_cloexec_default caml_unix_cloexec_default
#define unix_cloexec_p caml_unix_cloexec_p

#ifdef _WIN32
#define win_set_inherit caml_win32_set_inherit
#define win_set_cloexec caml_win32_set_cloexec
#else
#define unix_set_cloexec caml_unix_set_cloexec
#define unix_clear_cloexec caml_unix_clear_cloexec
#endif /* _WIN32 */
#endif /* CAML_BUILDING_UNIX */

#ifdef __cplusplus
}
#endif

#ifdef _WIN32
#define EXECV_CAST (const char_os * const *)
#else
#define EXECV_CAST
#endif

#endif /* CAML_UNIXSUPPORT_H */
