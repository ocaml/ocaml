/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <callback.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unixsupport.h"
#include "cst2constr.h"
#include <errno.h>
#include <winsock.h>

/* Windows socket errors */

#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#define ENOTSOCK                WSAENOTSOCK
#define EDESTADDRREQ            WSAEDESTADDRREQ
#define EMSGSIZE                WSAEMSGSIZE
#define EPROTOTYPE              WSAEPROTOTYPE
#define ENOPROTOOPT             WSAENOPROTOOPT
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              WSAEOPNOTSUPP
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#define EADDRINUSE              WSAEADDRINUSE
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#define ENETDOWN                WSAENETDOWN
#define ENETUNREACH             WSAENETUNREACH
#define ENETRESET               WSAENETRESET
#define ECONNABORTED            WSAECONNABORTED
#define ECONNRESET              WSAECONNRESET
#define ENOBUFS                 WSAENOBUFS
#define EISCONN                 WSAEISCONN
#define ENOTCONN                WSAENOTCONN
#define ESHUTDOWN               WSAESHUTDOWN
#define ETOOMANYREFS            WSAETOOMANYREFS
#define ETIMEDOUT               WSAETIMEDOUT
#define ECONNREFUSED            WSAECONNREFUSED
#define ELOOP                   WSAELOOP
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

/* Errors not available under Win32 */

#define EACCESS (-1)

int error_table[] = {
  E2BIG, EACCESS, EAGAIN, EBADF, EBUSY, ECHILD, EDEADLK, EDOM,
  EEXIST, EFAULT, EFBIG, EINTR, EINVAL, EIO, EISDIR, EMFILE, EMLINK,
  ENAMETOOLONG, ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC,
  ENOSYS, ENOTDIR, ENOTEMPTY, ENOTTY, ENXIO, EPERM, EPIPE, ERANGE,
  EROFS, ESPIPE, ESRCH, EXDEV, EWOULDBLOCK, EINPROGRESS, EALREADY,
  ENOTSOCK, EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT,
  EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT,
  EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN, ENETUNREACH,
  ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN, ENOTCONN,
  ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, EHOSTDOWN,
  EHOSTUNREACH, ELOOP /*, EUNKNOWNERR */
};

static value * unix_error_exn = NULL;

void unix_error(errcode, cmdname, cmdarg)
     int errcode;
     char * cmdname;
     value cmdarg;
{
  value res;
  value name = Val_unit, arg = Val_unit;

  Begin_roots2 (name, arg);
    if (unix_error_exn == NULL) {
      unix_error_exn = caml_named_value("Unix.Unix_error");
      if (unix_error_exn == NULL)
        invalid_argument("Exception Unix.Unix_error not initialized, must link unix.cma");
    }
    arg = cmdarg == Nothing ? copy_string("") : cmdarg;
    name = copy_string(cmdname);
    res = alloc(4, 0);
    Field(res, 0) = *unix_error_exn;
    Field(res, 1) =
      cst_to_constr(errcode, error_table, sizeof(error_table)/sizeof(int),
                    sizeof(error_table)/sizeof(int));
    Field(res, 2) = name;
    Field(res, 3) = arg;
  End_roots();
  mlraise(res);
}

void uerror(cmdname, cmdarg)
     char * cmdname;
     value cmdarg;
{
  unix_error(errno, cmdname, cmdarg);
}
