#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unix.h"
#include "cst2constr.h"
#include <errno.h>

#ifndef EPERM
#define EPERM (-1)
#endif
#ifndef ENOENT
#define ENOENT (-1)
#endif
#ifndef ESRCH
#define ESRCH (-1)
#endif
#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EIO
#define EIO (-1)
#endif
#ifndef ENXIO
#define ENXIO (-1)
#endif
#ifndef E2BIG
#define E2BIG (-1)
#endif
#ifndef ENOEXEC
#define ENOEXEC (-1)
#endif
#ifndef EBADF
#define EBADF (-1)
#endif
#ifndef ECHILD
#define ECHILD (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef ENOMEM
#define ENOMEM (-1)
#endif
#ifndef EACCES
#define EACCES (-1)
#endif
#ifndef EFAULT
#define EFAULT (-1)
#endif
#ifndef ENOTBLK
#define ENOTBLK (-1)
#endif
#ifndef EBUSY
#define EBUSY (-1)
#endif
#ifndef EEXIST
#define EEXIST (-1)
#endif
#ifndef EXDEV
#define EXDEV (-1)
#endif
#ifndef ENODEV
#define ENODEV (-1)
#endif
#ifndef ENOTDIR
#define ENOTDIR (-1)
#endif
#ifndef EISDIR
#define EISDIR (-1)
#endif
#ifndef EINVAL
#define EINVAL (-1)
#endif
#ifndef ENFILE
#define ENFILE (-1)
#endif
#ifndef EMFILE
#define EMFILE (-1)
#endif
#ifndef ENOTTY
#define ENOTTY (-1)
#endif
#ifndef ETXTBSY
#define ETXTBSY (-1)
#endif
#ifndef EFBIG
#define EFBIG (-1)
#endif
#ifndef ENOSPC
#define ENOSPC (-1)
#endif
#ifndef ESPIPE
#define ESPIPE (-1)
#endif
#ifndef EROFS
#define EROFS (-1)
#endif
#ifndef EMLINK
#define EMLINK (-1)
#endif
#ifndef EPIPE
#define EPIPE (-1)
#endif
#ifndef EDOM
#define EDOM (-1)
#endif
#ifndef ERANGE
#define ERANGE (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif
#ifndef EINPROGRESS
#define EINPROGRESS (-1)
#endif
#ifndef EALREADY
#define EALREADY (-1)
#endif
#ifndef ENOTSOCK
#define ENOTSOCK (-1)
#endif
#ifndef EDESTADDRREQ
#define EDESTADDRREQ (-1)
#endif
#ifndef EMSGSIZE
#define EMSGSIZE (-1)
#endif
#ifndef EPROTOTYPE
#define EPROTOTYPE (-1)
#endif
#ifndef ENOPROTOOPT
#define ENOPROTOOPT (-1)
#endif
#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT (-1)
#endif
#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT (-1)
#endif
#ifndef EOPNOTSUPP
#define EOPNOTSUPP (-1)
#endif
#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT (-1)
#endif
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT (-1)
#endif
#ifndef EADDRINUSE
#define EADDRINUSE (-1)
#endif
#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL (-1)
#endif
#ifndef ENETDOWN
#define ENETDOWN (-1)
#endif
#ifndef ENETUNREACH
#define ENETUNREACH (-1)
#endif
#ifndef ENETRESET
#define ENETRESET (-1)
#endif
#ifndef ECONNABORTED
#define ECONNABORTED (-1)
#endif
#ifndef ECONNRESET
#define ECONNRESET (-1)
#endif
#ifndef ENOBUFS
#define ENOBUFS (-1)
#endif
#ifndef EISCONN
#define EISCONN (-1)
#endif
#ifndef ENOTCONN
#define ENOTCONN (-1)
#endif
#ifndef ESHUTDOWN
#define ESHUTDOWN (-1)
#endif
#ifndef ETOOMANYREFS
#define ETOOMANYREFS (-1)
#endif
#ifndef ETIMEDOUT
#define ETIMEDOUT (-1)
#endif
#ifndef ECONNREFUSED
#define ECONNREFUSED (-1)
#endif
#ifndef ELOOP
#define ELOOP (-1)
#endif
#ifndef ENAMETOOLONG
#define ENAMETOOLONG (-1)
#endif
#ifndef EHOSTDOWN
#define EHOSTDOWN (-1)
#endif
#ifndef EHOSTUNREACH
#define EHOSTUNREACH (-1)
#endif
#ifndef ENOTEMPTY
#define ENOTEMPTY (-1)
#endif
#ifndef EPROCLIM
#define EPROCLIM (-1)
#endif
#ifndef EUSERS
#define EUSERS (-1)
#endif
#ifndef EDQUOT
#define EDQUOT (-1)
#endif
#ifndef ESTALE
#define ESTALE (-1)
#endif
#ifndef EREMOTE
#define EREMOTE (-1)
#endif
#ifndef EIDRM
#define EIDRM (-1)
#endif
#ifndef EDEADLK
#define EDEADLK (-1)
#endif
#ifndef ENOLCK
#define ENOLCK (-1)
#endif
#ifndef ENOSYS
#define ENOSYS (-1)
#endif

int error_table[] = {
  0, EPERM, ENOENT, ESRCH, EINTR, EIO, ENXIO, E2BIG, ENOEXEC, EBADF,
  ECHILD, EAGAIN, ENOMEM, EACCES, EFAULT, ENOTBLK, EBUSY, EEXIST, EXDEV,
  ENODEV, ENOTDIR, EISDIR, EINVAL, ENFILE, EMFILE, ENOTTY, ETXTBSY,
  EFBIG, ENOSPC, ESPIPE, EROFS, EMLINK, EPIPE, EDOM, ERANGE,
  EWOULDBLOCK, EINPROGRESS, EALREADY, ENOTSOCK, EDESTADDRREQ, EMSGSIZE,
  EPROTOTYPE, ENOPROTOOPT, EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP,
  EPFNOSUPPORT, EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN,
  ENETUNREACH, ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN,
  ENOTCONN, ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, ELOOP,
  ENAMETOOLONG, EHOSTDOWN, EHOSTUNREACH, ENOTEMPTY, EPROCLIM, EUSERS,
  EDQUOT, ESTALE, EREMOTE, EIDRM, EDEADLK, ENOLCK, ENOSYS
  /*, EUNKNOWNERROR */
};

static value unix_error_exn;

value unix_register_error(exnval)
     value exnval;
{
  unix_error_exn = Field(exnval, 0);
  register_global_root(&unix_error_exn);
  return Val_unit;
}

void unix_error(errcode, cmdname, cmdarg)
     int errcode;
     char * cmdname;
     value cmdarg;
{
  value res;
  Push_roots(r, 2);
#define name r[0]
#define arg r[1]
  arg = cmdarg == Nothing ? copy_string("") : cmdarg;
  name = copy_string(cmdname);
  res = alloc(4, 0);
  Field(res, 0) = unix_error_exn;
  Field(res, 1) =
    cst_to_constr(errcode, error_table, sizeof(error_table)/sizeof(int),
                  sizeof(error_table)/sizeof(int));
  Field(res, 2) = name;
  Field(res, 3) = arg;
  Pop_roots();
  mlraise(res);
}

void uerror(cmdname, cmdarg)
     char * cmdname;
     value cmdarg;
{
  unix_error(errno, cmdname, cmdarg);
}
