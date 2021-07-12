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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "unixsupport.h"
#include "cst2constr.h"
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

/* Not in POSIX (usually same as EAFNOSUPPORT) */
#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT (-1)
#endif
/* ESHUTDOWN and ETOOMANYREFS are Linux-specific */
#ifndef ESHUTDOWN
#define ESHUTDOWN (-1)
#endif
#ifndef ETOOMANYREFS
#define ETOOMANYREFS (-1)
#endif
/* Not in POSIX (known documentation issue) */
#ifndef EHOSTDOWN
#define EHOSTDOWN (-1)
#endif

int error_table[] = {
  E2BIG, EACCES, EAGAIN, EBADF, EBUSY, ECHILD, EDEADLK, EDOM,
  EEXIST, EFAULT, EFBIG, EINTR, EINVAL, EIO, EISDIR, EMFILE, EMLINK,
  ENAMETOOLONG, ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC,
  ENOSYS, ENOTDIR, ENOTEMPTY, ENOTTY, ENXIO, EPERM, EPIPE, ERANGE,
  EROFS, ESPIPE, ESRCH, EXDEV, EWOULDBLOCK, EINPROGRESS, EALREADY,
  ENOTSOCK, EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT,
  EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT,
  EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN, ENETUNREACH,
  ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN, ENOTCONN,
  ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, EHOSTDOWN,
  EHOSTUNREACH, ELOOP, EOVERFLOW /*, EUNKNOWNERR */
};

static const value * unix_error_exn = NULL;

value unix_error_of_code (int errcode)
{
  int errconstr;
  value err;

  errconstr =
      cst_to_constr(errcode, error_table, sizeof(error_table)/sizeof(int), -1);
  if (errconstr == Val_int(-1)) {
    err = caml_alloc_small(1, 0);
    Field(err, 0) = Val_int(errcode);
  } else {
    err = errconstr;
  }
  return err;
}

int code_of_unix_error (value error)
{
  if (Is_block(error)) {
    return Int_val(Field(error, 0));
  } else {
    return error_table[Int_val(error)];
  }
}

void unix_error(int errcode, const char *cmdname, value cmdarg)
{
  value res;
  value name = Val_unit, err = Val_unit, arg = Val_unit;

  Begin_roots3 (name, err, arg);
    arg = cmdarg == Nothing ? caml_copy_string("") : cmdarg;
    name = caml_copy_string(cmdname);
    err = unix_error_of_code (errcode);
    if (unix_error_exn == NULL) {
      unix_error_exn = caml_named_value("Unix.Unix_error");
      if (unix_error_exn == NULL)
        caml_invalid_argument("Exception Unix.Unix_error not initialized,"
                         " please link unix.cma");
    }
    res = caml_alloc_small(4, 0);
    Field(res, 0) = *unix_error_exn;
    Field(res, 1) = err;
    Field(res, 2) = name;
    Field(res, 3) = arg;
  End_roots();
  caml_raise(res);
}

void uerror(const char *cmdname, value cmdarg)
{
  unix_error(errno, cmdname, cmdarg);
}

void caml_unix_check_path(value path, const char * cmdname)
{
  if (! caml_string_is_c_safe(path)) unix_error(ENOENT, cmdname, path);
}

int unix_cloexec_default = 0;

int unix_cloexec_p(value cloexec)
{
  if (Is_some(cloexec))
    return Bool_val(Some_val(cloexec));
  else
    return unix_cloexec_default;
}

void unix_set_cloexec(int fd, char *cmdname, value cmdarg)
{
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1 ||
      fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1)
    uerror(cmdname, cmdarg);
}

void unix_clear_cloexec(int fd, char *cmdname, value cmdarg)
{
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1 ||
      fcntl(fd, F_SETFD, flags & ~FD_CLOEXEC) == -1)
    uerror(cmdname, cmdarg);
}
