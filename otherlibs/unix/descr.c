/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2018 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include "unixsupport.h"

#include <errno.h>

#ifndef _WIN32
#include <fcntl.h>
#endif

CAMLprim value unix_descr_of_fd (value fd)
{
  /* fcntl and _get_osfhandle will set errno to EBADF for a closed fd */
#ifdef _WIN32
  if (_get_osfhandle(Int_val(fd)) == -1)
    uerror("descr_of_fd", Nothing);

  return win_handle_fd(fd);
#else
  if (fcntl(Int_val(fd), F_GETFL) == -1)
    uerror("descr_of_fd", Nothing);

  return fd;
#endif
}

#ifdef _WIN32
CAMLprim value unix_descr_of_os (value vhandle)
{
  HANDLE handle = (HANDLE)Nativeint_val(vhandle);
  DWORD dwFlags;

  if (!GetHandleInformation(handle, &dwFlags)) {
    errno = EBADF;
    uerror("descr_of_os", Nothing);
  }

  return win_alloc_handle_or_socket(handle);
}
#endif
