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
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

extern int caml_unix_socket_domain_table[], caml_unix_socket_type_table[];

CAMLprim value caml_unix_socketpair(value cloexec, value domain,
                               value type, value proto)
{
  int sv[2];
  value res;
  int ty = caml_unix_socket_type_table[Int_val(type)];
#ifdef SOCK_CLOEXEC
  if (caml_unix_cloexec_p(cloexec)) ty |= SOCK_CLOEXEC;
#endif
  if (socketpair(caml_unix_socket_domain_table[Int_val(domain)],
                 ty, Int_val(proto), sv) == -1)
    caml_uerror("socketpair", Nothing);
#ifndef SOCK_CLOEXEC
  if (caml_unix_cloexec_p(cloexec)) {
    caml_unix_set_cloexec(sv[0], "socketpair", Nothing);
    caml_unix_set_cloexec(sv[1], "socketpair", Nothing);
  }
#endif
  res = caml_alloc_small(2, 0);
  Field(res,0) = Val_int(sv[0]);
  Field(res,1) = Val_int(sv[1]);
  return res;
}

#else

CAMLprim value caml_unix_socketpair(value cloexec, value domain, value type,
                               value proto)
{ caml_invalid_argument("socketpair not implemented"); }

#endif
