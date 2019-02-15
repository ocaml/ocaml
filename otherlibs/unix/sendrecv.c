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

#include <string.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS
#include "socketaddr.h"

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

CAMLprim value unix_recv(value sock, value buff, value ofs, value len,
                         value flags)
{
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  Begin_root (buff);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    caml_enter_blocking_section();
    ret = recv(Int_val(sock), iobuf, (int) numbytes, cv_flags);
    caml_leave_blocking_section();
    if (ret == -1) uerror("recv", Nothing);
    memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  End_roots();
  return Val_int(ret);
}

CAMLprim value unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags)
{
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  value res;
  value adr = Val_unit;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  Begin_roots2 (buff, adr);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    addr_len = sizeof(addr);

    // The sender must be in the same address family, but if the
    // sender is an unnamed AF_UNIX socket, recvfrom writes nothing
    // to `addr.s_gen` and sets addr_len to 0. alloc_sockaddr handles
    // a zero-length path, but only if sa_family is set correctly.
    //
    // We could use getsockname here to copy the sa_family of the receiver,
    // but since recvfrom only leaves the address uninitialized for an
    // unnamed AF_UNIX sender, we can just set it blindly.
#ifndef _WIN32
    addr.s_gen.sa_family = AF_UNIX;
#endif

    caml_enter_blocking_section();
    ret = recvfrom(Int_val(sock), iobuf, (int) numbytes, cv_flags,
                   &addr.s_gen, &addr_len);
    caml_leave_blocking_section();
    if (ret == -1) uerror("recvfrom", Nothing);
    /* fprintf(stderr, "Address family %d, len = %d\n", addr.s_gen.sa_family, addr_len); */
    memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
    adr = alloc_sockaddr(&addr, addr_len, -1);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_int(ret);
    Field(res, 1) = adr;
  End_roots();
  return res;
}

CAMLprim value unix_send(value sock, value buff, value ofs, value len,
                         value flags)
{
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  caml_enter_blocking_section();
  ret = send(Int_val(sock), iobuf, (int) numbytes, cv_flags);
  caml_leave_blocking_section();
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

CAMLprim value unix_sendto_native(value sock, value buff, value ofs, value len,
                                  value flags, value dest)
{
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  union sock_addr_union addr;
  socklen_param_type addr_len;

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  get_sockaddr(dest, &addr, &addr_len);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  caml_enter_blocking_section();
  ret = sendto(Int_val(sock), iobuf, (int) numbytes, cv_flags,
               &addr.s_gen, addr_len);
  caml_leave_blocking_section();
  if (ret == -1) uerror("sendto", Nothing);
  return Val_int(ret);
}

CAMLprim value unix_sendto(value *argv, int argc)
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

#else

CAMLprim value unix_recv(value sock, value buff, value ofs, value len,
                         value flags)
{ caml_invalid_argument("recv not implemented"); }

CAMLprim value unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags)
{ caml_invalid_argument("recvfrom not implemented"); }

CAMLprim value unix_send(value sock, value buff, value ofs, value len,
                         value flags)
{ caml_invalid_argument("send not implemented"); }

CAMLprim value unix_sendto_native(value sock, value buff, value ofs, value len,
                                  value flags, value dest)
{ caml_invalid_argument("sendto not implemented"); }

CAMLprim value unix_sendto(value *argv, int argc)
{ caml_invalid_argument("sendto not implemented"); }

#endif
