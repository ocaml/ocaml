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

CAMLprim value caml_unix_recv(value sock, value buff, value ofs, value len,
                         value flags)
{
  CAMLparam1(buff);
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  caml_enter_blocking_section();
  ret = recv(Int_val(sock), iobuf, (int) numbytes, cv_flags);
  caml_leave_blocking_section();
  if (ret == -1) caml_uerror("recv", Nothing);
  memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  CAMLreturn(Val_int(ret));
}

CAMLprim value caml_unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags)
{
  CAMLparam1(buff);
  CAMLlocal1(adr);
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  value res;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  addr_len = sizeof(addr);
  caml_enter_blocking_section();
  ret = recvfrom(Int_val(sock), iobuf, (int) numbytes, cv_flags,
                 &addr.s_gen, &addr_len);
  caml_leave_blocking_section();
  if (ret == -1) caml_uerror("recvfrom", Nothing);
  memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  adr = caml_unix_alloc_sockaddr(&addr, addr_len, -1);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(ret);
  Field(res, 1) = adr;
  CAMLreturn(res);
}

CAMLprim value caml_unix_send(value sock, value buff, value ofs, value len,
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
  if (ret == -1) caml_uerror("send", Nothing);
  return Val_int(ret);
}

CAMLprim value caml_unix_sendto_native(value sock, value buff, value ofs,
                                       value len, value flags, value dest)
{
  int ret, cv_flags;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  union sock_addr_union addr;
  socklen_param_type addr_len;

  cv_flags = caml_convert_flag_list(flags, msg_flag_table);
  caml_unix_get_sockaddr(dest, &addr, &addr_len);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  caml_enter_blocking_section();
  ret = sendto(Int_val(sock), iobuf, (int) numbytes, cv_flags,
               &addr.s_gen, addr_len);
  caml_leave_blocking_section();
  if (ret == -1) caml_uerror("sendto", Nothing);
  return Val_int(ret);
}

CAMLprim value caml_unix_sendto(value *argv, int argc)
{
  return caml_unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

#else

CAMLprim value caml_unix_recv(value sock, value buff, value ofs, value len,
                         value flags)
{ caml_invalid_argument("recv not implemented"); }

CAMLprim value caml_unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags)
{ caml_invalid_argument("recvfrom not implemented"); }

CAMLprim value caml_unix_send(value sock, value buff, value ofs, value len,
                         value flags)
{ caml_invalid_argument("send not implemented"); }

CAMLprim value caml_unix_sendto_native(value sock, value buff, value ofs,
                                       value len, value flags, value dest)
{ caml_invalid_argument("sendto not implemented"); }

CAMLprim value caml_unix_sendto(value *argv, int argc)
{ caml_invalid_argument("sendto not implemented"); }

#endif
