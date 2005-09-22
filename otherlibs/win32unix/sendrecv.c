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

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

CAMLprim value unix_recv(value sock, value buff, value ofs, value len, value flags)
{
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buff);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    enter_blocking_section();
    ret = recv(Socket_val(sock), iobuf, (int) numbytes,
               convert_flag_list(flags, msg_flag_table));
    leave_blocking_section();
    if (ret == -1) {
      win32_maperr(WSAGetLastError());
      uerror("recv", Nothing);
    }
    memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  End_roots();
  return Val_int(ret);
}

CAMLprim value unix_recvfrom(value sock, value buff, value ofs, value len, value flags)
{
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  value res;
  value adr = Val_unit;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  Begin_roots2 (buff, adr);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    addr_len = sizeof(sock_addr);
    enter_blocking_section();
    ret = recvfrom(Socket_val(sock),
                   iobuf, (int) numbytes,
                   convert_flag_list(flags, msg_flag_table),
                   &addr.s_gen, &addr_len);
    leave_blocking_section();
    if (ret == -1) {
      win32_maperr(WSAGetLastError());
      uerror("recvfrom", Nothing);
    }
    memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
    adr = alloc_sockaddr(&addr, addr_len, -1);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(ret);
    Field(res, 1) = adr;
  End_roots();
  return res;
}

CAMLprim value unix_send(value sock, value buff, value ofs, value len, value flags)
{
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  enter_blocking_section();
  ret = send(Socket_val(sock), iobuf, (int) numbytes,
             convert_flag_list(flags, msg_flag_table));
  leave_blocking_section();
  if (ret == -1) {
    win32_maperr(WSAGetLastError());
    uerror("send", Nothing);
  }
  return Val_int(ret);
}

value unix_sendto_native(value sock, value buff, value ofs, value len, value flags, value dest)
{
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(dest, &addr, &addr_len);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  enter_blocking_section();
  ret = sendto(Socket_val(sock),
               iobuf, (int) numbytes,
               convert_flag_list(flags, msg_flag_table),
               &addr.s_gen, addr_len);
  leave_blocking_section();
  if (ret == -1) {
    win32_maperr(WSAGetLastError());
    uerror("sendto", Nothing);
  }
  return Val_int(ret);
}

CAMLprim value unix_sendto(argv, argc)
     value * argv;
     int argc;
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
