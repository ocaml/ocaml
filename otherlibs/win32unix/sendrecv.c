/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include "socketaddr.h"

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value unix_recv(sock, buff, ofs, len, flags) /* ML */
     value sock, buff, ofs, len, flags;
{
  int ret;
  buff = unix_freeze_buffer(buff);
  enter_blocking_section();
  ret = recv((SOCKET) _get_osfhandle(Int_val(sock)),
             &Byte(buff, Long_val(ofs)), Int_val(len),
             convert_flag_list(flags, msg_flag_table));
  leave_blocking_section();
  if (ret == -1) uerror("recv", Nothing);
  return Val_int(ret);
}

value unix_recvfrom(sock, buff, ofs, len, flags) /* ML */
     value sock, buff, ofs, len, flags;
{
  int retcode;
  value res;
  value adr = Val_unit;

  Begin_root (adr);
    buff = unix_freeze_buffer(buff);     /* XXX Xavier regarde ca */
    sock_addr_len = sizeof(sock_addr);
    enter_blocking_section();
    retcode = recvfrom((SOCKET) _get_osfhandle(Int_val(sock)),
                       &Byte(buff, Long_val(ofs)), Int_val(len),
                       convert_flag_list(flags, msg_flag_table),
                       &sock_addr.s_gen, &sock_addr_len);
    leave_blocking_section();
    if (retcode == -1) uerror("recvfrom", Nothing);
    adr = alloc_sockaddr();
    res = alloc_tuple(2);
    Field(res, 0) = Val_int(retcode);
    Field(res, 1) = adr;
  End_roots();
  return res;
}

value unix_send(sock, buff, ofs, len, flags) /* ML */
     value sock, buff, ofs, len, flags;
{
  int ret;
  buff = unix_freeze_buffer(buff);
  enter_blocking_section();
  ret = send((SOCKET) _get_osfhandle(Int_val(sock)),
             &Byte(buff, Long_val(ofs)), Int_val(len),
             convert_flag_list(flags, msg_flag_table));
  leave_blocking_section();
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

value unix_sendto_native(sock, buff, ofs, len, flags, dest)
     value sock, buff, ofs, len, flags, dest;
{
  int ret;
  get_sockaddr(dest);
  buff = unix_freeze_buffer(buff);
  enter_blocking_section();
  ret = sendto((SOCKET) _get_osfhandle(Int_val(sock)),
               &Byte(buff, Long_val(ofs)),
               Int_val(len), convert_flag_list(flags, msg_flag_table),
               &sock_addr.s_gen, sock_addr_len);
  leave_blocking_section();
  if (ret == -1) uerror("sendto", Nothing);
  return Val_int(ret);
}

value unix_sendto(argv, argc)    /* ML */
     value * argv;
     int argc;
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
