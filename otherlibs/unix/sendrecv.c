/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"

#ifdef HAS_SOCKETS
#include "socketaddr.h"
#endif

#if defined(HAS_SOCKETS) && defined(MSG_OOB) && defined(MSG_DONTROUTE) && defined(MSG_PEEK)

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value unix_recv(sock, buff, ofs, len, flags) /* ML */
     value sock, buff, ofs, len, flags;
{
  int ret;
  enter_blocking_section();
  ret = recv(Int_val(sock), &Byte(buff, Long_val(ofs)), Int_val(len),
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
  Push_roots(a, 1);

  sock_addr_len = sizeof(sock_addr);
  enter_blocking_section();
  retcode = recvfrom(Int_val(sock), &Byte(buff, Long_val(ofs)), Int_val(len),
                     convert_flag_list(flags, msg_flag_table),
                     &sock_addr.s_gen, &sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) uerror("recvfrom", Nothing);
  a[0] = alloc_sockaddr();
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(retcode);
  Field(res, 1) = a[0];
  Pop_roots();
  return res;
}

value unix_send(sock, buff, ofs, len, flags) /* ML */
     value sock, buff, ofs, len, flags;
{
  int ret;
  enter_blocking_section();
  ret = send(Int_val(sock), &Byte(buff, Long_val(ofs)), Int_val(len),
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
  enter_blocking_section();
  ret = sendto(Int_val(sock), &Byte(buff, Long_val(ofs)),
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

#else

value unix_recv() { invalid_argument("recv not implemented"); }

value unix_recvfrom() { invalid_argument("recvfrom not implemented"); }

value unix_send() { invalid_argument("send not implemented"); }

value unix_sendto_native() { invalid_argument("sendto not implemented"); }

value unix_sendto() { invalid_argument("sendto not implemented"); }

#endif
