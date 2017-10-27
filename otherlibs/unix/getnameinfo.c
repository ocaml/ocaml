/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2004 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include <caml/misc.h>
#include "unixsupport.h"

#if defined(HAS_SOCKETS) && defined(HAS_IPV6)

#include "socketaddr.h"
#ifdef _WIN32
#include <Ws2tcpip.h>
#else
#include <sys/types.h>
#include <netdb.h>
#endif

#ifdef _WIN32
#define getnameinfo_os GetNameInfo
#else
#define getnameinfo_os getnameinfo
#endif

static int getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

CAMLprim value unix_getnameinfo(value vaddr, value vopts)
{
  CAMLparam0();
  CAMLlocal3(vhost, vserv, vres);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  char_os host[4096];
  char_os serv[1024];
  int opts, retcode;

  get_sockaddr(vaddr, &addr, &addr_len);
  opts = caml_convert_flag_list(vopts, getnameinfo_flag_table);
  caml_enter_blocking_section();
  retcode =
    getnameinfo_os((const struct sockaddr *) &addr.s_gen, addr_len,
                   host, sizeof(host)/sizeof(char_os), serv, sizeof(serv)/sizeof(char_os), opts);
  caml_leave_blocking_section();
  if (retcode != 0) caml_raise_not_found(); /* TODO: detailed error reporting? */
  vhost = caml_copy_string_of_os(host);
  vserv = caml_copy_string_of_os(serv);
  vres = caml_alloc_small(2, 0);
  Field(vres, 0) = vhost;
  Field(vres, 1) = vserv;
  CAMLreturn(vres);
}

#else

CAMLprim value unix_getnameinfo(value vaddr, value vopts)
{ caml_invalid_argument("getnameinfo not implemented"); }

#endif
