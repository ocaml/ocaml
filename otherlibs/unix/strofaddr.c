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
#include "caml/unixsupport.h"

#ifdef HAS_SOCKETS

#include "caml/socketaddr.h"

CAMLprim value caml_unix_string_of_inet_addr(value a)
{
  char * res;
#ifdef HAS_IPV6
#ifdef _WIN32
  char buffer[64];
  struct sockaddr_storage addr;
  socklen_t len;
  int retcode;
  memset(&addr, 0, sizeof(struct sockaddr_storage));
  if (caml_string_length(a) == 16) {
    addr.ss_family = AF_INET6;
    ((struct sockaddr_in6 *) &addr)->sin6_addr = GET_INET6_ADDR(a);
    len = sizeof(struct sockaddr_in6);
  } else {
    addr.ss_family = AF_INET;
    ((struct sockaddr_in *) &addr)->sin_addr = GET_INET_ADDR(a);
    len = sizeof(struct sockaddr_in);
  }
  retcode = getnameinfo((struct sockaddr *) &addr, len, buffer, sizeof(buffer),
                        NULL, 0, NI_NUMERICHOST);
  if (retcode != 0)
    res = NULL;
  else
    res = buffer;
#else
  char buffer[64];
  if (caml_string_length(a) == 16)
    res = (char *)
      inet_ntop(AF_INET6, (const void *) &GET_INET6_ADDR(a),
                buffer, sizeof(buffer));
  else
    res = (char *)
      inet_ntop(AF_INET, (const void *) &GET_INET_ADDR(a),
                buffer, sizeof(buffer));
#endif
#else
  res = inet_ntoa(GET_INET_ADDR(a));
#endif
  if (res == NULL) caml_uerror("string_of_inet_addr", Nothing);
  return caml_copy_string(res);
}

#else

CAMLprim value caml_unix_string_of_inet_addr(value a)
{ caml_invalid_argument("string_of_inet_addr not implemented"); }

#endif
