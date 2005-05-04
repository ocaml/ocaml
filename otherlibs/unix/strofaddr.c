/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
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
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_string_of_inet_addr(value a)
{
  char * res;
#ifdef HAS_IPV6
  char buffer[64];
  if (string_length(a) == 16)
    res = (char *)
      inet_ntop(AF_INET6, (const void *) &GET_INET6_ADDR(a),
                buffer, sizeof(buffer));
  else
    res = (char *)
      inet_ntop(AF_INET, (const void *) &GET_INET_ADDR(a),
                buffer, sizeof(buffer));
#else
  res = inet_ntoa(GET_INET_ADDR(a));
#endif
  if (res == NULL) uerror("string_of_inet_addr", Nothing);
  return copy_string(res);
}

#else

CAMLprim value unix_string_of_inet_addr(value a)
{ invalid_argument("string_of_inet_addr not implemented"); }

#endif
