/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_string_of_inet_addr(value a) /* ML */
{
  struct in_addr address;
  address.s_addr = GET_INET_ADDR(a);
  return copy_string(inet_ntoa(address));
}

#else

value unix_string_of_inet_addr(value a)
{ invalid_argument("string_of_inet_addr not implemented"); }

#endif
