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
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_inet_addr_of_string(value s) /* ML */
{
  unsigned int address;
  address = inet_addr(String_val(s));
  if (address == (unsigned int) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(address);
}

#else

value unix_inet_addr_of_string(value s)
{ invalid_argument("inet_addr_of_string not implemented"); }
  
#endif
