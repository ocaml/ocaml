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
#include <fail.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

extern unsigned long inet_addr();

value unix_inet_addr_of_string(s) /* ML */
     value s;
{
  unsigned long address;
  address = inet_addr(String_val(s));
  if (address == (unsigned long) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(address);
}

#else

value unix_inet_addr_of_string()
{ invalid_argument("inet_addr_of_string not implemented"); }
  
#endif
