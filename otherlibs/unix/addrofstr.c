/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_inet_addr_of_string(value s)
{
#ifdef HAS_INET_ATON
  struct in_addr address;
  if (inet_aton(String_val(s), &address) == 0)
    failwith("inet_addr_of_string");
  return alloc_inet_addr(address.s_addr);
#else
  unsigned int address;
  address = inet_addr(String_val(s));
  if (address == (unsigned int) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(address);
#endif
}

#else

CAMLprim value unix_inet_addr_of_string(value s)
{ invalid_argument("inet_addr_of_string not implemented"); }
  
#endif
