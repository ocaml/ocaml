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

#include <misc.h>
#include <sys/types.h>
#include <winsock.h>

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_in s_inet;
};

extern union sock_addr_union sock_addr;

#ifdef HAS_SOCKLEN_T
typedef socklen_t socklen_param_type;
#else
typedef int socklen_param_type;
#endif

void get_sockaddr (value mladdr,
                   union sock_addr_union * addr /*out*/,
                   socklen_param_type * addr_len /*out*/);
value alloc_sockaddr (union sock_addr_union * addr /*in*/,
                      socklen_param_type addr_len);
value alloc_inet_addr (uint32 inaddr);

#define GET_INET_ADDR(v) (*((uint32 *) (v)))
