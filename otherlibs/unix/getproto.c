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
#include <memory.h>
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#ifndef _WIN32
#include <netdb.h>
#else
#include <winsock.h>
#endif

static value alloc_proto_entry(entry)
     struct protoent * entry;
{
  value res;
  value name = Val_unit, aliases = Val_unit;

  Begin_roots2 (name, aliases);
    name = copy_string(entry->p_name);
    aliases = copy_string_array(entry->p_aliases);
    res = alloc_tuple(3);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(entry->p_proto);
  End_roots();
  return res;
}

value unix_getprotobyname(name)  /* ML */
     value name;
{
  struct protoent * entry;
  entry = getprotobyname(String_val(name));
  if (entry == (struct protoent *) NULL) raise_not_found();
  return alloc_proto_entry(entry);
}

value unix_getprotobynumber(proto) /* ML */
     value proto;
{
  struct protoent * entry;
  entry = getprotobynumber(Int_val(proto));
  if (entry == (struct protoent *) NULL) raise_not_found();
  return alloc_proto_entry(entry);
}

#else

value unix_getprotobynumber()
{ invalid_argument("getprotobynumber not implemented"); }
  
value unix_getprotobyname()
{ invalid_argument("getprotobyname not implemented"); }

#endif
