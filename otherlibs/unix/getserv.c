#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

static value alloc_service_entry(entry)
     struct servent * entry;
{
  value res;
  Push_roots(r, 3);

  r[0] = copy_string(entry->s_name);
  r[1] = copy_string_array(entry->s_aliases);
  r[2] = copy_string(entry->s_proto);
  res = alloc_tuple(4);
  Field(res,0) = r[0];
  Field(res,1) = r[1];
  Field(res,2) = Val_int(ntohs(entry->s_port));
  Field(res,3) = r[2];
  Pop_roots();
  return res;
}

value unix_getservbyname(name, proto)  /* ML */
     value name, proto;
{
  struct servent * entry;
  entry = getservbyname(String_val(name), String_val(proto));
  if (entry == (struct servent *) NULL) mlraise(Atom(NOT_FOUND_EXN));
  return alloc_service_entry(entry);
}

value unix_getservbyport(port, proto)  /* ML */
     value port, proto;
{
  struct servent * entry;
  entry = getservbyport(Int_val(port), String_val(proto));
  if (entry == (struct servent *) NULL) mlraise(Atom(NOT_FOUND_EXN));
  return alloc_service_entry(entry);
}

#else

value unix_getservbyport()
{ invalid_argument("getservbyport not implemented"); }
  
value unix_getservbyname()
{ invalid_argument("getservbyname not implemented"); }

#endif
