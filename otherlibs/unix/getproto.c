#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include <netdb.h>

static value alloc_proto_entry(entry)
     struct protoent * entry;
{
  value res;
  Push_roots(r, 2);

  r[0] = copy_string(entry->p_name);
  r[1] = copy_string_array(entry->p_aliases);
  res = alloc_tuple(3);
  Field(res,0) = r[0];
  Field(res,1) = r[1];
  Field(res,2) = Val_int(entry->p_proto);
  Pop_roots();
  return res;
}

value unix_getprotobyname(name)  /* ML */
     value name;
{
  struct protoent * entry;
  entry = getprotobyname(String_val(name));
  if (entry == (struct protoent *) NULL) mlraise(Atom(NOT_FOUND_EXN));
  return alloc_proto_entry(entry);
}

value unix_getprotobynumber(proto) /* ML */
     value proto;
{
  struct protoent * entry;
  entry = getprotobynumber(Int_val(proto));
  if (entry == (struct protoent *) NULL) mlraise(Atom(NOT_FOUND_EXN));
  return alloc_proto_entry(entry);
}

#else

value unix_getprotobynumber()
{ invalid_argument("getprotobynumber not implemented"); }
  
value unix_getprotobyname()
{ invalid_argument("getprotobyname not implemented"); }

#endif
