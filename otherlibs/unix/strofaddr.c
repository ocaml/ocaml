#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

extern char * inet_ntoa();

value unix_string_of_inet_addr(a) /* ML */
     value a;
{
  struct in_addr address;
  address.s_addr = GET_INET_ADDR(a);
  return copy_string(inet_ntoa(address));
}

#else

value unix_string_of_inet_addr()
{ invalid_argument("string_of_inet_addr not implemented"); }

#endif
