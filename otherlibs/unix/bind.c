#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
  
value unix_bind(socket, address)      /* ML */
     value socket, address;
{
  int ret;
  get_sockaddr(address);
  ret = bind(Int_val(socket), &sock_addr.s_gen, sock_addr_len);
  if (ret == -1) uerror("bind", Nothing);
  return Val_unit;
}

#else

value unix_bind() { invalid_argument("bind not implemented"); }
  
#endif
