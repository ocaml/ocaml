#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

value unix_listen(sock, backlog)
     value sock, backlog;
{
  if (listen(Int_val(sock), Int_val(backlog)) == -1) uerror("listen", Nothing);
  return Val_unit;
}

#else

value unix_listen() { invalid_argument("listen not implemented"); }

#endif
