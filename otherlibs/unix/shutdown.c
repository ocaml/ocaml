#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

static int shutdown_command_table[] = {
  0, 1, 2
};

value unix_shutdown(sock, cmd)   /* ML */
     value sock, cmd;
{
  if (shutdown(Int_val(sock), shutdown_command_table[Tag_val(cmd)]) == -1)
    uerror("shutdown", Nothing);
  return Val_unit;
}

#else

value unix_shutdown() { invalid_argument("shutdown not implemented"); }

#endif
