#include <mlvalues.h>
#include "unix.h"

value unix_dup(fd)               /* ML */
     value fd;
{
  int ret;
  ret = dup(Int_val(fd));
  if (ret == -1) uerror("dup", Nothing);
  return Val_int(ret);
}
