#include <mlvalues.h>
#include "unix.h"

value unix_close(fd)             /* ML */
     value fd;
{
  if (close(Int_val(fd)) == -1) uerror("close", Nothing);
  return Val_unit;
}
