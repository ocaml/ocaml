#include <mlvalues.h>
#include "unix.h"

value unix_setgid(gid)           /* ML */
     value gid;
{
  if (setgid(Int_val(gid)) == -1) uerror("setgid", Nothing);
  return Val_unit;
}
