#include <mlvalues.h>
#include "unix.h"

value unix_chroot(path)           /* ML */
     value path;
{
  int ret;
  ret = chroot(String_val(path));
  if (ret == -1) uerror("chroot", path);
  return Val_unit;
}
