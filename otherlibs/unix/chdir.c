#include <mlvalues.h>
#include "unix.h"

value unix_chdir(path)           /* ML */
     value path;
{
  int ret;
  ret = chdir(String_val(path));
  if (ret == -1) uerror("chdir", path);
  return Val_unit;
}
