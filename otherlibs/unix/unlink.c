#include <mlvalues.h>
#include "unix.h"

value unix_unlink(path)          /* ML */
     value path;
{
  if (unlink(String_val(path)) == -1) uerror("unlink", path);
  return Val_unit;
}
