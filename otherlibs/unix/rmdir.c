#include <mlvalues.h>
#include "unix.h"

value unix_rmdir(path)           /* ML */
     value path;
{
  if (rmdir(String_val(path)) == -1) uerror("rmdir", path);
  return Atom(0);
}
