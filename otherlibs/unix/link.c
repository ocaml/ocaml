#include <mlvalues.h>
#include "unix.h"

value unix_link(path1, path2)    /* ML */
     value path1, path2;
{
  if (link(String_val(path1), String_val(path2)) == -1) uerror("link", path2);
  return Val_unit;
}
