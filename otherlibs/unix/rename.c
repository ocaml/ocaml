#include <mlvalues.h>
#include "unix.h"

value unix_rename(path1, path2)  /* ML */
     value path1, path2;
{
  if (rename(String_val(path1), String_val(path2)) == -1)
    uerror("rename", path1);
  return Atom(0);
}
