#include <mlvalues.h>
#include "unix.h"

value unix_mkdir(path, perm)     /* ML */
     value path, perm;
{
  if (mkdir(String_val(path), Int_val(perm)) == -1) uerror("mkdir", path);
  return Val_unit;
}
