#include <mlvalues.h>
#include "unix.h"

value unix_umask(perm)           /* ML */
     value perm;
{
  return Val_int(umask(Int_val(perm)));
}
