#include <mlvalues.h>
#include "unix.h"

value unix_setuid(uid)           /* ML */
     value uid;
{
  if (setuid(Int_val(uid)) == -1) uerror("setuid", Nothing);
  return Val_unit;
}
