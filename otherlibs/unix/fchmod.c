#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_FCHMOD

value unix_fchmod(fd, perm)      /* ML */
     value fd, perm;
{
  if (fchmod(Int_val(fd), Int_val(perm)) == -1) uerror("fchmod", Nothing);
  return Val_unit;
}

#else

value unix_fchmod() { invalid_argument("fchmod not implemented"); }
  
#endif
