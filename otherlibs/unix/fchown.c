#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_FCHMOD

value unix_fchown(fd, uid, gid)  /* ML */
     value fd, uid, gid;
{
  if (fchown(Int_val(fd), Int_val(uid), Int_val(gid)) == -1)
    uerror("fchown", Nothing);
  return Val_unit;
}

#else

value unix_fchown() { invalid_argument("fchown not implemented"); }
  
#endif
