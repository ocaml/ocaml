#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_TRUNCATE

value unix_ftruncate(fd, len)    /* ML */
     value fd, len;
{
  if (ftruncate(Int_val(fd), Long_val(len)) == -1)
    uerror("ftruncate", Nothing);
  return Val_unit;
}

#else

value unix_ftruncate() { invalid_argument("ftruncate not implemented"); }

#endif
