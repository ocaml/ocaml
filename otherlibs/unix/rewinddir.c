#include <mlvalues.h>
#include "unix.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

value unix_rewinddir(d)          /* ML */
     value d;
{
  rewinddir((DIR *) d);
  return Atom(0);
}
