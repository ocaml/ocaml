#include <mlvalues.h>
#include "unix.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

value unix_closedir(d)           /* ML */
     value d;
{
  closedir((DIR *) d);
  return Val_unit;
}
