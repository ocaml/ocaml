#include <mlvalues.h>
#include "unix.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

value unix_opendir(path)         /* ML */
     value path;
{
  DIR * d;
  d = opendir(String_val(path));
  if (d == (DIR *) NULL) uerror("opendir", path);
  return (value) d;
}
