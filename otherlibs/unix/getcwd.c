#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

#ifdef HAS_GETCWD

#include <sys/param.h>

value unix_getcwd()     /* ML */
{
  char buff[MAXPATHLEN];
  if (getcwd(buff, sizeof(buff)) == 0) uerror("getcwd", NULL);
  return copy_string(buff);
}

#else
#ifdef HAS_GETWD

#include <sys/param.h>

value unix_getcwd()
{
  char buff[MAXPATHLEN];
  if (getwd(buff) == 0) uerror("getcwd", buff);
  return copy_string(buff);
}

#else

value unix_getcwd() { invalid_argument("getcwd not implemented"); }

#endif
#endif
