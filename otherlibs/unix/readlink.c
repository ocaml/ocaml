#include <mlvalues.h>
#include <alloc.h>

#ifdef HAS_SYMLINK

#include <sys/param.h>
#include "unix.h"

value unix_readlink(path)        /* ML */
     value path;
{
  char buffer[MAXPATHLEN];
  int len;
  len = readlink(String_val(path), buffer, sizeof(buffer) - 1);
  if (len == -1) uerror("readlink", path);
  buffer[len] = '\0';
  return copy_string(buffer);
}

#else

value unix_readlink() { invalid_argument("readlink not implemented"); }

#endif
