#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"
#include <errno.h>

extern char * getlogin();

value unix_getlogin()            /* ML */
{
  char * name;
  name = getlogin();
  if (name == NULL) unix_error(ENOENT, "getlogin", Nothing);
  return copy_string(name);
}
