#include <mlvalues.h>
#include "unix.h"

value unix_getppid()              /* ML */
{
  return Val_int(getppid());
}
