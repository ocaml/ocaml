#include <mlvalues.h>
#include "unix.h"

value unix_getpid()              /* ML */
{
  return Val_int(getpid());
}
