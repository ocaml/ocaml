#include <mlvalues.h>
#include "unix.h"

value unix_getgid()              /* ML */
{
  return Val_int(getgid());
}
