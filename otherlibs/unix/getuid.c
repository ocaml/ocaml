#include <mlvalues.h>
#include "unix.h"

value unix_getuid()              /* ML */
{
  return Val_int(getuid());
}
