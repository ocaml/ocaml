#include <mlvalues.h>
#include "unix.h"

value unix_geteuid()             /* ML */
{
  return Val_int(geteuid());
}
