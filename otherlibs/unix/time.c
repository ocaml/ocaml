#include <mlvalues.h>
#include "unix.h"

extern long time();

value unix_time()                /* ML */
{
  return Val_long(time((long *) NULL));
}
