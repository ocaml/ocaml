#include <mlvalues.h>
#include "unix.h"

value unix_pause()               /* ML */
{
  pause();
  return Val_unit;
}
