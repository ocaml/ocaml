#include <mlvalues.h>
#include "unix.h"

value unix_alarm(t)              /* ML */
     value t;
{
  return Val_int(alarm((unsigned int) Long_val(t)));
}
