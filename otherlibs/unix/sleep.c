#include <mlvalues.h>
#include "unix.h"

value unix_sleep(t)              /* ML */
     value t;
{
  enter_blocking_section();
  sleep(Int_val(t));
  leave_blocking_section();
  return Val_unit;
}
