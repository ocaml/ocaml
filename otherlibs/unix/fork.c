#include <mlvalues.h>
#include "unix.h"

value unix_fork(unit)               /* ML */
     value unit;
{
  int ret;
  ret = fork();
  if (ret == -1) uerror("fork", Nothing);
  return Val_int(ret);
}

