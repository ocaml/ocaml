#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

value unix_pipe()                /* ML */
{
  int fd[2];
  value res;
  if (pipe(fd) == -1) uerror("pipe", Nothing);
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}
