#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"
#include <sys/types.h>
#include <sys/times.h>

value unix_times()               /* ML */
{
  value res;
  struct tms buffer;
  int i;
  Push_roots(t,4);

#ifndef HZ
#define HZ 60
#endif

  times(&buffer);
  t[0] = copy_double((double) buffer.tms_utime / HZ);
  t[1] = copy_double((double) buffer.tms_stime / HZ);
  t[2] = copy_double((double) buffer.tms_cutime / HZ);
  t[3] = copy_double((double) buffer.tms_cstime / HZ);
  res = alloc_tuple(4);
  for (i = 0; i < 4; i++)
    Field(res, i) = t[i];
  Pop_roots();
  return res;
}
