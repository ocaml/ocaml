#include <mlvalues.h>
#include <alloc.h>
#include <time.h>

#include "unixsupport.h"

static time_t initial_time = 0;	/* 0 means uninitialized */
static DWORD initial_tickcount;

value unix_gettimeofday(value unit)                /* ML */
{
  if (initial_time == 0) {
    initial_tickcount = GetTickCount();
    initial_time = time(NULL);
    return copy_double((double) initial_time);
  } else {
    return copy_double(initial_time +
		       (GetTickCount() - initial_tickcount) * 1e-3);
  }
}
