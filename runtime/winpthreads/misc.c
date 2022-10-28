/*
   Copyright (c) 2011-2016  mingw-w64 project

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
*/

#include "pthread.h"
#include "windows.h"
#include "misc.h"

unsigned long long _pthread_time_in_ms(void)
{
    FILETIME ft;

    GetSystemTimeAsFileTime(&ft);
    return (((unsigned long long)ft.dwHighDateTime << 32) + ft.dwLowDateTime
            - 0x19DB1DED53E8000ULL) / 10000ULL;
}

unsigned long long _pthread_time_in_ms_from_timespec(const struct timespec *ts)
{
    unsigned long long t = (unsigned long long) ts->tv_sec * 1000LL;
    /* The +999999 is here to ensure that the division always rounds up */
    t += (unsigned long long) (ts->tv_nsec + 999999) / 1000000;

    return t;
}

unsigned long long _pthread_rel_time_in_ms(const struct timespec *ts)
{
    unsigned long long t1 = _pthread_time_in_ms_from_timespec(ts);
    unsigned long long t2 = _pthread_time_in_ms();

    /* Prevent underflow */
    if (t1 < t2) return 0;
    return t1 - t2;
}

static unsigned long long
_pthread_get_tick_count (long long *frequency)
{
#if defined (_WIN32_WINNT) && (_WIN32_WINNT >= _WIN32_WINNT_VISTA)
  (void) frequency; /* unused */
  return GetTickCount64 ();
#else
  LARGE_INTEGER freq, timestamp;

  if (*frequency == 0)
  {
    if (QueryPerformanceFrequency (&freq))
      *frequency = freq.QuadPart;
    else
      *frequency = -1;
  }

  if (*frequency > 0 && QueryPerformanceCounter (&timestamp))
    return timestamp.QuadPart / (*frequency / 1000);

  /* Fallback */
  return GetTickCount ();
#endif
}

/* A wrapper around WaitForSingleObject() that ensures that
 * the wait function does not time out before the time
 * actually runs out. This is needed because WaitForSingleObject()
 * might have poor accuracy, returning earlier than expected.
 * On the other hand, returning a bit *later* than expected
 * is acceptable in a preemptive multitasking environment.
 */
unsigned long
_pthread_wait_for_single_object (void *handle, unsigned long timeout)
{
  DWORD result;
  unsigned long long start_time, end_time;
  unsigned long wait_time;
  long long frequency = 0;

  if (timeout == INFINITE || timeout == 0)
    return WaitForSingleObject ((HANDLE) handle, (DWORD) timeout);

  start_time = _pthread_get_tick_count (&frequency);
  end_time = start_time + timeout;
  wait_time = timeout;

  do
  {
    unsigned long long current_time;

    result = WaitForSingleObject ((HANDLE) handle, (DWORD) wait_time);
    if (result != WAIT_TIMEOUT)
      break;

    current_time = _pthread_get_tick_count (&frequency);
    if (current_time >= end_time)
      break;

    wait_time = (DWORD) (end_time - current_time);
  } while (TRUE);

  return result;
}

/* A wrapper around WaitForMultipleObjects() that ensures that
 * the wait function does not time out before the time
 * actually runs out. This is needed because WaitForMultipleObjects()
 * might have poor accuracy, returning earlier than expected.
 * On the other hand, returning a bit *later* than expected
 * is acceptable in a preemptive multitasking environment.
 */
unsigned long
_pthread_wait_for_multiple_objects (unsigned long count, void **handles, unsigned int all, unsigned long timeout)
{
  DWORD result;
  unsigned long long start_time, end_time;
  unsigned long wait_time;
  long long frequency = 0;

  if (timeout == INFINITE || timeout == 0)
    return WaitForMultipleObjects ((DWORD) count, (HANDLE *) handles, all, (DWORD) timeout);

  start_time = _pthread_get_tick_count (&frequency);
  end_time = start_time + timeout;
  wait_time = timeout;

  do
  {
    unsigned long long current_time;

    result = WaitForMultipleObjects ((DWORD) count, (HANDLE *) handles, all, (DWORD) wait_time);
    if (result != WAIT_TIMEOUT)
      break;

    current_time = _pthread_get_tick_count (&frequency);
    if (current_time >= end_time)
      break;

    wait_time = (DWORD) (end_time - current_time);
  } while (TRUE);

  return result;
}
