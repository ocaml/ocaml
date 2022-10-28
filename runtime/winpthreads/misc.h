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

#ifndef WIN_PTHREADS_MISC_H
#define WIN_PTHREADS_MISC_H

#include "pthread_compat.h"

#ifndef assert

#ifndef ASSERT_TRACE
# define ASSERT_TRACE 0
#else
# undef ASSERT_TRACE
# define ASSERT_TRACE 0
#endif

# define assert(e) \
   ((e) ? ((ASSERT_TRACE) ? fprintf(stderr, \
                                    "Assertion succeeded: (%s), file %s, line %d\n", \
                        #e, __FILE__, (int) __LINE__), \
                                fflush(stderr) : \
                             0) : \
          (fprintf(stderr, "Assertion failed: (%s), file %s, line %d\n", \
                   #e, __FILE__, (int) __LINE__), exit(1), 0))

# define fixme(e) \
   ((e) ? ((ASSERT_TRACE) ? fprintf(stderr, \
                                    "Assertion succeeded: (%s), file %s, line %d\n", \
                        #e, __FILE__, (int) __LINE__), \
                                fflush(stderr) : \
                             0) : \
          (fprintf(stderr, "FIXME: (%s), file %s, line %d\n", \
                   #e, __FILE__, (int) __LINE__), 0, 0))

#endif

#define PTR2INT(x)	((int)(uintptr_t)(x))

#if SIZE_MAX>UINT_MAX
typedef long long LONGBAG;
#else
typedef long LONGBAG;
#endif

#if !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
#undef GetHandleInformation
#define GetHandleInformation(h,f)  (1)
#endif

#define CHECK_HANDLE(h) { DWORD dwFlags; \
    if (!(h) || ((h) == INVALID_HANDLE_VALUE) || !GetHandleInformation((h), &dwFlags)) \
    return EINVAL; }

#define CHECK_PTR(p)    if (!(p)) return EINVAL;

#define UPD_RESULT(x,r)    { int _r=(x); r = r ? r : _r; }

#define CHECK_THREAD(t)  { \
    CHECK_PTR(t); \
    CHECK_HANDLE(t->h); }

#define CHECK_OBJECT(o, e)  { DWORD dwFlags; \
    if (!(o)) return e; \
    if (!((o)->h) || (((o)->h) == INVALID_HANDLE_VALUE) || !GetHandleInformation(((o)->h), &dwFlags)) \
        return e; }

#define VALID(x)    if (!(p)) return EINVAL;

/* ms can be 64 bit, solve wrap-around issues: */
static WINPTHREADS_INLINE unsigned long dwMilliSecs(unsigned long long ms)
{
  if (ms >= 0xffffffffULL) return 0xfffffffful;
  return (unsigned long) ms;
}

#ifndef _mm_pause
#define _mm_pause()			{__asm__ __volatile__("pause");}
#endif

#ifndef _ReadWriteBarrier
#define _ReadWriteBarrier   __sync_synchronize
#endif

#ifndef YieldProcessor
#define YieldProcessor      _mm_pause
#endif

unsigned long long _pthread_time_in_ms(void);
unsigned long long _pthread_time_in_ms_from_timespec(const struct timespec *ts);
unsigned long long _pthread_rel_time_in_ms(const struct timespec *ts);
unsigned long _pthread_wait_for_single_object (void *handle, unsigned long timeout);
unsigned long _pthread_wait_for_multiple_objects (unsigned long count, void **handles, unsigned int all, unsigned long timeout);

#endif
