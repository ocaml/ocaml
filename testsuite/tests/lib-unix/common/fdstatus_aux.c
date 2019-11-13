/* Check if file descriptors are open or not */

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <winerror.h>

void process_fd(const char * s)
{
  int fd;
  HANDLE h;
  DWORD flags;

#ifdef _WIN64
  h = (HANDLE) _atoi64(s);
#else
  h = (HANDLE) atoi(s);
#endif
  if (GetHandleInformation(h, &flags)) {
    printf("open\n");
  } else if (GetLastError() == ERROR_INVALID_HANDLE) {
    printf("closed\n");
  } else {
    printf("error %lu\n", (unsigned long)(GetLastError()));
  }
}

#else

#include <limits.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

void process_fd(const char * s)
{
  long n;
  int fd;
  char * endp;
  struct stat st;
  n = strtol(s, &endp, 0);
  if (*endp != 0 || n < 0 || n > (long) INT_MAX) {
    printf("parsing error\n");
    return;
  }
  fd = (int) n;
  if (fstat(fd, &st) != -1) {
    printf("open\n");
  } else if (errno == EBADF) {
    printf("closed\n");
  } else {
    printf("error %s\n", strerror(errno));
  }
}

#endif

#include "caml/mlvalues.h"
#include "caml/memory.h"

CAMLprim value caml_process_fd(value CAMLnum, value CAMLfd)
{
  CAMLparam2(CAMLnum, CAMLfd);
  printf("#%d: ", Int_val(CAMLnum));
  process_fd(String_val(CAMLfd));
  CAMLreturn(Val_unit);
}
