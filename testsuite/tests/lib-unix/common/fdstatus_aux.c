/* Check if file descriptors are open or not */

#ifdef _WIN32
#define UNICODE
#define _CRT_NONSTDC_NO_WARNINGS
#include <io.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

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
    close(fd);
  } else if (errno == EBADF) {
    printf("closed\n");
  } else {
    printf("error %s\n", strerror(errno));
  }
}

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>

CAMLprim value caml_process_fd(value CAMLnum, value CAMLfd)
{
  CAMLparam2(CAMLnum, CAMLfd);
  printf("#%d: ", Int_val(CAMLnum));
  process_fd(String_val(CAMLfd));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_fd_of_filedescr(value v)
{
  CAMLparam1(v);

#ifdef _WIN32
  int fd = caml_win32_CRT_fd_of_filedescr(v);
#else
  int fd = Int_val(v);
#endif

  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_win32_delete_on_close(value path)
{
  CAMLparam1(path);

#ifdef _WIN32
  char_os *wpath = caml_stat_strdup_to_utf16(String_val(path));
  /* Open the file with FILE_FLAG_DELETE_ON_CLOSE - all previous calls to
     Unix.openfile need to have specified Unix.O_SHARE_DELETE or this will fail.
     The handle the intention "leaks" - it will be automatically closed when the
     process exits, at which point Windows will delete it. */
  HANDLE h =
    CreateFile(wpath, GENERIC_READ,
               FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
               OPEN_EXISTING, FILE_FLAG_DELETE_ON_CLOSE, NULL);
  caml_stat_free(wpath);
  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    caml_uerror("delete_on_close", path);
  }
#endif

  CAMLreturn(Val_unit);
}
