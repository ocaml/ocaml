#include <mlvalues.h>
#include "unix.h"

value unix_fcntl_int(fd, request, arg)
     value fd, request, arg;
{
  int retcode;
  retcode = fcntl(Int_val(fd), Int_val(request), (char *) Int_val(arg));
  if (retcode == -1) uerror("fcntl_int", Nothing);
  return Val_int(retcode);
}

value unix_fcntl_ptr(fd, request, arg)
     value fd, request, arg;
{
  int retcode;
  retcode = fcntl(Int_val(fd), Int_val(request), String_val(arg));
  if (retcode == -1) uerror("fcntl_ptr", Nothing);
  return Val_int(retcode);
}
