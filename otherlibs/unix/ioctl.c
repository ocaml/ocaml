#include <mlvalues.h>
#include "unix.h"

value unix_ioctl_int(fd, request, arg)
     value fd, request, arg;
{
  int retcode;
  retcode = ioctl(Int_val(fd), Int_val(request), (char *) Long_val(arg));
  if (retcode == -1) uerror("ioctl_int", Nothing);
  return Val_int(retcode);
}

value unix_ioctl_ptr(fd, request, arg)
     value fd, request, arg;
{
  int retcode;
  retcode = ioctl(Int_val(fd), Int_val(request), String_val(arg));
  if (retcode == -1) uerror("ioctl_ptr", Nothing);
  return Val_int(retcode);
}
