#include <mlvalues.h>
#include "unix.h"

value unix_chown(path, uid, gid) /* ML */
     value path, uid, gid;
{
  int ret;
  ret = chown(String_val(path), Int_val(uid), Int_val(gid));
  if (ret == -1) uerror("chown", path);
  return Val_unit;
}
