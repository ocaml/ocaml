#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#include <sys/file.h>
#ifndef R_OK
#define R_OK    4/* test for read permission */
#define W_OK    2/* test for write permission */
#define X_OK    1/* test for execute (search) permission */
#define F_OK    0/* test for presence of file */
#endif
#endif

static int access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

value unix_access(path, perms)   /* ML */
     value path, perms;
{
  int ret;
  ret = access(String_val(path),
               convert_flag_list(perms, access_permission_table));
  if (ret == -1)
    uerror("access", path);
  return Val_unit;
}
