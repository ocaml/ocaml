#include <errno.h>
#include <mlvalues.h>
#include <alloc.h>

extern int error_table[];

#ifdef HAS_STRERROR

#include <string.h>

value unix_error_message(err)
     value err;
{
  int errnum;
  errnum = error_table[Tag_val(err)];
  return copy_string(strerror(errno));
}

#else

extern int sys_nerr;
extern char *sys_errlist[];

value unix_error_message(err)
     value err;
{
  int errnum;
  errnum = error_table[Tag_val(err)];
  if (errnum < 0 || errnum >= sys_nerr) {
    return copy_string("Unknown error");
  } else {
    return copy_string(sys_errlist[errnum]);
  }
}

#endif
