/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"

extern int errno;

#ifdef HAS_STRERROR

extern char * strerror();

char * error_message()
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

char * error_message()
{
  if (errno < 0 || errno >= sys_nerr)
    return "unknown error";
  else
    return sys_errlist[errno];
}

#endif /* HAS_STRERROR */

void sys_error(arg)
     char * arg;
{
  char * err = error_message();
  int err_len = strlen(err);
  int arg_len;
  value str;

  if (arg == NULL) {
    str = alloc_string(err_len);
    bcopy(err, &Byte(str, 0), err_len);
  } else {
    arg_len = strlen(arg);
    str = alloc_string(arg_len + 2 + err_len);
    bcopy(arg, &Byte(str, 0), arg_len);
    bcopy(": ", &Byte(str, arg_len), 2);
    bcopy(err, &Byte(str, arg_len + 2), err_len);
  }
  raise_sys_error(str);
}

void sys_exit(retcode)          /* ML */
     value retcode;
{
  exit(Int_val(retcode));
}

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT
};

value sys_open(path, flags, perm) /* ML */
     value path, flags, perm;
{
  int ret;
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags),
             Int_val(perm));
  if (ret == -1) sys_error(String_val(path));
  return Val_long(ret);
}

value sys_file_exists(name)     /* ML */
     value name;
{
  return Val_bool(access(String_val(name), F_OK) == 0);
}

value sys_remove(name)          /* ML */
     value name;
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(String_val(name));
  return Val_unit;
}

value sys_rename(oldname, newname) /* ML */
     value oldname, newname;
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(String_val(oldname));
  return Val_unit;
}

value sys_chdir(dirname)        /* ML */
     value dirname;
{
  if (chdir(String_val(dirname)) != 0) sys_error(String_val(dirname));
  return Val_unit;
}

value sys_getenv(var)           /* ML */
     value var;
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return copy_string(res);
}

static char ** main_argv;

value sys_get_argv(unit)        /* ML */
     value unit;
{
  return copy_string_array(main_argv);
}

void sys_init(argv)
     char ** argv;
{
  main_argv = argv;
}

value sys_system_command(command)   /* ML */
     value command;
{
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(String_val(command));
  return Val_int(retcode);
}

/* Search path function */

char * searchpath(name)
     char * name;
{
  static char fullname[512];
  char * path;
  char * p;
  char * q;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ':') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '/';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, F_OK) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}
