/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if !macintosh
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

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
     value arg;
{
  char * err = error_message();
  value str;
  
  if (arg == NO_ARG) {
    str = copy_string(err);
  } else {
    int err_len = strlen(err);
    int arg_len = string_length(arg);
    Begin_root(arg);
      str = alloc_string(arg_len + 2 + err_len);
    End_roots();
    bcopy(String_val(arg), &Byte(str, 0), arg_len);
    bcopy(": ", &Byte(str, arg_len), 2);
    bcopy(err, &Byte(str, arg_len + 2), err_len);
  }
  raise_sys_error(str);
}

value sys_exit(retcode)          /* ML */
     value retcode;
{
#ifndef NATIVE_CODE
  debugger(PROGRAM_EXIT);
#endif
#ifdef HAS_UI
  ui_exit(Int_val(retcode));
#else
  exit(Int_val(retcode));
#endif
  return Val_unit;
}

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_NONBLOCK
#ifdef O_NDELAY
#define O_NONBLOCK O_NDELAY
#else
#define O_NONBLOCK 0
#endif
#endif

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

value sys_open(path, flags, perm) /* ML */
     value path, flags, perm;
{
  int ret;
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags)
#if !macintosh
             , Int_val(perm)
#endif
                                       );
  if (ret == -1) sys_error(path);
  return Val_long(ret);
}

value sys_close(fd)             /* ML */
     value fd;
{
  close(Int_val(fd));
  return Val_unit;
}

value sys_file_exists(name)     /* ML */
     value name;
{
#if macintosh
  int f;
  f = open (String_val (name), O_RDONLY);
  if (f == -1) return (Val_bool (0));
  close (f);
  return (Val_bool (1));
#else
  struct stat st;
  return Val_bool(stat(String_val(name), &st) == 0);
#endif
}

value sys_remove(name)          /* ML */
     value name;
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}

value sys_rename(oldname, newname) /* ML */
     value oldname, newname;
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}

value sys_chdir(dirname)        /* ML */
     value dirname;
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}

value sys_getcwd(unit)          /* ML */
     value unit;
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return copy_string(buff);
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
  if (retcode == -1) sys_error(command);
  return Val_int(retcode);
}

value sys_get_config(unit)  /* ML */
     value unit;
{
  value result;
  value ostype;

  ostype = copy_string(OCAML_OS_TYPE);
  Begin_root(ostype);
    result = alloc_tuple(2);
    Field(result, 0) = ostype;
    Field(result, 1) = Val_long (8 * sizeof(value));
  End_roots ();
  return result;
}

/* Search path function */

#ifdef _WIN32

char * searchpath(name)
     char * name;
{
  char * fullname;
  char * path;
  char * p;
  char * q;
  struct stat st;

  if (stat(name, &st) == 0) return name;
  path = getenv("PATH");
  if (path == NULL) return 0;
  fullname = stat_alloc(strlen(name) + strlen(path) + 6);
  strcpy(fullname, name);
  strcat(fullname, ".exe");
  if (stat(fullname, &st) == 0) return fullname;
  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\' || *p == ':') return name;
  }
  while(1) {
    for (p = fullname; *path != 0 && *path != ';'; p++, path++) *p = *path;
    if (p != fullname && p[-1] != '\\') *p++ = '\\';
    for (q = name; *q != 0; p++, q++) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0) break;
    strcpy(p, ".exe");
    if (stat(fullname, &st) == 0) break;
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}

#elif macintosh

/* We don't need searchpath on the Macintosh because there are no #! scripts */

char *searchpath (name)
     char *name;
{
  return name;
}

#else

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * searchpath(name)
     char * name;
{
  char * fullname;
  char * path;
  char * p;
  char * q;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == NULL) return 0;
  fullname = stat_alloc(strlen(name) + strlen(path) + 2);
  while(1) {
    for (p = fullname; *path != 0 && *path != ':'; p++, path++) *p = *path;
    if (p != fullname) *p++ = '/';
    for (q = name; *q != 0; p++, q++) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}

#endif /* _WIN32, macintosh, ... */
