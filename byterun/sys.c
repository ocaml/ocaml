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
#include <time.h>
#if !macintosh
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_TIMES
#include <sys/times.h>
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

extern char * strerror(int);

char * error_message(void)
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

char * error_message(void)
{
  if (errno < 0 || errno >= sys_nerr)
    return "unknown error";
  else
    return sys_errlist[errno];
}

#endif /* HAS_STRERROR */

void sys_error(value arg)
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

value sys_exit(value retcode)          /* ML */
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

value sys_open(value path, value flags, value perm) /* ML */
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

value sys_close(value fd)             /* ML */
{
  close(Int_val(fd));
  return Val_unit;
}

value sys_file_exists(value name)     /* ML */
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

value sys_remove(value name)          /* ML */
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}

value sys_rename(value oldname, value newname) /* ML */
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}

value sys_chdir(value dirname)        /* ML */
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}

value sys_getcwd(value unit)          /* ML */
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return copy_string(buff);
}

value sys_getenv(value var)           /* ML */
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return copy_string(res);
}

static char ** main_argv;

value sys_get_argv(value unit)        /* ML */
{
  return copy_string_array(main_argv);
}

void sys_init(char **argv)
{
  main_argv = argv;
}

value sys_system_command(value command)   /* ML */
{
#ifndef _WIN32
  int retcode = system(String_val(command));
#else
  int retcode = win32_system(String_val(command));
#endif
  if (retcode == -1) sys_error(command);
  return Val_int(retcode);
}

value sys_time(value unit)            /* ML */
{
#ifdef HAS_TIMES
#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif
  struct tms t;
  times(&t);
  return copy_double((double)(t.tms_utime + t.tms_stime) / CLK_TCK);
#else
  /* clock() is standard ANSI C */
  return copy_double((double)clock() / CLOCKS_PER_SEC);
#endif
}

value sys_get_config(value unit)  /* ML */
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

char * searchpath(char * name)
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
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    strcpy(p, ".exe");
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}

#elif macintosh

/* We don't need searchpath on the Macintosh because there are no #! scripts */

char *searchpath (char * name)
{
  return name;
}

#else

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * searchpath(char * name)
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

/* Wrapper around "system" for Win32 */

#ifdef _WIN32

#include <ctype.h>
extern char * mktemp(char *);

int win32_system(char * cmdline)
{
#define MAX_CMD_LENGTH 256
  char cmd[MAX_CMD_LENGTH + 16];
  char template[9];
  char * tempfile;
  int len, i, j, fd, retcode;

  len = strlen(cmdline);
  if (len < 1000) {
    return system(cmdline);
  } else {
    /* Skip initial blanks, if any */
    for (i = 0; cmdline[i] != 0 && isspace(cmdline[i]); i++) /*nothing*/;
    /* Copy command name to buffer, stop at first blank */
    for (j = 0; cmdline[i] != 0 && ! isspace(cmdline[i]); i++) {
      if (j < MAX_CMD_LENGTH) cmd[j++] = cmdline[i];
    }
    /* Save remainder of command line to temp file */
    strcpy(template, "cmXXXXXX");
    tempfile = mktemp(template);
    fd = open(tempfile, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, 0666);
    if (fd == -1) return -1;
    write(fd, cmdline + i, len - i);
    close(fd);
    /* Add " @tempfile" to the command line */
    sprintf(cmd + j, " @%s", tempfile);
    /* Run command */
    retcode = system(cmd);
    /* Remove temp file and exit */
    unlink(tempfile);
    return retcode;
  }
}

#endif
