/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Basic system calls */

#define _FILE_OFFSET_BITS 64

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
#if !macintosh && !_WIN32
#include <sys/wait.h>
#endif
#if macintosh
#include "macintosh.h"
#endif
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_TIMES
#include <sys/times.h>
#endif
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifndef _WIN32
extern int errno;
#endif

#ifdef HAS_STRERROR

#ifndef _WIN32
extern char * strerror(int);
#endif

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

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLexport void sys_error(value arg)
{
  CAMLparam1 (arg);
  char * err;
  CAMLlocal1 (str);
  
  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    raise_sys_blocked_io();
  } else {
    err = error_message();
    if (arg == NO_ARG) {
      str = copy_string(err);
    } else {
      int err_len = strlen(err);
      int arg_len = string_length(arg);
      str = alloc_string(arg_len + 2 + err_len);
      memmove(&Byte(str, 0), String_val(arg), arg_len);
      memmove(&Byte(str, arg_len), ": ", 2);
      memmove(&Byte(str, arg_len + 2), err, err_len);
    }
    raise_sys_error(str);
  }
}

CAMLprim value sys_exit(value retcode)
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
  O_RDONLY, O_WRONLY, O_APPEND | O_WRONLY, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

CAMLprim value sys_open(value path, value flags, value perm)
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

CAMLprim value sys_close(value fd)
{
  close(Int_val(fd));
  return Val_unit;
}

CAMLprim value sys_file_exists(value name)
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

CAMLprim value sys_remove(value name)
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}

CAMLprim value sys_rename(value oldname, value newname)
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}

CAMLprim value sys_chdir(value dirname)
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}

CAMLprim value sys_getcwd(value unit)
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return copy_string(buff);
}

#ifdef _WIN32
extern char * win32_getenv(char * command);
#endif

CAMLprim value sys_getenv(value var)
{
  char * res;

#ifndef _WIN32
  res = getenv(String_val(var));
#else
  res = win32_getenv(String_val(var));
#endif
  if (res == 0) raise_not_found();
  return copy_string(res);
}

char * caml_exe_name;
static char ** caml_main_argv;

CAMLprim value sys_get_argv(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal3 (exe_name, argv, res);
  exe_name = copy_string(caml_exe_name);
  argv = copy_string_array((char const **) caml_main_argv);
  res = alloc_small(2, 0);
  Field(res, 0) = exe_name;
  Field(res, 1) = argv;
  CAMLreturn(res);
}

void sys_init(char * exe_name, char **argv)
{
  caml_exe_name = exe_name;
  caml_main_argv = argv;
}

#if !(defined(WIFEXITED) && defined(WEXITSTATUS))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#endif

#ifdef _WIN32
extern int win32_system(char * command);
#endif

CAMLprim value sys_system_command(value command)
{
  int status, retcode;
  
  enter_blocking_section ();
#ifndef _WIN32
  status = system(String_val(command));
  if (WIFEXITED(status))
    retcode = WEXITSTATUS(status);
  else
    retcode = 255;
#else
  status = retcode = win32_system(String_val(command));
#endif
  leave_blocking_section ();
  if (status == -1) sys_error(command);
  return Val_int(retcode);
}

CAMLprim value sys_time(value unit)
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

CAMLprim value sys_random_seed (value unit)
{
#ifdef HAS_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return Val_int(tv.tv_sec ^ tv.tv_usec);
#else
  return Val_int(time (NULL));
#endif
}

CAMLprim value sys_get_config(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal2 (result, ostype);

  ostype = copy_string(OCAML_OS_TYPE);
  result = alloc_small (2, 0);
  Field(result, 0) = ostype;
  Field(result, 1) = Val_long (8 * sizeof(value));
  CAMLreturn (result);
}

