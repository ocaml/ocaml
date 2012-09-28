/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: sys.c 12959 2012-09-27 13:12:51Z maranget $ */

/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#if !_WIN32
#include <sys/wait.h>
#endif
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_TIMES
#include <sys/times.h>
#endif
#ifdef HAS_GETRUSAGE
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"

static char * error_message(void)
{
  return strerror(errno);
}

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLexport void caml_sys_error(value arg)
{
  CAMLparam1 (arg);
  char * err;
  CAMLlocal1 (str);

  err = error_message();
  if (arg == NO_ARG) {
    str = caml_copy_string(err);
  } else {
    int err_len = strlen(err);
    int arg_len = caml_string_length(arg);
    str = caml_alloc_string(arg_len + 2 + err_len);
    memmove(&Byte(str, 0), String_val(arg), arg_len);
    memmove(&Byte(str, arg_len), ": ", 2);
    memmove(&Byte(str, arg_len + 2), err, err_len);
  }
  caml_raise_sys_error(str);
  CAMLnoreturn;
}

CAMLexport void caml_sys_io_error(value arg)
{
  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    caml_raise_sys_blocked_io();
  } else {
    caml_sys_error(arg);
  }
}

CAMLprim value caml_sys_exit(value retcode)
{
#ifndef NATIVE_CODE
  caml_debugger(PROGRAM_EXIT);
#endif
  exit(Int_val(retcode));
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

CAMLprim value caml_sys_open(value path, value vflags, value vperm)
{
  CAMLparam3(path, vflags, vperm);
  int fd, flags, perm;
  char * p;

  p = caml_stat_alloc(caml_string_length(path) + 1);
  strcpy(p, String_val(path));
  flags = caml_convert_flag_list(vflags, sys_open_flags);
  perm = Int_val(vperm);
  /* open on a named FIFO can block (PR#1533) */
  caml_enter_blocking_section();
  fd = open(p, flags, perm);
  /* fcntl on a fd can block (PR#5069)*/
#if defined(F_SETFD) && defined(FD_CLOEXEC)
  if (fd != -1)
    fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (fd == -1) caml_sys_error(path);
  CAMLreturn(Val_long(fd));
}

CAMLprim value caml_sys_close(value fd)
{
  close(Int_val(fd));
  return Val_unit;
}

CAMLprim value caml_sys_file_exists(value name)
{
  struct stat st;
  return Val_bool(stat(String_val(name), &st) == 0);
}

CAMLprim value caml_sys_is_directory(value name)
{
  struct stat st;
  if (stat(String_val(name), &st) == -1) caml_sys_error(name);
#ifdef S_ISDIR
  return Val_bool(S_ISDIR(st.st_mode));
#else
  return Val_bool(st.st_mode & S_IFDIR);
#endif
}

CAMLprim value caml_sys_remove(value name)
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) caml_sys_error(name);
  return Val_unit;
}

CAMLprim value caml_sys_rename(value oldname, value newname)
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value caml_sys_chdir(value dirname)
{
  if (chdir(String_val(dirname)) != 0) caml_sys_error(dirname);
  return Val_unit;
}

CAMLprim value caml_sys_getcwd(value unit)
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) caml_sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) caml_sys_error(NO_ARG);
#endif /* HAS_GETCWD */
  return caml_copy_string(buff);
}

CAMLprim value caml_sys_getenv(value var)
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) caml_raise_not_found();
  return caml_copy_string(res);
}

char * caml_exe_name;
static char ** caml_main_argv;

CAMLprim value caml_sys_get_argv(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal3 (exe_name, argv, res);
  exe_name = caml_copy_string(caml_exe_name);
  argv = caml_copy_string_array((char const **) caml_main_argv);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = exe_name;
  Field(res, 1) = argv;
  CAMLreturn(res);
}

void caml_sys_init(char * exe_name, char **argv)
{
  caml_exe_name = exe_name;
  caml_main_argv = argv;
}

#ifdef _WIN32
#define WIFEXITED(status) 1
#define WEXITSTATUS(status) (status)
#else
#if !(defined(WIFEXITED) && defined(WEXITSTATUS))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#endif
#endif

CAMLprim value caml_sys_system_command(value command)
{
  CAMLparam1 (command);
  int status, retcode;
  char *buf;
  intnat len;

  len = caml_string_length (command);
  buf = caml_stat_alloc (len + 1);
  memmove (buf, String_val (command), len + 1);
  caml_enter_blocking_section ();
  status = system(buf);
  caml_leave_blocking_section ();
  caml_stat_free(buf);
  if (status == -1) caml_sys_error(command);
  if (WIFEXITED(status))
    retcode = WEXITSTATUS(status);
  else
    retcode = 255;
  CAMLreturn (Val_int(retcode));
}

CAMLprim value caml_sys_time(value unit)
{
#ifdef HAS_GETRUSAGE
  struct rusage ru;

  getrusage (RUSAGE_SELF, &ru);
  return caml_copy_double (ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6
                           + ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);
#else
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
    return caml_copy_double((double)(t.tms_utime + t.tms_stime) / CLK_TCK);
  #else
    /* clock() is standard ANSI C */
    return caml_copy_double((double)clock() / CLOCKS_PER_SEC);
  #endif
#endif
}

#ifdef _WIN32
extern int caml_win32_random_seed (intnat data[16]);
#endif

CAMLprim value caml_sys_random_seed (value unit)
{
  intnat data[16];
  int n, i;
  value res;
#ifdef _WIN32
  n = caml_win32_random_seed(data);
#else
  int fd;
  n = 0;
  /* Try /dev/urandom first */
  fd = open("/dev/urandom", O_RDONLY, 0);
  if (fd != -1) {
    unsigned char buffer[12];
    int nread = read(fd, buffer, 12);
    close(fd);
    while (nread > 0) data[n++] = buffer[--nread];
  }
  /* If the read from /dev/urandom fully succeeded, we now have 96 bits
     of good random data and can stop here.  Otherwise, complement
     whatever we got (probably nothing) with some not-very-random data. */
  if (n < 12) {
#ifdef HAS_GETTIMEOFDAY
    struct timeval tv;
    gettimeofday(&tv, NULL);
    data[n++] = tv.tv_usec;
    data[n++] = tv.tv_sec;
#else
    data[n++] = time(NULL);
#endif
#ifdef HAS_UNISTD
    data[n++] = getpid();
    data[n++] = getppid();
#endif
  }
#endif
  /* Convert to an OCaml array of ints */
  res = caml_alloc_small(n, 0);
  for (i = 0; i < n; i++) Field(res, i) = Val_long(data[i]);
  return res;
}

CAMLprim value caml_sys_get_config(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal2 (result, ostype);

  ostype = caml_copy_string(OCAML_OS_TYPE);
  result = caml_alloc_small (3, 0);
  Field(result, 0) = ostype;
  Field(result, 1) = Val_long (8 * sizeof(value));
#ifdef ARCH_BIG_ENDIAN
  Field(result, 2) = Val_true;
#else
  Field(result, 2) = Val_false;
#endif
  CAMLreturn (result);
}

CAMLprim value caml_sys_read_directory(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct ext_table tbl;

  caml_ext_table_init(&tbl, 50);
  if (caml_read_directory(String_val(path), &tbl) == -1){
    caml_ext_table_free(&tbl, 1);
    caml_sys_error(path);
  }
  caml_ext_table_add(&tbl, NULL);
  result = caml_copy_string_array((char const **) tbl.contents);
  caml_ext_table_free(&tbl, 1);
  CAMLreturn(result);
}
