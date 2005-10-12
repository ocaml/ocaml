/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Win32-specific stuff */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "signals.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * caml_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = caml_stat_alloc(strlen(path) + 1);
  strcpy(p, path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ';'; n++) /*nothing*/;
    caml_ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * caml_search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = caml_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "\\");
    strcat(fullname, name);
    caml_gc_message(0x100, "Searching %s\n", (uintnat) fullname);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    caml_stat_free(fullname);
  }
 not_found:
  caml_gc_message(0x100, "%s not found in search path\n", (uintnat) name);
  fullname = caml_stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}
  
CAMLexport char * caml_search_exe_in_path(char * name)
{
  char * fullname, * filepart;
  DWORD pathlen, retcode;

  pathlen = strlen(name) + 1;
  if (pathlen < 256) pathlen = 256;
  while (1) {
    fullname = stat_alloc(pathlen);
    retcode = SearchPath(NULL,              /* use system search path */
			 name,
			 ".exe",            /* add .exe extension if needed */
			 pathlen,
			 fullname,
			 &filepart);
    if (retcode == 0) {
      caml_gc_message(0x100, "%s not found in search path\n",
		      (uintnat) name);
      strcpy(fullname, name);
      break;
    }
    if (retcode < pathlen) break;
    stat_free(fullname);
    pathlen = retcode + 1;
  }
  return fullname;
}

char * caml_search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = caml_stat_alloc(strlen(name) + 5);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".dll");
  res = caml_search_in_path(path, dllname);
  caml_stat_free(dllname);
  return res;
}

void * caml_dlopen(char * libname)
{
  return (void *) LoadLibrary(libname);
}

void caml_dlclose(void * handle)
{
  FreeLibrary((HMODULE) handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return (void *) GetProcAddress((HMODULE) handle, name);
}

char * caml_dlerror(void)
{
  static char dlerror_buffer[256];
  DWORD msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,         /* message source */
                  GetLastError(), /* error number */
                  0,            /* default language */
                  dlerror_buffer, /* destination */
                  sizeof(dlerror_buffer), /* size of destination */
                  NULL);         /* no inserts */
  if (msglen == 0)
    return "unknown error";
  else
    return dlerror_buffer;
}

/* Proper emulation of signal(), including ctrl-C and ctrl-break */

typedef void (*sighandler)(int sig);
static int ctrl_handler_installed = 0;
static volatile sighandler ctrl_handler_action = SIG_DFL;

static BOOL WINAPI ctrl_handler(DWORD event)
{
  int saved_mode;

  /* Only ctrl-C and ctrl-Break are handled */
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  /* Default behavior is to exit, which we get by not handling the event */
  if (ctrl_handler_action == SIG_DFL) return FALSE;
  /* Ignore behavior is to do nothing, which we get by claiming that we
     have handled the event */
  if (ctrl_handler_action == SIG_IGN) return TRUE;
  /* Win32 doesn't like it when we do a longjmp() at this point
     (it looks like we're running in a different thread than
     the main program!).  So, just record the signal. */
  caml_record_signal(SIGINT);
  /* We have handled the event */
  return TRUE;
}

sighandler caml_win32_signal(int sig, sighandler action)
{
  sighandler oldaction;

  if (sig != SIGINT) return signal(sig, action);
  if (! ctrl_handler_installed) {
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
    ctrl_handler_installed = 1;
  }
  oldaction = ctrl_handler_action;
  ctrl_handler_action = action;
  return oldaction;
}

/* Expansion of @responsefile and *? file patterns in the command line */

static int argc;
static char ** argv;
static int argvsize;

static void store_argument(char * arg);
static void expand_argument(char * arg);
static void expand_pattern(char * arg);
static void expand_diversion(char * filename);

static void out_of_memory(void)
{
  fprintf(stderr, "Out of memory while expanding command line\n");
  exit(2);
}

static void store_argument(char * arg)
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv = (char **) realloc(argv, argvsize * sizeof(char *));
    if (argv == NULL) out_of_memory();
  }
  argv[argc++] = arg;
}

static void expand_argument(char * arg)
{
  char * p;

  if (arg[0] == '@') {
    expand_diversion(arg + 1);
    return;
  }
  for (p = arg; *p != 0; p++) {
    if (*p == '*' || *p == '?') {
      expand_pattern(arg);
      return;
    }
  }
  store_argument(arg);
}

static void expand_pattern(char * pat)
{
  int handle;
  struct _finddata_t ffblk;
  int preflen;

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  for (preflen = strlen(pat); preflen > 0; preflen--) {
    char c = pat[preflen - 1];
    if (c == '\\' || c == '/' || c == ':') break;
  }
  do {
    char * name = malloc(preflen + strlen(ffblk.name) + 1);
    if (name == NULL) out_of_memory();
    memcpy(name, pat, preflen);
    strcpy(name + preflen, ffblk.name);
    store_argument(name);
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
}

static void expand_diversion(char * filename)
{
  struct _stat stat;
  int fd;
  char * buf, * endbuf, * p, * q, * s;
  int inquote;

  if (_stat(filename, &stat) == -1 ||
      (fd = _open(filename, O_RDONLY | O_BINARY, 0)) == -1) {
    fprintf(stderr, "Cannot open file %s\n", filename);
    exit(2);
  }
  buf = (char *) malloc(stat.st_size + 1);
  if (buf == NULL) out_of_memory();
  _read(fd, buf, stat.st_size);
  endbuf = buf + stat.st_size;
  _close(fd);
  for (p = buf; p < endbuf; /*nothing*/) {
    /* Skip leading blanks */
    while (p < endbuf && isspace(*p)) p++;
    if (p >= endbuf) break;
    s = p;
    /* Skip to end of argument, taking quotes into account */
    q = s;
    inquote = 0;
    while (p < endbuf) {
      if (! inquote) {
        if (isspace(*p)) break;
        if (*p == '"') { inquote = 1; p++; continue; }
        *q++ = *p++;
      } else {
        switch (*p) {
          case '"':
            inquote = 0; p++; continue;
          case '\\':
            if (p + 4 <= endbuf && strncmp(p, "\\\\\\\"", 4) == 0) {
              p += 4; *q++ = '\\'; *q++ = '"'; continue;
            }
            if (p + 3 <= endbuf && strncmp(p, "\\\\\"", 3) == 0) {
              p += 3; *q++ = '\\'; inquote = 0; continue;
            }
            if (p + 2 <= endbuf && p[1] == '"') {
              p += 2; *q++ = '"'; continue;
            }
            /* fallthrough */
        default:
          *q++ = *p++;
        }
      }
    }
    /* Delimit argument and expand it */
    *q++ = 0;
    expand_argument(s);
    p++;
  }
}

CAMLexport void caml_expand_command_line(int * argcp, char *** argvp)
{
  int i;
  argc = 0;
  argvsize = 16;
  argv = (char **) malloc(argvsize * sizeof(char *));
  if (argv == NULL) out_of_memory();
  for (i = 0; i < *argcp; i++) expand_argument((*argvp)[i]);
  argv[argc] = NULL;
  *argcp = argc;
  *argvp = argv;
}

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

int caml_read_directory(char * dirname, struct ext_table * contents)
{
  char * template;
  intptr_t h;
  struct _finddata_t fileinfo;
  char * p;

  template = caml_stat_alloc(strlen(dirname) + 5);
  strcpy(template, dirname);
  strcat(template, "\\*.*");
  h = _findfirst(template, &fileinfo);
  caml_stat_free(template);
  if (h == -1) return errno == ENOENT ? 0 : -1;
  do {
    if (strcmp(fileinfo.name, ".") != 0 && strcmp(fileinfo.name, "..") != 0) {
      p = caml_stat_alloc(strlen(fileinfo.name) + 1);
      strcpy(p, fileinfo.name);
      caml_ext_table_add(contents, p);
    }
  } while (_findnext(h, &fileinfo) == 0);
  _findclose(h);
  return 0;
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation and termination */

void caml_signal_thread(void * lpParam)
{
  char *endptr;
  HANDLE h;
  /* Get an hexa-code raw handle through the environment */
  h = (HANDLE) strtol(getenv("CAMLSIGPIPE"), &endptr, 16);
  while (1) {
    DWORD numread;
    BOOL ret;
    char iobuf[2];
    /* This shall always return a single character */
    ret = ReadFile(h, iobuf, 1, &numread, NULL);
    if (!ret || numread != 1) caml_sys_exit(Val_int(2));
    switch (iobuf[0]) {
    case 'C':
      caml_record_signal(SIGINT);
      break;
    case 'T':
      raise(SIGTERM);
      return;
    }
  }
}

#endif /* NATIVE_CODE */
