/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Win32-specific stuff */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef HAS_UI
#include <io.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include "signals.h"

/* Path searching function */

char * searchpath(char * name)
{
#define MAX_PATH_LENGTH 1024
  static char fullname[MAX_PATH_LENGTH];
  char * filepart;

  if (SearchPath(NULL,              /* use system search path */
                 name,
                 ".exe",            /* add .exe extension if needed */
                 MAX_PATH_LENGTH,   /* size of buffer */
                 fullname,
                 &filepart))
    return fullname;
  else
    return name;
}

/* Expansion of @responsefile and *? file patterns in the command line */

#ifndef HAS_UI

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

  handle = _findfirst(pat, &ffblk);
  if (handle == -1) {
    store_argument(pat); /* a la Bourne shell */
    return;
  }
  do {
    store_argument(strdup(ffblk.name));
  } while (_findnext(handle, &ffblk) != -1);
  _findclose(handle);
}

static void expand_diversion(char * filename)
{
  struct _stat stat;
  int fd;
  char * buf, * endbuf, * p, * s;

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
    /* Skip to next blank or end of buffer */
    while (p < endbuf && !isspace(*p)) p++;
    /* Delimit argument and expand it */
    *p++ = 0;
    expand_argument(s);
  }
}

void expand_command_line(int * argcp, char *** argvp)
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

#endif

/* Wrapper around "system" for Win32.  Create a diversion file if
   command line is too long. */

extern char * mktemp(char *);

int win32_system(char * cmdline)
{
#define MAX_CMD_LENGTH 256
  char cmd[MAX_CMD_LENGTH + 16];
  char template[9];
  char * tempfile;
  FILE * fd;
  int len, i, j, k, retcode;

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
    fd = fopen(tempfile, "w");
    if (fd == NULL) return -1;
    for (k = i; k < len; k++)
      fputc((isspace(cmdline[k]) ? '\n' : cmdline[k]), fd);
    fclose(fd);
    /* Add " @tempfile" to the command line */
    sprintf(cmd + j, " @%s", tempfile);
    /* Run command */
    retcode = system(cmd);
    /* Remove temp file and exit */
    unlink(tempfile);
    return retcode;
  }
}

#ifndef NATIVE_CODE

/* Set up a new thread for control-C emulation */

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
    if (!ret || numread != 1) sys_exit(Val_int(0));
    switch (iobuf[0]) {
    case 'C':
      pending_signal = SIGINT;
      something_to_do = 1;
      break;
    case 'T':
      exit(0);
      break;
    }
  }
}

#endif
