/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Expansion of @responsefile and *? file patterns in the command line */

#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>

static int argc;
static char ** argv;
static int argvsize;

static void store_argument(), expand_argument();
static void expand_pattern(), expand_diversion();

static void out_of_memory()
{
  fprintf(stderr, "Out of memory while expanding command line\n");
  exit(2);
}

static void store_argument(arg)
     char * arg;
{
  if (argc + 1 >= argvsize) {
    argvsize *= 2;
    argv = (char **) realloc(argv, argvsize * sizeof(char *));
    if (argv == NULL) out_of_memory;
  }
  argv[argc++] = arg;
}

static void expand_argument(arg)
     char * arg;
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

static void expand_pattern(pat)
     char * pat;
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

static void expand_diversion(filename)
     char * filename;
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

void expand_command_line(argcp, argvp)
     int * argcp;
     char *** argvp;
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
