/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The launcher for bytecode executables (if #! is not working) */

#include <stdlib.h>
#include <string.h>
#include "../config/s.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../byterun/exec.h"

char * default_runtime_path = RUNTIME_NAME;

#define MAXPATHLEN 1024

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

static char * searchpath(char * name)
{
  static char fullname[MAXPATHLEN + 1];
  char * path;
  char * p;
  char * q;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == NULL) return name;
  while(1) {
    for (p = fullname; *path != 0 && *path != ':'; p++, path++)
      if (p < fullname + MAXPATHLEN) *p = *path;
    if (p != fullname && p < fullname + MAXPATHLEN)
      *p++ = '/';
    for (q = name; *q != 0; p++, q++)
      if (p < fullname + MAXPATHLEN) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return name;
    path++;
  }
  return fullname;
}

static unsigned long read_size(char * ptr)
{
  unsigned char * p = (unsigned char *) ptr;
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

static char * read_runtime_path(int fd)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[MAXPATHLEN];
  unsigned path_size, code_size, prim_size, data_size;
  unsigned symbol_size, debug_size, size;

  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return NULL;
  path_size = read_size(buffer);
  code_size = read_size(buffer + 4);
  prim_size = read_size(buffer + 8);
  data_size = read_size(buffer + 12);
  symbol_size = read_size(buffer + 16);
  debug_size = read_size(buffer + 20);
  if (path_size > MAXPATHLEN) return NULL;
  if (path_size == 0) return default_runtime_path;
  size = path_size + code_size + prim_size +
         data_size + symbol_size + debug_size + TRAILER_SIZE;
  lseek(fd, -size, SEEK_END);
  if (read(fd, runtime_path, path_size) != path_size) return NULL;
  runtime_path[path_size - 1] = 0;
  return runtime_path;
}

static void errwrite(char * msg)
{
  write(2, msg, strlen(msg));
}

int main(int argc, char ** argv)
{
  char * truename, * runtime_path;
  int fd;

  truename = searchpath(argv[0]);
  fd = open(truename, O_RDONLY);
  if (fd == -1 || (runtime_path = read_runtime_path(fd)) == NULL) {
    errwrite(truename);
    errwrite(" not found or is not a bytecode executable file\n");
    return 2;
  }
  execv(runtime_path, argv);
  errwrite("Cannot exec ");
  errwrite(runtime_path);
  errwrite("\n");
  return 2;
}
