/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The launcher for bytecode executables (if #! is not working) */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../config/s.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../byterun/mlvalues.h"
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
  int num_sections, i;
  uint32 path_size;
  long ofs;

  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return NULL;
  num_sections = read_size(buffer);
  ofs = TRAILER_SIZE + num_sections * 8;
  lseek(fd, -ofs, SEEK_END);
  path_size = 0;
  for (i = 0; i < num_sections; i++) {
    if (read(fd, buffer, 8) < 8) return NULL;
    if (buffer[0] == 'R' && buffer[1] == 'N' &&
        buffer[2] == 'T' && buffer[3] == 'M') {
      path_size = read_size(buffer + 4);
      ofs += path_size;
    } else if (path_size > 0)
      ofs += read_size(buffer + 4);
  }
  if (path_size == 0) return default_runtime_path;
  if (path_size >= MAXPATHLEN) return NULL;
  lseek(fd, -ofs, SEEK_END);
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
