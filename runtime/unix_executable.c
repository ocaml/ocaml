/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* caml_search_in_system_path and caml_executable_name live here as they're
   shared with ../stdlib/header.c */

#define CAML_INTERNALS
#include "caml/memory.h"

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

#include <unistd.h>
#include <string.h>
#include <sys/stat.h>

caml_stat_string caml_search_in_system_path(const char * name)
{
  char * fullname;
  char * path;
  struct stat st;
  size_t len = 0;

  for (char *p = (char *)name, len = 0; *p != 0; p++, len++) {
    if (*p == '/') return NULL;
  }
  if ((path = getenv("PATH")) == NULL) return NULL;
  /* len is now strlen(name) + strlen(path) + separator + terminator */
  len += strlen(path) + 2;
  if ((fullname = (char *)caml_stat_alloc(len)) == NULL) return NULL;
  while(1) {
    char * p;
    for (p = fullname; *path != 0 && *path != ':'; p++, path++)
      if (p < fullname + len) *p = *path;
    if (p != fullname && p < fullname + len)
      *p++ = '/';
    for (char *q = (char *)name; *q != 0; p++, q++)
      if (p < fullname + len) *p = *q;
    *p = 0;
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
    if (*path == 0) return NULL;
    path++;
  }
  return fullname;
}

/* Recover executable name from /proc/self/exe if possible */

char * caml_executable_name(void)
{
#if defined(__linux__)
  int namelen, retcode;
  char * name;
  struct stat st;

  /* lstat("/proc/self/exe") returns st_size == 0 so we cannot use it
     to determine the size of the buffer.  Instead, we guess and adjust. */
  namelen = 256;
  while (1) {
    name = caml_stat_alloc(namelen);
    retcode = readlink("/proc/self/exe", name, namelen);
    if (retcode == -1) { caml_stat_free(name); return NULL; }
    if (retcode < namelen) break;
    caml_stat_free(name);
    if (namelen >= 1024*1024) return NULL; /* avoid runaway and overflow */
    namelen *= 2;
  }
  /* readlink() does not zero-terminate its result.
     There is room for a final zero since retcode < namelen. */
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) == -1 || ! S_ISREG(st.st_mode)) {
    caml_stat_free(name); return NULL;
  }
  return name;

#elif defined(__APPLE__)
  unsigned int namelen;
  char * name;

  namelen = 256;
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  /* Buffer is too small, but namelen now contains the size needed */
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  return NULL;

#else
  return NULL;

#endif
}
