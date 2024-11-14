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

/* caml_search_in_system_path lives here as it's shared with ../stdlib/header.c
   */

#define CAML_INTERNALS
#include "caml/memory.h"

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
