/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Unix-specific stuff */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "config.h"
#ifdef SUPPORT_DYNAMIC_LINKING
#include <dlfcn.h>
#endif
#include "memory.h"
#include "misc.h"
#include "osdeps.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = stat_alloc(strlen(path) + 1);
  strcpy(p, path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ':'; n++) /*nothing*/;
    ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = stat_alloc(strlen((char *)(path->contents[i])) +
                          strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    if (fullname[0] != 0) strcat(fullname, "/");
    strcat(fullname, name);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    stat_free(fullname);
  }
 not_found:
  fullname = stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}
  
#ifdef __CYGWIN32__

/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static int cygwin_file_exists(char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  close(fd);
  return 1;
}

static char * cygwin_search_exe_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = stat_alloc(strlen((char *)(path->contents[i])) +
                          strlen(name) + 6);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "/");
    strcat(fullname, name);
    if (cygwin_file_exists(fullname)) return fullname;
    strcat(fullname, ".exe");
    if (cygwin_file_exists(fullname)) return fullname;
    stat_free(fullname);
  }
 not_found:
  fullname = stat_alloc(strlen(name) + 5);
  strcpy(fullname, name);
  if (cygwin_file_exists(fullname)) return fullname;
  strcat(fullname, ".exe");
  if (cygwin_file_exists(fullname)) return fullname;
  strcpy(fullname, name);
  return fullname;
}
  
#endif

char * search_exe_in_path(char * name)
{
  struct ext_table path;
  char * tofree;
  char * res;

  ext_table_init(&path, 8);
  tofree = decompose_path(&path, getenv("PATH"));
#ifndef __CYGWIN32__
  res = search_in_path(&path, name);
#else
  res = cygwin_search_exe_in_path(&path, name);
#endif
  stat_free(tofree);
  ext_table_free(&path, 0);
  return res;
}

char * search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = stat_alloc(strlen(name) + 4);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".so");
  res = search_in_path(path, dllname);
  stat_free(dllname);
  return res;
}

#ifdef SUPPORT_DYNAMIC_LINKING

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

void * caml_dlopen(char * libname)
{
  return dlopen(libname, RTLD_NOW|RTLD_GLOBAL);
}

void caml_dlclose(void * handle)
{
  dlclose(handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return dlsym(handle, name);
}

char * caml_dlerror(void)
{
  return dlerror();
}

#else

void * caml_dlopen(char * libname)
{
  return NULL;
}

void caml_dlclose(void * handle)
{
}

void * caml_dlsym(void * handle, char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

#endif

